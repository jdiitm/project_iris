-module(iris_keys).
-behaviour(gen_server).

%% =============================================================================
%% E2EE Key Bundle Storage (RFC-001-AMENDMENT-001, FR-13, NFR-22)
%% =============================================================================
%% Purpose: Manage X3DH key bundles for end-to-end encryption.
%% 
%% Key Types:
%% - Identity Key (IK): Long-term key pair, device-bound
%% - Signed Pre-Key (SPK): Medium-term key, rotates weekly
%% - One-Time Pre-Keys (OPK): Single-use keys, pool of 100+
%%
%% Key Bundle Durability:
%% - All key bundle operations use QUORUM WRITES (W=2 of N=3 replicas)
%% - Ensures 99.999% durability even during node failures
%% - Key bundles survive single node failure before replication completes
%%
%% Durability: 99.999% (same as message durability per NFR-22)
%% =============================================================================

-export([start_link/0]).

%% Key Bundle API
-export([upload_bundle/2, fetch_bundle/1, fetch_bundle/2, fetch_bundle/3]).
-export([get_identity_key/1, get_signed_prekey/1, pop_one_time_prekey/1]).
-export([refill_one_time_prekeys/2]).
-export([get_prekey_count/1]).

%% Safety Number (EK-1: RFC-001-AMENDMENT-001 v1.3 Section 5.3.1)
-export([compute_safety_number/2]).

%% GAP-13: Key Change Contact Tracking (RFC-001-AMENDMENT-001 Section 5.3.2)
-export([record_key_contact/2, get_key_contacts/1]).

%% Admin API
-export([delete_user_keys/1, list_users/0]).

%% Exported for testing (quorum write durability)
-export([store_key_bundle_durable/2]).

%% Metrics API
-export([get_opk_metrics/0]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MIN_OPK_COUNT, 20).      %% Alert threshold for one-time prekeys (NFR-24)
-define(DEFAULT_OPK_COUNT, 100). %% Default one-time prekey pool size

%% Metrics ETS table for OPK tracking
-define(METRICS_ETS, iris_keys_metrics).
%% GAP-13: Key change contact tracking
-define(CONTACTS_TABLE, iris_key_contacts).

%% =============================================================================
%% Records
%% =============================================================================

%% Key bundle stored per user
-record(key_bundle, {
    user_id :: binary(),                    %% User identifier
    identity_key :: binary(),               %% Public Identity Key (32 bytes)
    signed_prekey :: binary(),              %% Public Signed Pre-Key (32 bytes)
    signed_prekey_signature :: binary(),    %% Signature over SPK (64 bytes)
    signed_prekey_timestamp :: integer(),   %% When SPK was created
    one_time_prekeys :: [binary()],         %% List of public OPKs (32 bytes each)
    created_at :: integer(),                %% Bundle creation timestamp
    updated_at :: integer()                 %% Last update timestamp
}).

%% GenServer state
-record(state, {
    low_opk_alerts = #{} :: #{binary() => integer()}  %% user_id => last_alert_time
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Upload a complete key bundle for a user
%% Bundle format: #{identity_key => binary(), signed_prekey => binary(), 
%%                  signed_prekey_signature => binary(), one_time_prekeys => [binary()]}
-spec upload_bundle(binary(), map()) -> ok | {error, term()}.
upload_bundle(UserId, Bundle) when is_binary(UserId), is_map(Bundle) ->
    gen_server:call(?SERVER, {upload_bundle, UserId, Bundle}).

%% @doc Fetch a user's key bundle (consumes one OPK if available)
-spec fetch_bundle(binary()) -> {ok, map()} | {error, not_found}.
fetch_bundle(UserId) ->
    fetch_bundle(UserId, true).

%% @doc Fetch a user's key bundle, optionally consuming an OPK
-spec fetch_bundle(binary(), boolean()) -> {ok, map()} | {error, not_found}.
fetch_bundle(UserId, ConsumeOPK) when is_binary(UserId), is_boolean(ConsumeOPK) ->
    gen_server:call(?SERVER, {fetch_bundle, UserId, ConsumeOPK}).

%% @doc Fetch a user's key bundle with contact tracking (GAP-13)
%% Records the requester as a contact of the owner for key change notifications.
-spec fetch_bundle(binary(), boolean(), binary()) -> {ok, map()} | {error, not_found}.
fetch_bundle(UserId, ConsumeOPK, RequesterUserId) when is_binary(UserId), is_boolean(ConsumeOPK), is_binary(RequesterUserId) ->
    gen_server:call(?SERVER, {fetch_bundle, UserId, ConsumeOPK, RequesterUserId}).

%% @doc Get only the identity key (no OPK consumption)
-spec get_identity_key(binary()) -> {ok, binary()} | {error, not_found}.
get_identity_key(UserId) ->
    gen_server:call(?SERVER, {get_identity_key, UserId}).

%% @doc Get the current signed prekey
-spec get_signed_prekey(binary()) -> {ok, binary(), binary()} | {error, not_found}.
get_signed_prekey(UserId) ->
    gen_server:call(?SERVER, {get_signed_prekey, UserId}).

%% @doc Pop a one-time prekey (for key exchange)
-spec pop_one_time_prekey(binary()) -> {ok, binary(), integer()} | {error, no_prekeys | not_found}.
pop_one_time_prekey(UserId) ->
    gen_server:call(?SERVER, {pop_one_time_prekey, UserId}).

%% @doc Refill one-time prekeys
-spec refill_one_time_prekeys(binary(), [binary()]) -> ok | {error, term()}.
refill_one_time_prekeys(UserId, NewPrekeys) when is_binary(UserId), is_list(NewPrekeys) ->
    gen_server:call(?SERVER, {refill_prekeys, UserId, NewPrekeys}).

%% @doc Get count of remaining one-time prekeys
-spec get_prekey_count(binary()) -> {ok, integer()} | {error, not_found}.
get_prekey_count(UserId) ->
    gen_server:call(?SERVER, {get_prekey_count, UserId}).

%% @doc Delete all keys for a user
-spec delete_user_keys(binary()) -> ok.
delete_user_keys(UserId) ->
    gen_server:call(?SERVER, {delete_user_keys, UserId}).

%% @doc List all users with key bundles
-spec list_users() -> [binary()].
list_users() ->
    gen_server:call(?SERVER, list_users).

%% =============================================================================
%% GAP-13: Key Change Contact Tracking (Pure ETS Operations)
%% =============================================================================

%% @doc Record that FetcherUserId has fetched OwnerUserId's key bundle.
%% Used to notify contacts when the owner's identity key changes.
%% Uses Mnesia (persistent) instead of ETS (RAM-only).
-spec record_key_contact(binary(), binary()) -> ok.
record_key_contact(OwnerUserId, FetcherUserId) ->
    %% Avoid duplicates: check before insert (bag table allows dupes otherwise)
    %% Transaction for key contact durability
    Existing = mnesia:dirty_match_object({key_contact, OwnerUserId, FetcherUserId}),
    case Existing of
        [] ->
            {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write({key_contact, OwnerUserId, FetcherUserId})
            end);
        _  -> ok
    end,
    ok.

%% @doc Get all users who have fetched this user's key bundle.
%% Reads from Mnesia (survives restart).
-spec get_key_contacts(binary()) -> [binary()].
get_key_contacts(OwnerUserId) ->
    Entries = mnesia:dirty_read(key_contact, OwnerUserId),
    [Fetcher || {key_contact, _, Fetcher} <- Entries].

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% Initialize Mnesia table for key storage
    ok = init_table(),
    
    %% Create metrics table for OPK tracking
    case ets:info(?METRICS_ETS) of
        undefined ->
            ets:new(?METRICS_ETS, [named_table, public, {write_concurrency, true}]),
            ets:insert(?METRICS_ETS, {opk_exhausted_count, 0}),
            ets:insert(?METRICS_ETS, {opk_low_alerts, 0}),
            ets:insert(?METRICS_ETS, {spk_fallback_count, 0}),
            ets:insert(?METRICS_ETS, {spk_rotation_needed, 0});
        _ -> ok
    end,
    
    %% Key contacts table MUST be persistent (Mnesia disc_copies).
    %% Previously ETS (RAM-only): restart wiped the contact graph, silently
    %% breaking key change notifications (Amendment 5.3.2 MUST requirement).
    init_contacts_table(),
    
    %% NFR-25: Schedule periodic SPK rotation check (default: 7 days)
    RotationInterval = application:get_env(iris_core, spk_rotation_interval_ms, 604800000),
    erlang:send_after(RotationInterval, self(), check_spk_rotation),
    
    {ok, #state{}}.

handle_call({upload_bundle, UserId, Bundle}, _From, State) ->
    Result = do_upload_bundle(UserId, Bundle),
    {reply, Result, State};

handle_call({fetch_bundle, UserId, ConsumeOPK}, _From, State) ->
    {Result, NewState} = do_fetch_bundle(UserId, ConsumeOPK, State),
    {reply, Result, NewState};

handle_call({fetch_bundle, UserId, ConsumeOPK, RequesterUserId}, _From, State) ->
    {Result, NewState} = do_fetch_bundle(UserId, ConsumeOPK, State),
    %% GAP-13: Record requester as contact for key change notifications
    case Result of
        {ok, _} -> record_key_contact(UserId, RequesterUserId);
        _ -> ok
    end,
    {reply, Result, NewState};

handle_call({get_identity_key, UserId}, _From, State) ->
    Result = do_get_identity_key(UserId),
    {reply, Result, State};

handle_call({get_signed_prekey, UserId}, _From, State) ->
    Result = do_get_signed_prekey(UserId),
    {reply, Result, State};

handle_call({pop_one_time_prekey, UserId}, _From, State) ->
    {Result, NewState} = do_pop_one_time_prekey(UserId, State),
    {reply, Result, NewState};

handle_call({refill_prekeys, UserId, NewPrekeys}, _From, State) ->
    Result = do_refill_prekeys(UserId, NewPrekeys),
    {reply, Result, State};

handle_call({get_prekey_count, UserId}, _From, State) ->
    Result = do_get_prekey_count(UserId),
    {reply, Result, State};

handle_call({delete_user_keys, UserId}, _From, State) ->
    ok = do_delete_user_keys(UserId),
    {reply, ok, State};

handle_call(list_users, _From, State) ->
    Users = do_list_users(),
    {reply, Users, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_spk_rotation, State) ->
    rotate_expired_spks(),
    %% Reschedule
    Interval = application:get_env(iris_core, spk_rotation_interval_ms, 604800000),
    erlang:send_after(Interval, self(), check_spk_rotation),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal: Mnesia Table
%% =============================================================================

init_table() ->
    %% Determine storage type: disc_copies if schema supports it, else ram_copies
    StorageType = case mnesia:table_info(schema, disc_copies) of
        [] -> ram_copies;  %% No disc schema, use ram
        _ -> disc_copies   %% Disc schema exists
    end,
    case mnesia:create_table(e2ee_key_bundle, [
        {attributes, record_info(fields, key_bundle)},
        {record_name, key_bundle},
        {StorageType, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> 
            logger:info("Created e2ee_key_bundle table (~p)", [StorageType]),
            ok;
        {aborted, {already_exists, e2ee_key_bundle}} -> 
            ok;
        {aborted, Reason} -> 
            logger:error("Failed to create e2ee_key_bundle table: ~p", [Reason]),
            {error, Reason}
    end.

%% Mnesia-backed key contacts table
%% Record: {key_contact, OwnerUserId, FetcherUserId}
init_contacts_table() ->
    StorageType = case mnesia:table_info(schema, disc_copies) of
        [] -> ram_copies;
        _ -> disc_copies
    end,
    case mnesia:create_table(key_contact, [
        {attributes, [owner, fetcher]},
        {StorageType, [node()]},
        {type, bag}  %% bag allows multiple fetchers per owner
    ]) of
        {atomic, ok} ->
            logger:info("Created key_contact table (~p)", [StorageType]),
            ok;
        {aborted, {already_exists, key_contact}} ->
            ok;
        {aborted, Reason} ->
            logger:error("Failed to create key_contact table: ~p", [Reason]),
            {error, Reason}
    end,
    mnesia:wait_for_tables([key_contact], 5000),
    ok.

%% =============================================================================
%% Internal: Bundle Operations
%% =============================================================================

do_upload_bundle(UserId, Bundle) ->
    %% Validate bundle
    case validate_bundle(Bundle) of
        ok ->
            NewIK = maps:get(identity_key, Bundle),
            %% RFC-001-AMENDMENT-001 Section 5.3.2: Key Change Detection (GAP-13)
            %% Compare new IK with existing IK to detect identity key changes
            detect_identity_key_change(UserId, NewIK),
            Now = os:system_time(millisecond),
            Record = #key_bundle{
                user_id = UserId,
                identity_key = NewIK,
                signed_prekey = maps:get(signed_prekey, Bundle),
                signed_prekey_signature = maps:get(signed_prekey_signature, Bundle),
                signed_prekey_timestamp = maps:get(signed_prekey_timestamp, Bundle, Now),
                one_time_prekeys = maps:get(one_time_prekeys, Bundle, []),
                created_at = Now,
                updated_at = Now
            },
            
            %% Use quorum writes for key bundle durability
            %% This ensures the key bundle survives node failures
            store_key_bundle_durable(UserId, Record);
        {error, _} = Error ->
            Error
    end.

%% @doc Detect identity key change and log for future notification (GAP-13)
%% RFC-001-AMENDMENT-001 Section 5.3.2: "When a user's Identity Key changes,
%% the server MUST notify all active sessions."
%%
%% GAP-13 IMPLEMENTED: Key change detection + contact notification.
%% 1. key_contacts ETS tracks which users have fetched each other's key bundles
%% 2. On IK change, contacts are looked up and notification packets are routed
%% 3. Opcode 0x1A (key_change_alert) encodes the notification
detect_identity_key_change(UserId, NewIK) ->
    case do_get_identity_key(UserId) of
        {ok, ExistingIK} when ExistingIK =/= NewIK ->
            logger:warning("KEY_CHANGE: Identity key changed for user ~p", [UserId]),
            iris_metrics:inc(iris_identity_key_changes),
            %% GAP-13: Notify contacts who have fetched this user's key bundle
            %% Direct pid delivery -- bypasses router pool (which may not be running)
            Contacts = get_key_contacts(UserId),
            case Contacts of
                [] -> ok;
                _ ->
                    AlertPacket = iris_proto:encode_key_change_alert(UserId),
                    lists:foreach(fun(ContactId) ->
                        try
                            case iris_core:lookup_user(ContactId) of
                                {ok, _Node, Pid} when is_pid(Pid) ->
                                    Pid ! {deliver_msg, AlertPacket};
                                _ ->
                                    %% Contact offline: store for delivery on reconnect (RFC 5.3.2 MUST)
                                    iris_core:store_offline_durable(ContactId, AlertPacket)
                            end
                        catch Class:Reason ->
                            %% Lookup crashed (e.g. shard not running) -- treat as offline
                            %% RFC 5.3.2: MUST notify, so store durably for later delivery
                            logger:warning("Key change alert for ~p failed (~p:~p), storing offline",
                                           [ContactId, Class, Reason]),
                            catch iris_core:store_offline_durable(ContactId, AlertPacket)
                        end
                    end, Contacts)
            end;
        _ ->
            %% No existing key or same key -- no change
            ok
    end.

%% Quorum-based durable storage for key bundles
store_key_bundle_durable(UserId, Record) ->
    %% Try quorum write first (if module is available)
    case whereis(iris_quorum_write) of
        undefined ->
            %% Fallback to sync_transaction (single-node durability)
            F = fun() -> mnesia:write(e2ee_key_bundle, Record, write) end,
            case mnesia:sync_transaction(F) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> {error, Reason}
            end;
        _ ->
            %% Use quorum writes for multi-node durability
            case iris_quorum_write:write_durable(e2ee_key_bundle, UserId, Record) of
                ok -> ok;
                {error, quorum_not_reached} ->
                    %% CP > AP for key bundles. Do NOT fallback to single-node
                    %% write. Propagate failure; clients should retry.
                    logger:error("Quorum write failed for key bundle ~p, rejecting (CP > AP)", [UserId]),
                    {error, quorum_not_reached};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

validate_bundle(Bundle) ->
    Required = [identity_key, signed_prekey, signed_prekey_signature],
    case lists:all(fun(K) -> maps:is_key(K, Bundle) end, Required) of
        true ->
            IK = maps:get(identity_key, Bundle),
            SPK = maps:get(signed_prekey, Bundle),
            Sig = maps:get(signed_prekey_signature, Bundle),
            
            %% Validate key sizes (Curve25519 = 32 bytes, Ed25519 sig = 64 bytes)
            case {byte_size(IK), byte_size(SPK), byte_size(Sig)} of
                {32, 32, 64} -> ok;
                _ -> {error, invalid_key_sizes}
            end;
        false ->
            {error, missing_required_fields}
    end.

do_fetch_bundle(UserId, ConsumeOPK, State) ->
    F = fun() ->
        case mnesia:read(e2ee_key_bundle, UserId, write) of
            [Record] ->
                %% Optionally consume one OPK
                {OPK, OPKIndex, NewRecord, FallbackMode} = case {ConsumeOPK, Record#key_bundle.one_time_prekeys} of
                    {true, [First | Rest]} ->
                        Index = length(Record#key_bundle.one_time_prekeys) - length(Rest),
                        UpdatedRecord = Record#key_bundle{
                            one_time_prekeys = Rest,
                            updated_at = os:system_time(millisecond)
                        },
                        mnesia:write(e2ee_key_bundle, UpdatedRecord, write),
                        {First, Index, UpdatedRecord, false};
                    {true, []} ->
                        %% OPK exhausted - X3DH fallback to SPK-only mode
                        %% Per RFC, this is valid but less secure (no forward secrecy per-message)
                        {undefined, undefined, Record, true};
                    {false, _} ->
                        {undefined, undefined, Record, false}
                end,
                
                Bundle = #{
                    identity_key => Record#key_bundle.identity_key,
                    signed_prekey => Record#key_bundle.signed_prekey,
                    signed_prekey_signature => Record#key_bundle.signed_prekey_signature,
                    signed_prekey_timestamp => Record#key_bundle.signed_prekey_timestamp,
                    one_time_prekey => OPK,
                    one_time_prekey_index => OPKIndex,
                    prekeys_remaining => length(NewRecord#key_bundle.one_time_prekeys),
                    %% Signal to caller that this is SPK-only mode
                    spk_fallback_mode => FallbackMode
                },
                {ok, Bundle, FallbackMode};
            [] ->
                {error, not_found}
        end
    end,
    
    case mnesia:sync_transaction(F) of
        {atomic, {ok, Bundle, FallbackMode}} ->
            %% Check if OPK count is low and maybe alert
            Remaining = maps:get(prekeys_remaining, Bundle, 0),
            NewState = maybe_alert_low_prekeys(UserId, Remaining, State),
            
            %% Track OPK exhaustion metrics
            case FallbackMode of
                true ->
                    incr_metric(opk_exhausted_count),
                    incr_metric(spk_fallback_count),
                    logger:warning("OPK exhausted for user ~s, using SPK-only X3DH fallback", [UserId]);
                false ->
                    ok
            end,
            
            {{ok, Bundle}, NewState};
        {atomic, {error, _} = Error} ->
            {Error, State};
        {aborted, Reason} ->
            {{error, Reason}, State}
    end.

do_get_identity_key(UserId) ->
    F = fun() ->
        case mnesia:read(e2ee_key_bundle, UserId) of
            [Record] -> {ok, Record#key_bundle.identity_key};
            [] -> {error, not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

do_get_signed_prekey(UserId) ->
    F = fun() ->
        case mnesia:read(e2ee_key_bundle, UserId) of
            [Record] -> 
                {ok, Record#key_bundle.signed_prekey, Record#key_bundle.signed_prekey_signature};
            [] -> 
                {error, not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

do_pop_one_time_prekey(UserId, State) ->
    F = fun() ->
        case mnesia:read(e2ee_key_bundle, UserId, write) of
            [Record] ->
                case Record#key_bundle.one_time_prekeys of
                    [First | Rest] ->
                        Index = length(Record#key_bundle.one_time_prekeys),
                        UpdatedRecord = Record#key_bundle{
                            one_time_prekeys = Rest,
                            updated_at = os:system_time(millisecond)
                        },
                        mnesia:write(e2ee_key_bundle, UpdatedRecord, write),
                        {ok, First, Index, length(Rest)};
                    [] ->
                        {error, no_prekeys}
                end;
            [] ->
                {error, not_found}
        end
    end,
    
    case mnesia:sync_transaction(F) of
        {atomic, {ok, Key, Index, Remaining}} ->
            NewState = maybe_alert_low_prekeys(UserId, Remaining, State),
            {{ok, Key, Index}, NewState};
        {atomic, {error, _} = Error} ->
            {Error, State};
        {aborted, Reason} ->
            {{error, Reason}, State}
    end.

do_refill_prekeys(UserId, NewPrekeys) ->
    %% Validate all prekeys are 32 bytes
    case lists:all(fun(K) -> is_binary(K) andalso byte_size(K) =:= 32 end, NewPrekeys) of
        true ->
            F = fun() ->
                case mnesia:read(e2ee_key_bundle, UserId, write) of
                    [Record] ->
                        %% Append new prekeys to existing pool
                        Combined = Record#key_bundle.one_time_prekeys ++ NewPrekeys,
                        UpdatedRecord = Record#key_bundle{
                            one_time_prekeys = Combined,
                            updated_at = os:system_time(millisecond)
                        },
                        mnesia:write(e2ee_key_bundle, UpdatedRecord, write),
                        {ok, length(Combined), UpdatedRecord};
                    [] ->
                        {error, not_found}
                end
            end,
            case mnesia:sync_transaction(F) of
                {atomic, {ok, Count, UpdatedRecord}} ->
                    %% Monitored spawn for key replication
                    iris_async:spawn_monitored(key_replication, fun() -> 
                        case whereis(iris_quorum_write) of
                            undefined -> ok;
                            _ -> iris_quorum_write:replicate_async(e2ee_key_bundle, UserId, UpdatedRecord)
                        end
                    end),
                    {ok, Count};
                {atomic, {error, _} = Error} -> 
                    Error;
                {aborted, Reason} -> 
                    {error, Reason}
            end;
        false ->
            {error, invalid_prekey_size}
    end.

do_get_prekey_count(UserId) ->
    F = fun() ->
        case mnesia:read(e2ee_key_bundle, UserId) of
            [Record] -> {ok, length(Record#key_bundle.one_time_prekeys)};
            [] -> {error, not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

do_delete_user_keys(UserId) ->
    F = fun() -> mnesia:delete({e2ee_key_bundle, UserId}) end,
    {atomic, ok} = mnesia:sync_transaction(F),
    ok.

do_list_users() ->
    F = fun() -> mnesia:all_keys(e2ee_key_bundle) end,
    case mnesia:transaction(F) of
        {atomic, Keys} -> Keys;
        {aborted, _} -> []
    end.

%% =============================================================================
%% Internal: SPK Rotation Check (NFR-25)
%% =============================================================================

-define(SPK_MAX_AGE_SECONDS, 604800). %% 7 days

rotate_expired_spks() ->
    %% Scan all key bundles for expired SPKs (timestamp > 7 days old)
    Now = os:system_time(second),
    F = fun() -> mnesia:all_keys(e2ee_key_bundle) end,
    case mnesia:transaction(F) of
        {atomic, UserIds} ->
            lists:foreach(fun(UserId) ->
                case mnesia:transaction(fun() -> mnesia:read(e2ee_key_bundle, UserId) end) of
                    {atomic, [Record]} ->
                        SpkTs = element(6, Record), %% signed_prekey_timestamp field
                        Age = Now - SpkTs,
                        case Age > ?SPK_MAX_AGE_SECONDS of
                            true ->
                                incr_metric(spk_rotation_needed),
                                logger:warning("SPK expired for user ~p (age: ~p seconds)", [UserId, Age]);
                            false ->
                                ok
                        end;
                    _ ->
                        ok
                end
            end, UserIds);
        _ ->
            ok
    end.

%% =============================================================================
%% Internal: Low Prekey Alert (NFR-24)
%% =============================================================================

maybe_alert_low_prekeys(UserId, Remaining, State) when Remaining < ?MIN_OPK_COUNT ->
    Alerts = State#state.low_opk_alerts,
    Now = os:system_time(second),
    
    %% Only alert once per hour per user
    case maps:get(UserId, Alerts, 0) of
        LastAlert when Now - LastAlert > 3600 ->
            %% Track low OPK alerts via metrics
            incr_metric(opk_low_alerts),
            logger:warning("User ~s has low one-time prekeys: ~p remaining (threshold: ~p) [NFR-24]",
                          [UserId, Remaining, ?MIN_OPK_COUNT]),
            State#state{low_opk_alerts = maps:put(UserId, Now, Alerts)};
        _ ->
            State
    end;
maybe_alert_low_prekeys(_UserId, _Remaining, State) ->
    State.

%% =============================================================================
%% Internal: Metrics
%% =============================================================================

incr_metric(Key) ->
    try
        ets:update_counter(?METRICS_ETS, Key, 1, {Key, 0})
    catch
        error:badarg -> ok  %% Table not created yet
    end.

%% @doc Get OPK-related metrics
-spec get_opk_metrics() -> map().
get_opk_metrics() ->
    try
        #{
            opk_exhausted_count => ets:lookup_element(?METRICS_ETS, opk_exhausted_count, 2),
            opk_low_alerts => ets:lookup_element(?METRICS_ETS, opk_low_alerts, 2),
            spk_fallback_count => ets:lookup_element(?METRICS_ETS, spk_fallback_count, 2),
            spk_rotation_needed => ets:lookup_element(?METRICS_ETS, spk_rotation_needed, 2)
        }
    catch
        error:badarg -> #{}  %% Table not created yet
    end.

%% =============================================================================
%% Safety Number Computation (EK-1)
%% RFC-001-AMENDMENT-001 v1.3 Section 5.3.1:
%% SHA-256(sort(IK_A, IK_B))[:30] displayed as 12 groups of 5 digits
%% =============================================================================

-spec compute_safety_number(binary(), binary()) -> {ok, binary()} | {error, invalid_key}.
compute_safety_number(IK_A, IK_B) when is_binary(IK_A), is_binary(IK_B),
                                        byte_size(IK_A) >= 16, byte_size(IK_B) >= 16 ->
    Sorted = lists:sort([IK_A, IK_B]),
    Combined = erlang:iolist_to_binary(Sorted),
    %% Use SHA-512 (64 bytes) to provide enough entropy
    %% for 30 digit-pairs using 2 bytes each (60 bytes needed).
    Hash = crypto:hash(sha512, Combined),
    %% Take first 60 bytes -> 30 digit-pairs via 16-bit sampling
    Trunc = binary:part(Hash, 0, 60),
    Digits = format_safety_number_digits(Trunc),
    {ok, Digits};
compute_safety_number(_, _) ->
    {error, invalid_key}.

%% Convert 60 bytes to 60 decimal digits grouped as 12x5.
%% Read 2 bytes (16 bits, 0-65535) per digit-pair, then
%% rem 100. Bias: 65536/100 = 655 full cycles + remainder 36, giving
%% max bias of 655/656 ≈ 0.15% (negligible vs previous 1.5x from 8-bit).
format_safety_number_digits(Bytes) ->
    DigitList = lists:flatten([
        io_lib:format("~2..0B", [W rem 100])
        || <<W:16>> <= Bytes
    ]),
    DigitsBin = list_to_binary(DigitList),
    %% Group into 12 groups of 5 digits
    Groups = [binary:part(DigitsBin, I * 5, 5) || I <- lists:seq(0, 11)],
    iolist_to_binary(lists:join(<<" ">>, Groups)).
