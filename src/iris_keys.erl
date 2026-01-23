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
%% Durability: 99.999% (same as message durability per NFR-22)
%% =============================================================================

-export([start_link/0]).

%% Key Bundle API
-export([upload_bundle/2, fetch_bundle/1, fetch_bundle/2]).
-export([get_identity_key/1, get_signed_prekey/1, pop_one_time_prekey/1]).
-export([refill_one_time_prekeys/2]).
-export([get_prekey_count/1]).

%% Admin API
-export([delete_user_keys/1, list_users/0]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(MIN_OPK_COUNT, 20).      %% Alert threshold for one-time prekeys
-define(DEFAULT_OPK_COUNT, 100). %% Default one-time prekey pool size

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
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% Initialize Mnesia table for key storage
    ok = init_table(),
    {ok, #state{}}.

handle_call({upload_bundle, UserId, Bundle}, _From, State) ->
    Result = do_upload_bundle(UserId, Bundle),
    {reply, Result, State};

handle_call({fetch_bundle, UserId, ConsumeOPK}, _From, State) ->
    {Result, NewState} = do_fetch_bundle(UserId, ConsumeOPK, State),
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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal: Mnesia Table
%% =============================================================================

init_table() ->
    case mnesia:create_table(e2ee_key_bundle, [
        {attributes, record_info(fields, key_bundle)},
        {record_name, key_bundle},
        {disc_copies, [node()]},  %% Durable storage
        {type, set}
    ]) of
        {atomic, ok} -> 
            logger:info("Created e2ee_key_bundle table"),
            ok;
        {aborted, {already_exists, e2ee_key_bundle}} -> 
            ok;
        {aborted, Reason} -> 
            logger:error("Failed to create e2ee_key_bundle table: ~p", [Reason]),
            {error, Reason}
    end.

%% =============================================================================
%% Internal: Bundle Operations
%% =============================================================================

do_upload_bundle(UserId, Bundle) ->
    %% Validate bundle
    case validate_bundle(Bundle) of
        ok ->
            Now = os:system_time(millisecond),
            Record = #key_bundle{
                user_id = UserId,
                identity_key = maps:get(identity_key, Bundle),
                signed_prekey = maps:get(signed_prekey, Bundle),
                signed_prekey_signature = maps:get(signed_prekey_signature, Bundle),
                signed_prekey_timestamp = maps:get(signed_prekey_timestamp, Bundle, Now),
                one_time_prekeys = maps:get(one_time_prekeys, Bundle, []),
                created_at = Now,
                updated_at = Now
            },
            
            %% Use sync_transaction for durability (NFR-22: 99.999%)
            F = fun() -> mnesia:write(e2ee_key_bundle, Record, write) end,
            case mnesia:sync_transaction(F) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> {error, Reason}
            end;
        {error, _} = Error ->
            Error
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
                {OPK, OPKIndex, NewRecord} = case {ConsumeOPK, Record#key_bundle.one_time_prekeys} of
                    {true, [First | Rest]} ->
                        Index = length(Record#key_bundle.one_time_prekeys) - length(Rest),
                        UpdatedRecord = Record#key_bundle{
                            one_time_prekeys = Rest,
                            updated_at = os:system_time(millisecond)
                        },
                        mnesia:write(e2ee_key_bundle, UpdatedRecord, write),
                        {First, Index, UpdatedRecord};
                    {true, []} ->
                        {undefined, undefined, Record};
                    {false, _} ->
                        {undefined, undefined, Record}
                end,
                
                Bundle = #{
                    identity_key => Record#key_bundle.identity_key,
                    signed_prekey => Record#key_bundle.signed_prekey,
                    signed_prekey_signature => Record#key_bundle.signed_prekey_signature,
                    signed_prekey_timestamp => Record#key_bundle.signed_prekey_timestamp,
                    one_time_prekey => OPK,
                    one_time_prekey_index => OPKIndex,
                    prekeys_remaining => length(NewRecord#key_bundle.one_time_prekeys)
                },
                {ok, Bundle};
            [] ->
                {error, not_found}
        end
    end,
    
    case mnesia:sync_transaction(F) of
        {atomic, {ok, Bundle}} ->
            %% Check if OPK count is low and maybe alert
            Remaining = maps:get(prekeys_remaining, Bundle, 0),
            NewState = maybe_alert_low_prekeys(UserId, Remaining, State),
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
                        {ok, length(Combined)};
                    [] ->
                        {error, not_found}
                end
            end,
            case mnesia:sync_transaction(F) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, Reason}
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
%% Internal: Low Prekey Alert
%% =============================================================================

maybe_alert_low_prekeys(UserId, Remaining, State) when Remaining < ?MIN_OPK_COUNT ->
    Alerts = State#state.low_opk_alerts,
    Now = os:system_time(second),
    
    %% Only alert once per hour per user
    case maps:get(UserId, Alerts, 0) of
        LastAlert when Now - LastAlert > 3600 ->
            logger:warning("User ~s has low one-time prekeys: ~p remaining (threshold: ~p)",
                          [UserId, Remaining, ?MIN_OPK_COUNT]),
            State#state{low_opk_alerts = maps:put(UserId, Now, Alerts)};
        _ ->
            State
    end;
maybe_alert_low_prekeys(_UserId, _Remaining, State) ->
    State.
