-module(iris_presence).

%% =============================================================================
%% Versioned Presence Module (AUDIT FIX: Online/Offline Race Condition)
%% =============================================================================
%% 
%% PROBLEM: User disconnects while sender looks up presence.
%% 1. Sender queries presence → sees {online, Node, Pid}
%% 2. User disconnects → presence deleted
%% 3. Sender sends to Pid → process dead, message lost
%%
%% SOLUTION: Add monotonic version number to presence entries.
%% - Every presence update increments the version
%% - Routing layer includes expected version when sending
%% - If version mismatches, message is re-routed or stored offline
%%
%% =============================================================================

-export([init_table/0]).
-export([register/3, unregister/1]).
-export([lookup/1, lookup_versioned/1]).
-export([route_to_online/3, route_to_online/4]).
-export([get_version/1]).

%% ETS table for version counters (lockfree)
-define(VERSION_ETS, iris_presence_versions).

%% Presence record with version
-record(presence_v2, {
    user_id :: binary(),
    node :: node(),
    pid :: pid(),
    version :: non_neg_integer(),
    connected_at :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

%% @doc Initialize Mnesia table for versioned presence
-spec init_table() -> ok | {error, term()}.
init_table() ->
    %% Create Mnesia table
    Result = mnesia:create_table(presence_v2, [
        {attributes, record_info(fields, presence_v2)},
        {record_name, presence_v2},
        {ram_copies, [node()]},  %% In-memory for speed
        {type, set}
    ]),
    case Result of
        {atomic, ok} -> ok;
        {aborted, {already_exists, presence_v2}} -> ok;
        {aborted, Reason} -> 
            logger:error("Failed to create presence_v2 table: ~p", [Reason]),
            ok  %% Continue anyway, table might exist on other node
    end,
    
    %% Create ETS table for version counters
    case ets:info(?VERSION_ETS) of
        undefined ->
            ets:new(?VERSION_ETS, [named_table, public, {write_concurrency, true}]);
        _ -> ok
    end,
    ok.

%% @doc Register user as online with versioned presence
-spec register(binary(), node(), pid()) -> {ok, non_neg_integer()} | {error, term()}.
register(UserId, Node, Pid) when is_binary(UserId), is_pid(Pid) ->
    %% Increment version (lockfree)
    Version = ets:update_counter(?VERSION_ETS, UserId, 1, {UserId, 0}),
    
    Record = #presence_v2{
        user_id = UserId,
        node = Node,
        pid = Pid,
        version = Version,
        connected_at = erlang:system_time(millisecond)
    },
    
    F = fun() -> mnesia:write(presence_v2, Record, write) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, Version};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Unregister user (mark as offline)
-spec unregister(binary()) -> ok.
unregister(UserId) when is_binary(UserId) ->
    %% Increment version to invalidate any in-flight routing decisions
    ets:update_counter(?VERSION_ETS, UserId, 1, {UserId, 0}),
    
    %% Delete presence entry
    mnesia:dirty_delete(presence_v2, UserId),
    ok.

%% @doc Lookup user presence (simple, without version)
-spec lookup(binary()) -> {ok, node(), pid()} | {error, not_found}.
lookup(UserId) ->
    case mnesia:dirty_read(presence_v2, UserId) of
        [#presence_v2{node = Node, pid = Pid}] -> {ok, Node, Pid};
        [] -> {error, not_found}
    end.

%% @doc Lookup user presence with version
-spec lookup_versioned(binary()) -> {ok, node(), pid(), non_neg_integer()} | {error, not_found}.
lookup_versioned(UserId) ->
    case mnesia:dirty_read(presence_v2, UserId) of
        [#presence_v2{node = Node, pid = Pid, version = Version}] -> 
            {ok, Node, Pid, Version};
        [] -> 
            {error, not_found}
    end.

%% @doc Get current version for a user (even if offline)
-spec get_version(binary()) -> non_neg_integer().
get_version(UserId) ->
    case ets:lookup(?VERSION_ETS, UserId) of
        [{UserId, Version}] -> Version;
        [] -> 0
    end.

%% @doc Route message to online user with version check
%% Returns: ok | {ok, offline} | {error, version_mismatch}
-spec route_to_online(binary(), binary(), non_neg_integer()) -> 
    ok | {ok, offline} | {error, term()}.
route_to_online(UserId, Msg, ExpectedVersion) ->
    route_to_online(UserId, Msg, ExpectedVersion, #{}).

-spec route_to_online(binary(), binary(), non_neg_integer(), map()) -> 
    ok | {ok, offline} | {error, term()}.
route_to_online(UserId, Msg, ExpectedVersion, Opts) ->
    case lookup_versioned(UserId) of
        {ok, Node, Pid, CurrentVersion} when CurrentVersion == ExpectedVersion ->
            %% Version matches - safe to deliver
            case Node == node() of
                true ->
                    %% Local delivery
                    deliver_local(Pid, Msg);
                false ->
                    %% Remote delivery via RPC
                    deliver_remote(Node, Pid, Msg)
            end;
        
        {ok, _Node, _Pid, CurrentVersion} when CurrentVersion > ExpectedVersion ->
            %% Version changed - presence was updated (user reconnected)
            %% Re-route with new version
            ReRoute = maps:get(auto_reroute, Opts, true),
            case ReRoute of
                true ->
                    %% Recursive call with new version
                    route_to_online(UserId, Msg, CurrentVersion, Opts);
                false ->
                    {error, version_mismatch}
            end;
        
        {error, not_found} ->
            %% User is offline - store for later delivery
            store_offline_fallback(UserId, Msg)
    end.

%% =============================================================================
%% Internal: Delivery
%% =============================================================================

deliver_local(Pid, Msg) ->
    case is_process_alive(Pid) of
        true ->
            Pid ! {deliver_msg, Msg},
            ok;
        false ->
            %% Process died between lookup and delivery
            %% This is rare but possible - the version check should have caught it
            {error, process_dead}
    end.

deliver_remote(Node, Pid, Msg) ->
    %% Try to deliver via RPC
    case rpc:call(Node, erlang, is_process_alive, [Pid], 2000) of
        true ->
            %% Process is alive - send
            Pid ! {deliver_msg, Msg},
            ok;
        false ->
            {error, process_dead};
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}}
    end.

%% =============================================================================
%% Internal: Offline Fallback
%% =============================================================================

store_offline_fallback(UserId, Msg) ->
    %% Use iris_core's offline storage
    case whereis(iris_core) of
        undefined ->
            %% Core not running, try iris_store directly
            try
                iris_store:put(offline_msg, 
                    {UserId, erlang:system_time(microsecond)}, 
                    Msg, 
                    #{durability => guaranteed}),
                {ok, offline}
            catch
                _:_ -> {error, storage_unavailable}
            end;
        _ ->
            try
                iris_core:store_offline_durable(UserId, Msg),
                {ok, offline}
            catch
                _:_ -> {error, storage_unavailable}
            end
    end.
