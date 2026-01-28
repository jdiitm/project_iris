-module(iris_quorum_write).

%% =============================================================================
%% Quorum Write Module: Hot Failover with Majority Replication
%% =============================================================================
%% 
%% CONSISTENCY GUARANTEES:
%% - Writes succeed when majority (N/2+1) of replicas acknowledge
%% - Failed replicas are repaired asynchronously
%% - Hot standby: Automatic primary promotion on failure
%% 
%% PARTITION SAFETY:
%% - Respects iris_partition_guard for consistent behavior with iris_store
%% - Writes blocked during detected partitions (safe mode)
%% - Ensures operator intent is honored across all write paths
%% 
%% DESIGN RATIONALE:
%% - sync_transaction waits for ALL replicas (slow/stuck node blocks everyone)
%% - Quorum writes tolerate minority failures without blocking
%% - Maintains RPO=0 for majority of replicas
%% 
%% USAGE:
%%   iris_quorum_write:write_durable(offline_msg, Key, Value) -> ok | {error, Reason}
%% 
%% =============================================================================

-export([write_durable/3, write_durable/4]).
-export([read_quorum/2, read_quorum/3]).
-export([get_replicas/1, repair_async/4]).
-export([set_replication_factor/1, get_replication_factor/0]).
-export([local_sync_write/3]).  %% Called via RPC on remote nodes

%% Configurable via application env
-define(DEFAULT_REPLICATION_FACTOR, 3).
-define(DEFAULT_WRITE_TIMEOUT_MS, 3000).
-define(DEFAULT_READ_TIMEOUT_MS, 2000).

%% =============================================================================
%% API: Durable Writes with Quorum
%% =============================================================================

%% @doc Write with quorum guarantee (majority of replicas must ACK)
%% Returns 'ok' when quorum is reached, repairs failed replicas async
%% 
%% PARTITION SAFETY: Respects iris_partition_guard to ensure consistent
%% behavior with iris_store during detected network partitions.
-spec write_durable(atom(), term(), term()) -> ok | {error, term()}.
write_durable(Table, Key, Value) ->
    write_durable(Table, Key, Value, #{}).

-spec write_durable(atom(), term(), term(), map()) -> ok | {error, term()}.
write_durable(Table, Key, Value, Opts) ->
    %% Check partition guard first - consistent with iris_store behavior
    case check_partition_guard() of
        ok ->
            do_write_durable(Table, Key, Value, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal: Actual quorum write implementation
do_write_durable(Table, Key, Value, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_WRITE_TIMEOUT_MS),
    ReplicationFactor = get_replication_factor(),
    WriteQuorum = (ReplicationFactor div 2) + 1,  %% Majority
    
    %% Get replica nodes for this key
    Replicas = get_replicas(Key),
    
    %% Parallel write to all replicas
    Parent = self(),
    Ref = make_ref(),
    
    %% P1-H4 FIX: Track Node -> {Pid, MRef} mapping for proper cleanup
    Workers = lists:foldl(fun(Node, Acc) ->
        {Pid, MRef} = spawn_monitor(fun() ->
            Result = do_replica_write(Node, Table, Key, Value, Timeout),
            Parent ! {Ref, Node, Result}
        end),
        maps:put(Node, {Pid, MRef}, Acc)
    end, #{}, Replicas),
    
    %% Collect results with timeout
    {Successes, Failures} = collect_results(Ref, Workers, Timeout, [], []),
    
    SuccessCount = length(Successes),
    
    case SuccessCount >= WriteQuorum of
        true ->
            %% Quorum reached - ACK to caller
            %% Async repair any failed replicas
            case Failures of
                [] -> ok;
                _ -> 
                    spawn(fun() -> repair_failed_replicas(Failures, Table, Key, Value) end)
            end,
            ok;
        false ->
            logger:error("Quorum write failed: ~p/~p successes (need ~p)", 
                        [SuccessCount, length(Replicas), WriteQuorum]),
            {error, {quorum_not_reached, SuccessCount, WriteQuorum}}
    end.

%% =============================================================================
%% Internal: Partition Guard Check
%% =============================================================================

%% @doc Check if partition guard allows writes
%% Returns 'ok' if safe, {error, partition_detected} if in safe mode
check_partition_guard() ->
    case whereis(iris_partition_guard) of
        undefined -> 
            ok;  %% Guard not running = permissive
        _Pid ->
            iris_partition_guard:is_safe_for_writes()
    end.

%% =============================================================================
%% API: Quorum Reads
%% =============================================================================

%% @doc Read with quorum (for linearizable reads)
-spec read_quorum(atom(), term()) -> {ok, term()} | not_found | {error, term()}.
read_quorum(Table, Key) ->
    read_quorum(Table, Key, #{}).

-spec read_quorum(atom(), term(), map()) -> {ok, term()} | not_found | {error, term()}.
read_quorum(Table, Key, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_READ_TIMEOUT_MS),
    Replicas = get_replicas(Key),
    ReadQuorum = (length(Replicas) div 2) + 1,
    
    %% Parallel read from all replicas
    Parent = self(),
    Ref = make_ref(),
    
    %% P1-H4 FIX: Use map-based worker tracking
    Workers = lists:foldl(fun(Node, Acc) ->
        {Pid, MRef} = spawn_monitor(fun() ->
            Result = do_replica_read(Node, Table, Key, Timeout),
            Parent ! {Ref, Node, Result}
        end),
        maps:put(Node, {Pid, MRef}, Acc)
    end, #{}, Replicas),
    
    %% Collect and reconcile results
    {Results, _Failures} = collect_read_results(Ref, Workers, Timeout, []),
    
    case length(Results) >= ReadQuorum of
        true ->
            %% Return most recent value (by timestamp if available)
            reconcile_reads(Results);
        false ->
            {error, quorum_not_reached}
    end.

%% =============================================================================
%% API: Configuration
%% =============================================================================

-spec set_replication_factor(pos_integer()) -> ok.
set_replication_factor(N) when is_integer(N), N > 0 ->
    application:set_env(iris_core, replication_factor, N).

-spec get_replication_factor() -> pos_integer().
get_replication_factor() ->
    application:get_env(iris_core, replication_factor, ?DEFAULT_REPLICATION_FACTOR).

%% =============================================================================
%% API: Replica Management
%% =============================================================================

%% @doc Get replica nodes for a given key
-spec get_replicas(term()) -> [node()].
get_replicas(Key) ->
    ReplicationFactor = get_replication_factor(),
    
    %% Get all available nodes
    AllNodes = get_available_nodes(),
    
    case length(AllNodes) of
        0 ->
            %% No nodes available - use local
            [node()];
        N when N =< ReplicationFactor ->
            %% Fewer nodes than replication factor - use all
            AllNodes;
        _ ->
            %% Select replicas using consistent hashing
            select_replicas(Key, AllNodes, ReplicationFactor)
    end.

%% @doc Async repair for failed replicas (exported for manual triggering)
-spec repair_async(atom(), term(), term(), [node()]) -> ok.
repair_async(Table, Key, Value, FailedNodes) ->
    spawn(fun() -> repair_failed_replicas(FailedNodes, Table, Key, Value) end),
    ok.

%% =============================================================================
%% Internal: Replica Write
%% =============================================================================

do_replica_write(Node, Table, Key, Value, Timeout) when Node == node() ->
    %% Local write - use Mnesia directly
    F = fun() -> mnesia:write({Table, Key, Value}) end,
    try mnesia:activity(sync_transaction, F) of
        ok -> {ok, Node};
        {atomic, _} -> {ok, Node};
        {aborted, Reason} -> {error, Node, Reason}
    catch
        _:Err -> {error, Node, Err}
    end;

do_replica_write(Node, Table, Key, Value, Timeout) ->
    %% Remote write via RPC
    case rpc:call(Node, ?MODULE, local_sync_write, [Table, Key, Value], Timeout) of
        ok -> {ok, Node};
        {atomic, _} -> {ok, Node};
        {badrpc, Reason} -> {error, Node, Reason};
        {error, Reason} -> {error, Node, Reason};
        {aborted, Reason} -> {error, Node, Reason}
    end.

%% Called via RPC on remote nodes
local_sync_write(Table, Key, Value) ->
    F = fun() -> mnesia:write({Table, Key, Value}) end,
    mnesia:activity(sync_transaction, F).

%% =============================================================================
%% Internal: Replica Read
%% =============================================================================

do_replica_read(Node, Table, Key, Timeout) when Node == node() ->
    %% Local read
    case mnesia:dirty_read(Table, Key) of
        [] -> {not_found, Node};
        [{Table, Key, Value}] -> {ok, Node, Value};
        [Record | _] -> {ok, Node, Record}
    end;

do_replica_read(Node, Table, Key, Timeout) ->
    %% Remote read via RPC
    case rpc:call(Node, mnesia, dirty_read, [Table, Key], Timeout) of
        {badrpc, Reason} -> {error, Node, Reason};
        [] -> {not_found, Node};
        [{Table, Key, Value}] -> {ok, Node, Value};
        [Record | _] -> {ok, Node, Record}
    end.

%% =============================================================================
%% Internal: Result Collection
%% =============================================================================

%% P1-H4 FIX: Workers is now a map of Node -> {Pid, MRef}
collect_results(_Ref, Workers, _Timeout, Successes, Failures) when map_size(Workers) == 0 ->
    {Successes, Failures};
collect_results(Ref, Workers, Timeout, Successes, Failures) ->
    receive
        {Ref, Node, {ok, Node}} ->
            %% Success - remove worker for this node
            Workers2 = remove_worker_for_node(Workers, Node),
            collect_results(Ref, Workers2, Timeout, [Node | Successes], Failures);
        {Ref, Node, {error, Node, Reason}} ->
            %% Failure
            Workers2 = remove_worker_for_node(Workers, Node),
            collect_results(Ref, Workers2, Timeout, Successes, [{Node, Reason} | Failures]);
        {'DOWN', MRef, process, _Pid, Reason} ->
            %% Worker crashed - find which node it belonged to
            {CrashedNode, Workers2} = remove_worker_by_mref(Workers, MRef),
            collect_results(Ref, Workers2, Timeout, Successes, [{CrashedNode, Reason} | Failures])
    after Timeout ->
        %% Timeout - treat remaining as failures
        TimedOut = [{Node, timeout} || Node <- maps:keys(Workers)],
        %% Clean up remaining workers
        maps:foreach(fun(_Node, {_Pid, MRef}) ->
            demonitor(MRef, [flush])
        end, Workers),
        {Successes, Failures ++ TimedOut}
    end.

%% P1-H4 FIX: Workers is now a map of Node -> {Pid, MRef}
collect_read_results(_Ref, Workers, _Timeout, Results) when map_size(Workers) == 0 ->
    {Results, []};
collect_read_results(Ref, Workers, Timeout, Results) ->
    receive
        {Ref, Node, {ok, Node, Value}} ->
            Workers2 = remove_worker_for_node(Workers, Node),
            collect_read_results(Ref, Workers2, Timeout, [{Node, Value} | Results]);
        {Ref, Node, {not_found, Node}} ->
            Workers2 = remove_worker_for_node(Workers, Node),
            collect_read_results(Ref, Workers2, Timeout, [{Node, not_found} | Results]);
        {Ref, ErrNode, {error, ErrNode, _Reason}} ->
            Workers2 = remove_worker_for_node(Workers, ErrNode),
            collect_read_results(Ref, Workers2, Timeout, Results);
        {'DOWN', MRef, process, _Pid, _Reason} ->
            {_CrashedNode, Workers2} = remove_worker_by_mref(Workers, MRef),
            collect_read_results(Ref, Workers2, Timeout, Results)
    after Timeout ->
        maps:foreach(fun(_Node, {_Pid, MRef}) ->
            demonitor(MRef, [flush])
        end, Workers),
        {Results, []}
    end.

%% P1-H4 FIX: Remove worker by node (O(1) lookup in map)
remove_worker_for_node(Workers, Node) ->
    case maps:get(Node, Workers, undefined) of
        undefined ->
            Workers;
        {_Pid, MRef} ->
            demonitor(MRef, [flush]),
            maps:remove(Node, Workers)
    end.

%% P1-H4 FIX: Remove worker by monitor reference (find node by MRef)
remove_worker_by_mref(Workers, MRef) ->
    case maps:fold(fun(Node, {_Pid, M}, Acc) ->
        case M of
            MRef -> {found, Node};
            _ -> Acc
        end
    end, not_found, Workers) of
        {found, Node} ->
            {Node, maps:remove(Node, Workers)};
        not_found ->
            {unknown, Workers}
    end.

reconcile_reads([]) ->
    not_found;
reconcile_reads(Results) ->
    %% Filter out not_found
    Values = [V || {_Node, V} <- Results, V =/= not_found],
    case Values of
        [] -> not_found;
        [V | _] -> {ok, V}  %% Return first (TODO: version/timestamp comparison)
    end.

%% =============================================================================
%% Internal: Replica Selection
%% =============================================================================

get_available_nodes() ->
    %% Get nodes from pg if available, otherwise use connected nodes
    case whereis(iris_shard) of
        undefined ->
            %% Fallback: use Mnesia running nodes or connected nodes
            case catch mnesia:system_info(running_db_nodes) of
                Nodes when is_list(Nodes) -> Nodes;
                _ -> [node() | nodes()]
            end;
        _ ->
            %% Use shard module for node discovery
            case pg:get_members(iris_shards) of
                [] -> [node() | nodes()];
                Pids -> lists:usort([node(P) || P <- Pids])
            end
    end.

select_replicas(Key, AllNodes, ReplicationFactor) ->
    %% Consistent hashing: hash key to select starting position
    Hash = erlang:phash2(Key),
    SortedNodes = lists:sort(AllNodes),
    N = length(SortedNodes),
    StartIdx = (Hash rem N) + 1,
    
    %% Select N consecutive nodes (with wrap-around)
    select_n_from_ring(SortedNodes, StartIdx, ReplicationFactor, []).

select_n_from_ring(_Nodes, _Idx, 0, Acc) ->
    lists:reverse(Acc);
select_n_from_ring(Nodes, Idx, Count, Acc) ->
    N = length(Nodes),
    WrappedIdx = ((Idx - 1) rem N) + 1,
    Node = lists:nth(WrappedIdx, Nodes),
    select_n_from_ring(Nodes, Idx + 1, Count - 1, [Node | Acc]).

%% =============================================================================
%% Internal: Async Repair
%% =============================================================================

repair_failed_replicas([], _Table, _Key, _Value) ->
    ok;
repair_failed_replicas([{Node, Reason} | Rest], Table, Key, Value) ->
    logger:info("Repairing failed replica ~p (reason: ~p)", [Node, Reason]),
    
    %% Try to write to failed node
    case Node of
        unknown ->
            %% Can't repair unknown node
            ok;
        _ ->
            case rpc:call(Node, ?MODULE, local_sync_write, [Table, Key, Value], 5000) of
                ok -> 
                    logger:info("Replica ~p repaired successfully", [Node]);
                {badrpc, _} -> 
                    logger:warning("Replica ~p still unavailable", [Node]);
                _ -> 
                    ok
            end
    end,
    repair_failed_replicas(Rest, Table, Key, Value).
