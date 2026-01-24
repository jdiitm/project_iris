-module(iris_raft).

%% =============================================================================
%% Iris Raft: CP Mode with Linearizable Consistency
%% =============================================================================
%% 
%% PURPOSE:
%% Provides TRUE CP (Consistency + Partition tolerance) semantics using Raft
%% consensus. Unlike the default Hardened AP mode, this guarantees:
%% 
%%   1. LINEARIZABLE READS: Read your own writes, always see latest committed value
%%   2. NO STALE DATA: Reads blocked during partition (prefer correctness over availability)
%%   3. AUTOMATIC LEADER ELECTION: No manual failover required
%% 
%% WHEN TO USE:
%%   - Critical message storage requiring zero data loss
%%   - Financial transactions or audit logs
%%   - Any data where consistency > availability
%% 
%% WHEN NOT TO USE:
%%   - Presence/status (stale OK, use Mnesia)
%%   - High-throughput ephemeral data
%%   - Latency-sensitive reads
%% 
%% IMPLEMENTATION:
%% Uses the `ra` library (RabbitMQ's Raft implementation) which provides:
%%   - Proven production-grade Raft consensus
%%   - Automatic leader election and log compaction
%%   - Snapshotting for fast recovery
%% 
%% CONFIGURATION:
%%   {iris_core, [
%%       {consistency_mode, cp},        %% Options: ap | hardened_ap | cp
%%       {cp_cluster_name, iris_raft},  %% Raft cluster name
%%       {cp_data_dir, "/var/lib/iris/raft"}
%%   ]}
%% 
%% =============================================================================

%% Behaviour for ra state machine
-behaviour(ra_machine).

%% API
-export([start_cluster/1, start_cluster/2]).
-export([join_cluster/1, leave_cluster/0]).
-export([put/3, get/2, delete/2]).
-export([get_leader/0, get_members/0]).
-export([is_cp_mode/0]).

%% ra_machine callbacks
-export([init/1, apply/3, state_enter/2]).
-export([snapshot_module/0]).

%% State machine state
-record(state, {
    data = #{} :: map(),      %% Table -> Key -> Value
    sequences = #{} :: map()  %% Table -> Key -> Version (for CAS)
}).

%% =============================================================================
%% API: Cluster Management
%% =============================================================================

%% @doc Start a new Raft cluster with this node as the initial member
-spec start_cluster([node()]) -> ok | {error, term()}.
start_cluster(Nodes) ->
    start_cluster(Nodes, #{}).

-spec start_cluster([node()], map()) -> ok | {error, term()}.
start_cluster(Nodes, Opts) ->
    ClusterName = maps:get(cluster_name, Opts, iris_raft),
    DataDir = maps:get(data_dir, Opts, get_data_dir()),
    
    %% Ensure ra application is started
    case application:ensure_all_started(ra) of
        {ok, _} -> 
            do_start_cluster(ClusterName, DataDir, Nodes);
        {error, RaStartError} ->
            logger:error("Failed to start ra application: ~p", [RaStartError]),
            {error, {ra_not_started, RaStartError}}
    end.

do_start_cluster(ClusterName, DataDir, Nodes) ->
    %% Configure ra
    ok = application:set_env(ra, data_dir, DataDir),
    
    %% Build server configuration
    ServerIds = [{ClusterName, N} || N <- Nodes],
    
    Machine = {module, ?MODULE, #{}},
    
    %% Start the Raft cluster
    case ra:start_cluster(default, ClusterName, Machine, ServerIds) of
        {ok, Started, _NotStarted} ->
            logger:info("Raft cluster started with members: ~p", [Started]),
            ok;
        {error, ClusterError} ->
            logger:error("Failed to start Raft cluster: ~p", [ClusterError]),
            {error, ClusterError}
    end.

%% @doc Join an existing Raft cluster
-spec join_cluster(node()) -> ok | {error, term()}.
join_cluster(ExistingNode) ->
    ClusterName = get_cluster_name(),
    ServerId = {ClusterName, node()},
    ExistingServerId = {ClusterName, ExistingNode},
    
    %% Start local ra server
    Machine = {module, ?MODULE, #{}},
    case ra:start_server(default, ClusterName, ServerId, Machine, [ExistingServerId]) of
        ok ->
            %% Add ourselves to the cluster
            case ra:add_member(ExistingServerId, ServerId) of
                {ok, _, _} ->
                    logger:info("Joined Raft cluster via ~p", [ExistingNode]),
                    ok;
                {error, Reason} ->
                    logger:error("Failed to join cluster: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Leave the Raft cluster
-spec leave_cluster() -> ok | {error, term()}.
leave_cluster() ->
    ClusterName = get_cluster_name(),
    ServerId = {ClusterName, node()},
    
    case ra:leave_and_delete_server(default, ClusterName, ServerId) of
        ok ->
            logger:info("Left Raft cluster"),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% =============================================================================
%% API: Data Operations (Linearizable)
%% =============================================================================

%% @doc Put a value with linearizable consistency
%% Blocks until majority acknowledges the write
-spec put(atom(), term(), term()) -> ok | {error, term()}.
put(Table, Key, Value) ->
    ClusterName = get_cluster_name(),
    ServerId = {ClusterName, node()},
    
    Command = {put, Table, Key, Value, erlang:system_time(millisecond)},
    
    case ra:process_command(ServerId, Command, 5000) of
        {ok, ok, _Leader} ->
            ok;
        {ok, {error, Reason}, _Leader} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason};
        {timeout, _} ->
            {error, timeout}
    end.

%% @doc Get a value with linearizable consistency
%% Reads from leader to ensure latest committed value
-spec get(atom(), term()) -> {ok, term()} | not_found | {error, term()}.
get(Table, Key) ->
    ClusterName = get_cluster_name(),
    ServerId = {ClusterName, node()},
    
    %% Use consistent query (reads from leader)
    Query = fun(State) ->
        TableData = maps:get(Table, State#state.data, #{}),
        case maps:get(Key, TableData, not_found) of
            not_found -> not_found;
            Value -> {ok, Value}
        end
    end,
    
    case ra:consistent_query(ServerId, Query, 5000) of
        {ok, Result, _Leader} ->
            Result;
        {error, Reason} ->
            {error, Reason};
        {timeout, _} ->
            {error, timeout}
    end.

%% @doc Delete a value with linearizable consistency
-spec delete(atom(), term()) -> ok | {error, term()}.
delete(Table, Key) ->
    ClusterName = get_cluster_name(),
    ServerId = {ClusterName, node()},
    
    Command = {delete, Table, Key, erlang:system_time(millisecond)},
    
    case ra:process_command(ServerId, Command, 5000) of
        {ok, ok, _Leader} ->
            ok;
        {error, Reason} ->
            {error, Reason};
        {timeout, _} ->
            {error, timeout}
    end.

%% =============================================================================
%% API: Cluster Information
%% =============================================================================

%% @doc Get the current Raft leader
-spec get_leader() -> {ok, node()} | {error, no_leader}.
get_leader() ->
    ClusterName = get_cluster_name(),
    ServerId = {ClusterName, node()},
    
    case ra:members(ServerId) of
        {ok, _Members, Leader} when Leader =/= undefined ->
            {_, LeaderNode} = Leader,
            {ok, LeaderNode};
        _ ->
            {error, no_leader}
    end.

%% @doc Get all cluster members
-spec get_members() -> {ok, [node()]} | {error, term()}.
get_members() ->
    ClusterName = get_cluster_name(),
    ServerId = {ClusterName, node()},
    
    case ra:members(ServerId) of
        {ok, Members, _Leader} ->
            Nodes = [N || {_, N} <- Members],
            {ok, Nodes};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Check if CP mode is enabled
-spec is_cp_mode() -> boolean().
is_cp_mode() ->
    case application:get_env(iris_core, consistency_mode) of
        {ok, cp} -> true;
        _ -> false
    end.

%% =============================================================================
%% ra_machine Callbacks
%% =============================================================================

%% Initialize the state machine
init(_Config) ->
    #state{}.

%% Apply a command to the state machine
apply(_Meta, {put, Table, Key, Value, _Timestamp}, State) ->
    TableData = maps:get(Table, State#state.data, #{}),
    NewTableData = maps:put(Key, Value, TableData),
    NewData = maps:put(Table, NewTableData, State#state.data),
    
    %% Update sequence number for CAS support
    Sequences = State#state.sequences,
    TableSeqs = maps:get(Table, Sequences, #{}),
    OldSeq = maps:get(Key, TableSeqs, 0),
    NewTableSeqs = maps:put(Key, OldSeq + 1, TableSeqs),
    NewSequences = maps:put(Table, NewTableSeqs, Sequences),
    
    {State#state{data = NewData, sequences = NewSequences}, ok};

apply(_Meta, {delete, Table, Key, _Timestamp}, State) ->
    TableData = maps:get(Table, State#state.data, #{}),
    NewTableData = maps:remove(Key, TableData),
    NewData = maps:put(Table, NewTableData, State#state.data),
    {State#state{data = NewData}, ok};

apply(_Meta, {cas, Table, Key, ExpectedValue, NewValue, _Timestamp}, State) ->
    %% Compare-and-swap for optimistic concurrency
    TableData = maps:get(Table, State#state.data, #{}),
    CurrentValue = maps:get(Key, TableData, not_found),
    
    case CurrentValue of
        ExpectedValue ->
            NewTableData = maps:put(Key, NewValue, TableData),
            NewData = maps:put(Table, NewTableData, State#state.data),
            {State#state{data = NewData}, ok};
        _ ->
            {State, {error, {cas_failed, CurrentValue}}}
    end;

apply(_Meta, _Command, State) ->
    {State, {error, unknown_command}}.

%% Called when a server changes state (follower, candidate, leader)
state_enter(leader, State) ->
    logger:info("This node became Raft leader"),
    {State, []};
state_enter(follower, State) ->
    logger:info("This node became Raft follower"),
    {State, []};
state_enter(_StateName, State) ->
    {State, []}.

%% Snapshot module for state serialization
snapshot_module() ->
    ?MODULE.

%% =============================================================================
%% Internal Helpers
%% =============================================================================

get_cluster_name() ->
    application:get_env(iris_core, cp_cluster_name, iris_raft).

get_data_dir() ->
    case application:get_env(iris_core, cp_data_dir) of
        {ok, Dir} -> Dir;
        _ -> 
            %% Default to Mnesia directory + /raft
            MnesiaDir = mnesia:system_info(directory),
            filename:join(MnesiaDir, "raft")
    end.
