-module(iris_partition_guard).
-behaviour(gen_server).

%% =============================================================================
%% Partition Guard: Split-Brain Detection and Safe Mode
%% =============================================================================
%% This module monitors cluster membership and detects network partitions.
%% When quorum is lost:
%% - Writes are rejected to prevent data divergence
%% - Reads are allowed (stale data is better than no data)
%% - Warnings are logged continuously
%%
%% RFC Compliance:
%% - Supports hardened AP semantics with explicit partition handling
%% - Prevents silent data divergence during split-brain
%% =============================================================================

-export([start_link/0]).
-export([is_safe_for_writes/0, get_status/0, force_unsafe_mode/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL_MS, 5000).  %% Check every 5 seconds
-define(QUORUM_RECOVERY_DELAY_MS, 10000).  %% Wait 10s before re-enabling writes

-record(state, {
    mode = normal :: normal | safe_mode | forced_unsafe,
    expected_nodes = [] :: [node()],
    visible_nodes = [] :: [node()],
    last_quorum_loss :: integer() | undefined,
    quorum_threshold = 0.5 :: float(),  %% Must see >50% of expected nodes
    check_timer :: reference() | undefined,
    partition_count = 0 :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if cluster is safe for write operations
%% Returns 'ok' if safe, {error, partition_detected} if in safe mode
-spec is_safe_for_writes() -> ok | {error, partition_detected}.
is_safe_for_writes() ->
    case whereis(?SERVER) of
        undefined -> ok;  %% Guard not running = permissive
        _Pid ->
            gen_server:call(?SERVER, is_safe_for_writes, 1000)
    end.

%% @doc Get current partition guard status
-spec get_status() -> map().
get_status() ->
    case whereis(?SERVER) of
        undefined -> #{mode => not_running, safe_for_writes => true};
        _Pid ->
            gen_server:call(?SERVER, get_status, 1000)
    end.

%% @doc Force unsafe mode (for emergency operations)
%% WARNING: This bypasses partition detection - use with extreme caution
-spec force_unsafe_mode(boolean()) -> ok.
force_unsafe_mode(Enable) ->
    gen_server:call(?SERVER, {force_unsafe_mode, Enable}).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% Get expected cluster nodes from config
    ExpectedNodes = get_expected_nodes(),
    
    %% Schedule periodic check
    Timer = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_partition),
    
    logger:info("Partition Guard started. Expected nodes: ~p", [ExpectedNodes]),
    
    {ok, #state{
        expected_nodes = ExpectedNodes,
        visible_nodes = [node() | nodes()],
        check_timer = Timer
    }}.

handle_call(is_safe_for_writes, _From, State = #state{mode = normal}) ->
    {reply, ok, State};
handle_call(is_safe_for_writes, _From, State = #state{mode = forced_unsafe}) ->
    {reply, ok, State};
handle_call(is_safe_for_writes, _From, State = #state{mode = safe_mode}) ->
    {reply, {error, partition_detected}, State};

handle_call(get_status, _From, State) ->
    Status = #{
        mode => State#state.mode,
        safe_for_writes => State#state.mode =/= safe_mode,
        expected_nodes => State#state.expected_nodes,
        visible_nodes => State#state.visible_nodes,
        partition_count => State#state.partition_count,
        last_quorum_loss => State#state.last_quorum_loss
    },
    {reply, Status, State};

handle_call({force_unsafe_mode, true}, _From, State) ->
    logger:warning("=== PARTITION GUARD: FORCED UNSAFE MODE ENABLED ==="),
    logger:warning("Writes are now allowed regardless of partition status"),
    {reply, ok, State#state{mode = forced_unsafe}};

handle_call({force_unsafe_mode, false}, _From, State) ->
    logger:info("Partition Guard: Forced unsafe mode disabled, resuming normal checks"),
    {reply, ok, State#state{mode = normal}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_partition, State = #state{mode = forced_unsafe}) ->
    %% Skip checks in forced unsafe mode
    Timer = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_partition),
    {noreply, State#state{check_timer = Timer}};

handle_info(check_partition, State) ->
    NewState = do_partition_check(State),
    Timer = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_partition),
    {noreply, NewState#state{check_timer = Timer}};

handle_info({nodedown, Node}, State) ->
    logger:warning("Partition Guard: Node down detected: ~p", [Node]),
    {noreply, do_partition_check(State)};

handle_info({nodeup, Node}, State) ->
    logger:info("Partition Guard: Node up detected: ~p", [Node]),
    {noreply, do_partition_check(State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

do_partition_check(State = #state{expected_nodes = Expected, quorum_threshold = Threshold}) ->
    %% Get currently visible nodes
    VisibleNodes = [node() | nodes()],
    
    %% Also check Mnesia's view of running nodes
    MnesiaNodes = try mnesia:system_info(running_db_nodes) catch _:_ -> [] end,
    
    %% Combine both views
    AllVisible = lists:usort(VisibleNodes ++ MnesiaNodes),
    
    %% Calculate quorum
    ExpectedCount = length(Expected),
    VisibleCount = length([N || N <- Expected, lists:member(N, AllVisible)]),
    
    HasQuorum = case ExpectedCount of
        0 -> true;  %% No expected nodes configured = always have quorum
        _ -> (VisibleCount / ExpectedCount) > Threshold
    end,
    
    case {HasQuorum, State#state.mode} of
        {true, safe_mode} ->
            %% Quorum restored - check if we should exit safe mode
            maybe_exit_safe_mode(State#state{visible_nodes = AllVisible});
        
        {false, normal} ->
            %% Quorum lost - enter safe mode
            enter_safe_mode(State#state{visible_nodes = AllVisible});
        
        {_, _} ->
            %% No change needed
            State#state{visible_nodes = AllVisible}
    end.

enter_safe_mode(State) ->
    Now = os:system_time(second),
    NewCount = State#state.partition_count + 1,
    
    logger:error("=== PARTITION DETECTED ==="),
    logger:error("Expected nodes: ~p", [State#state.expected_nodes]),
    logger:error("Visible nodes: ~p", [State#state.visible_nodes]),
    logger:error("Entering SAFE MODE - writes will be rejected"),
    logger:error("Partition count: ~p", [NewCount]),
    
    %% Log to metrics if available
    try iris_metrics:increment(partition_detected) catch _:_ -> ok end,
    
    State#state{
        mode = safe_mode,
        last_quorum_loss = Now,
        partition_count = NewCount
    }.

maybe_exit_safe_mode(State = #state{last_quorum_loss = LastLoss}) ->
    Now = os:system_time(second),
    TimeSinceLoss = (Now - LastLoss) * 1000,  %% Convert to ms
    
    case TimeSinceLoss >= ?QUORUM_RECOVERY_DELAY_MS of
        true ->
            logger:info("=== QUORUM RESTORED ==="),
            logger:info("Visible nodes: ~p", [State#state.visible_nodes]),
            logger:info("Exiting safe mode - writes enabled"),
            State#state{mode = normal};
        false ->
            %% Still in recovery delay
            RemainingMs = ?QUORUM_RECOVERY_DELAY_MS - TimeSinceLoss,
            logger:info("Quorum restored, but waiting ~p ms before enabling writes", [RemainingMs]),
            State
    end.

get_expected_nodes() ->
    %% Get from config, or use empty list (permissive mode)
    case application:get_env(iris_core, expected_cluster_nodes) of
        {ok, Nodes} when is_list(Nodes) -> Nodes;
        _ ->
            %% Fallback: use join_seeds if configured
            case application:get_env(iris_core, join_seeds) of
                {ok, Seeds} when is_list(Seeds) -> Seeds;
                _ -> []
            end
    end.
