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
%% CRITICAL: Dynamic mode is DEPRECATED (CB-1 Audit Finding)
%% ---------------------------------------------------------
%% Dynamic mode uses pg for membership discovery, which shrinks during
%% partitions. This defeats split-brain protection because both sides
%% of a partition see 100% of their (reduced) expected nodes.
%%
%% ALWAYS use static mode with explicit expected_cluster_nodes in production.
%%
%% RFC Compliance:
%% - Supports hardened AP semantics with explicit partition handling
%% - Prevents silent data divergence during split-brain
%% =============================================================================

%% pg group for dynamic membership discovery
-define(PG_GROUP, iris_core_nodes).

-export([start_link/0]).
-export([is_safe_for_writes/0, get_status/0, force_unsafe_mode/1]).
-export([resolve_authority/4]).  %% FM-2: Split-brain resolution
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL_MS, 5000).  %% Check every 5 seconds
-define(QUORUM_RECOVERY_DELAY_MS, 10000).  %% Wait 10s before re-enabling writes

-record(state, {
    mode = normal :: normal | safe_mode | forced_unsafe,
    membership_mode = static :: static | dynamic,  %% AUDIT FIX (Finding #3)
    expected_nodes = [] :: [node()],
    visible_nodes = [] :: [node()],
    last_quorum_loss :: integer() | undefined,
    quorum_threshold = 0.5 :: float(),  %% Must see >50% of expected nodes
    check_timer :: reference() | undefined,
    partition_count = 0 :: integer(),
    epoch = 0 :: non_neg_integer()  %% FM-2: Epoch counter for split-brain resolution
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
    %% Determine membership mode (with deprecation check)
    MembershipMode = get_membership_mode(),
    
    %% CB-1 AUDIT FIX: Emit CRITICAL warning for dynamic mode
    case MembershipMode of
        dynamic ->
            logger:critical("======================================================="),
            logger:critical("PARTITION GUARD: DYNAMIC MODE IS DEPRECATED (CB-1)"),
            logger:critical(""),
            logger:critical("Dynamic mode uses pg for membership discovery, which"),
            logger:critical("SHRINKS during network partitions. This defeats"),
            logger:critical("split-brain protection - BOTH sides of a partition"),
            logger:critical("will see 100% quorum and accept writes!"),
            logger:critical(""),
            logger:critical("ACTION REQUIRED: Configure expected_cluster_nodes"),
            logger:critical("and set partition_guard_mode = static (or remove it)."),
            logger:critical("======================================================="),
            %% Still register with pg for node discovery hints (not quorum)
            register_with_pg();
        static ->
            ok
    end,
    
    %% Get expected cluster nodes from config
    %% CB-1 FIX: In dynamic mode, still use static config for quorum checks
    ExpectedNodes = get_static_expected_nodes(),
    
    %% Check production safety
    IsProduction = is_production_env(),
    
    %% Warn if no expected nodes configured
    case {ExpectedNodes, IsProduction} of
        {[], true} ->
            logger:critical("======================================================="),
            logger:critical("PARTITION GUARD: PRODUCTION WITHOUT SPLIT-BRAIN PROTECTION"),
            logger:critical(""),
            logger:critical("IRIS_ENV=prod but no expected_cluster_nodes configured!"),
            logger:critical("This cluster is VULNERABLE to split-brain data corruption."),
            logger:critical(""),
            logger:critical("Configure iris_core.expected_cluster_nodes immediately."),
            logger:critical("=======================================================");
        {[], false} ->
            logger:warning("======================================================="),
            logger:warning("PARTITION GUARD: No expected_cluster_nodes configured"),
            logger:warning("Split-brain protection is DISABLED (permissive mode)."),
            logger:warning("Configure iris_core.expected_cluster_nodes for production."),
            logger:warning("=======================================================");
        _ ->
            logger:info("Partition Guard enabled with ~p expected nodes", 
                       [length(ExpectedNodes)])
    end,
    
    %% Schedule periodic check
    Timer = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_partition),
    
    logger:info("Partition Guard started. Expected nodes: ~p", [ExpectedNodes]),
    
    {ok, #state{
        membership_mode = static,  %% CB-1 FIX: Always use static for quorum checks
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
        membership_mode => State#state.membership_mode,  %% AUDIT FIX (Finding #3)
        safe_for_writes => State#state.mode =/= safe_mode,
        expected_nodes => State#state.expected_nodes,
        visible_nodes => State#state.visible_nodes,
        partition_count => State#state.partition_count,
        last_quorum_loss => State#state.last_quorum_loss,
        epoch => State#state.epoch  %% FM-2: Epoch for split-brain resolution
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

do_partition_check(State = #state{quorum_threshold = Threshold}) ->
    %% CB-1 FIX: Always use static expected nodes for quorum checks
    %% Dynamic pg membership shrinks during partitions, defeating protection
    Expected = State#state.expected_nodes,
    
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
            maybe_exit_safe_mode(State#state{expected_nodes = Expected, visible_nodes = AllVisible});
        
        {false, normal} ->
            %% Quorum lost - enter safe mode
            enter_safe_mode(State#state{expected_nodes = Expected, visible_nodes = AllVisible});
        
        {_, _} ->
            %% No change needed
            State#state{expected_nodes = Expected, visible_nodes = AllVisible}
    end.

enter_safe_mode(State) ->
    Now = os:system_time(second),
    NewCount = State#state.partition_count + 1,
    NewEpoch = State#state.epoch + 1,  %% FM-2: Increment epoch on partition
    
    logger:error("=== PARTITION DETECTED (epoch ~p) ===", [NewEpoch]),
    logger:error("Expected nodes: ~p", [State#state.expected_nodes]),
    logger:error("Visible nodes: ~p", [State#state.visible_nodes]),
    logger:error("Entering SAFE MODE - writes will be rejected"),
    logger:error("Partition count: ~p", [NewCount]),
    
    %% Log to metrics if available
    try iris_metrics:increment(partition_detected) catch _:_ -> ok end,
    
    State#state{
        mode = safe_mode,
        last_quorum_loss = Now,
        partition_count = NewCount,
        epoch = NewEpoch
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

%% =============================================================================
%% Configuration and Environment Checks
%% =============================================================================

%% Check if running in production environment
is_production_env() ->
    case os:getenv("IRIS_ENV") of
        "prod" -> true;
        "production" -> true;
        _ ->
            case application:get_env(iris_core, environment) of
                {ok, prod} -> true;
                {ok, production} -> true;
                _ -> false
            end
    end.

%% Get membership mode from config (with deprecation warning)
get_membership_mode() ->
    case application:get_env(iris_core, partition_guard_mode) of
        {ok, dynamic} -> dynamic;  %% Deprecated, warning emitted in init
        {ok, static} -> static;
        _ -> static  %% Default to static (safe)
    end.

%% Register this node with pg for dynamic discovery
register_with_pg() ->
    %% Ensure pg is started
    try
        case pg:start_link(?PG_GROUP) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            _ -> ok
        end
    catch
        _:_ -> ok
    end,
    
    %% Join the core nodes group
    try
        pg:join(?PG_GROUP, self())
    catch
        _:_ -> ok
    end.

%% Get static expected nodes from config
%% CB-1 FIX: This is now the ONLY method for determining expected nodes.
%% Dynamic pg-based discovery is deprecated because pg membership shrinks
%% during partitions, defeating split-brain protection.
get_static_expected_nodes() ->
    case application:get_env(iris_core, expected_cluster_nodes) of
        {ok, Nodes} when is_list(Nodes) -> Nodes;
        _ ->
            %% Fallback: use join_seeds if configured
            case application:get_env(iris_core, join_seeds) of
                {ok, Seeds} when is_list(Seeds) -> Seeds;
                _ -> []
            end
    end.

%% =============================================================================
%% FM-2: Split-Brain Resolution
%% RFC-001 v4.0 Section 7.1.1:
%% Higher epoch wins; equal epoch ties broken by lowest node ID.
%% =============================================================================

-spec resolve_authority(non_neg_integer(), node(), non_neg_integer(), node()) ->
    {authoritative, node()}.
resolve_authority(EpochA, NodeA, EpochB, _NodeB) when EpochA > EpochB ->
    {authoritative, NodeA};
resolve_authority(EpochA, _NodeA, EpochB, NodeB) when EpochA < EpochB ->
    {authoritative, NodeB};
resolve_authority(_Epoch, NodeA, _Epoch2, NodeB) ->
    %% Equal epoch: lowest node ID wins
    case NodeA < NodeB of
        true -> {authoritative, NodeA};
        false -> {authoritative, NodeB}
    end.
