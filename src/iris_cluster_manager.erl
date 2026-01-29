-module(iris_cluster_manager).
-behaviour(gen_server).

%% =============================================================================
%% Cluster Self-Healing Manager
%% =============================================================================
%% FORENSIC_AUDIT_FIX: Automates cluster topology management.
%%
%% Problem: When nodes die/restart, cross-region replication must be manually
%% retriggered via shell scripts, which is error-prone and causes data stranding.
%%
%% Solution: This GenServer monitors nodeup/nodedown events and automatically:
%% 1. Detects new core nodes joining the cluster
%% 2. Waits for stability (configurable delay)
%% 3. Triggers cross-region replication setup
%% 4. Retries with exponential backoff on failure
%%
%% Configuration:
%% - {iris_core, [{cluster_manager_enabled, true}]} - Enable auto-healing
%% - {iris_core, [{stability_delay_ms, 5000}]} - Wait before triggering replication
%% - {iris_core, [{max_replication_retries, 3}]} - Max retry attempts
%% =============================================================================

-export([start_link/0]).
-export([get_status/0, force_replication/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_STABILITY_DELAY_MS, 5000).
-define(DEFAULT_MAX_RETRIES, 3).
-define(RETRY_BACKOFF_MS, 2000).

-record(state, {
    enabled = true :: boolean(),
    stability_delay_ms :: integer(),
    max_retries :: integer(),
    pending_nodes = #{} :: map(),  %% Node => {RetryCount, TimerRef}
    last_replication :: integer() | undefined,
    stats = #{} :: map()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get current cluster manager status
-spec get_status() -> map().
get_status() ->
    gen_server:call(?SERVER, get_status).

%% @doc Force replication check (for manual intervention)
-spec force_replication() -> ok | {error, term()}.
force_replication() ->
    gen_server:call(?SERVER, force_replication, 30000).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Check if cluster manager is enabled
    Enabled = application:get_env(iris_core, cluster_manager_enabled, true),
    StabilityDelay = application:get_env(iris_core, stability_delay_ms, ?DEFAULT_STABILITY_DELAY_MS),
    MaxRetries = application:get_env(iris_core, max_replication_retries, ?DEFAULT_MAX_RETRIES),
    
    case Enabled of
        true ->
            %% Subscribe to node events
            ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
            logger:info("iris_cluster_manager started, monitoring node events");
        false ->
            logger:info("iris_cluster_manager disabled by config")
    end,
    
    {ok, #state{
        enabled = Enabled,
        stability_delay_ms = StabilityDelay,
        max_retries = MaxRetries,
        stats = #{
            nodeup_events => 0,
            nodedown_events => 0,
            replication_triggers => 0,
            replication_successes => 0,
            replication_failures => 0
        }
    }}.

handle_call(get_status, _From, State) ->
    Status = #{
        enabled => State#state.enabled,
        pending_nodes => maps:keys(State#state.pending_nodes),
        last_replication => State#state.last_replication,
        stats => State#state.stats
    },
    {reply, Status, State};

handle_call(force_replication, _From, State) ->
    logger:info("Manual replication trigger requested"),
    Result = do_replication(),
    NewStats = case Result of
        ok ->
            maps:update_with(replication_successes, fun(V) -> V + 1 end, 1, State#state.stats);
        {error, _} ->
            maps:update_with(replication_failures, fun(V) -> V + 1 end, 1, State#state.stats)
    end,
    {reply, Result, State#state{
        stats = NewStats,
        last_replication = erlang:system_time(second)
    }};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Node joined the cluster
handle_info({nodeup, Node, _Info}, State = #state{enabled = true}) ->
    logger:info("Cluster node up: ~p", [Node]),
    NewStats = maps:update_with(nodeup_events, fun(V) -> V + 1 end, 1, State#state.stats),
    
    %% Only schedule replication for core nodes
    case is_core_node(Node) of
        true ->
            %% Cancel any existing timer for this node
            State1 = cancel_pending_timer(Node, State),
            
            %% Schedule replication check after stability delay
            TimerRef = erlang:send_after(State#state.stability_delay_ms, self(), {check_replication, Node, 0}),
            PendingNodes = maps:put(Node, {0, TimerRef}, State1#state.pending_nodes),
            
            logger:info("Scheduling replication check for ~p in ~pms", 
                       [Node, State#state.stability_delay_ms]),
            {noreply, State1#state{pending_nodes = PendingNodes, stats = NewStats}};
        false ->
            {noreply, State#state{stats = NewStats}}
    end;

handle_info({nodeup, Node, _Info}, State = #state{enabled = false}) ->
    logger:debug("Ignoring nodeup ~p (cluster manager disabled)", [Node]),
    {noreply, State};

%% Node left the cluster
handle_info({nodedown, Node, Info}, State = #state{enabled = true}) ->
    logger:warning("Cluster node down: ~p, reason: ~p", [Node, Info]),
    NewStats = maps:update_with(nodedown_events, fun(V) -> V + 1 end, 1, State#state.stats),
    
    %% Cancel any pending replication for this node
    State1 = cancel_pending_timer(Node, State),
    {noreply, State1#state{stats = NewStats}};

handle_info({nodedown, Node, _Info}, State = #state{enabled = false}) ->
    logger:debug("Ignoring nodedown ~p (cluster manager disabled)", [Node]),
    {noreply, State};

%% Replication check triggered
handle_info({check_replication, Node, RetryCount}, State) ->
    logger:info("Checking replication for node ~p (attempt ~p/~p)", 
               [Node, RetryCount + 1, State#state.max_retries]),
    
    %% Remove from pending (timer already fired)
    PendingNodes = maps:remove(Node, State#state.pending_nodes),
    
    %% Verify node is still connected
    case lists:member(Node, nodes()) of
        true ->
            %% Trigger replication
            NewStats = maps:update_with(replication_triggers, fun(V) -> V + 1 end, 1, State#state.stats),
            
            case do_replication() of
                ok ->
                    logger:info("Replication successful after node ~p joined", [Node]),
                    FinalStats = maps:update_with(replication_successes, fun(V) -> V + 1 end, 1, NewStats),
                    {noreply, State#state{
                        pending_nodes = PendingNodes,
                        stats = FinalStats,
                        last_replication = erlang:system_time(second)
                    }};
                {error, Reason} ->
                    logger:warning("Replication failed: ~p", [Reason]),
                    %% Retry with backoff if under limit
                    State2 = maybe_schedule_retry(Node, RetryCount, State#state{pending_nodes = PendingNodes}),
                    FinalStats = maps:update_with(replication_failures, fun(V) -> V + 1 end, 1, NewStats),
                    {noreply, State2#state{stats = FinalStats}}
            end;
        false ->
            logger:info("Node ~p disconnected before replication check", [Node]),
            {noreply, State#state{pending_nodes = PendingNodes}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    net_kernel:monitor_nodes(false),
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% Check if a node is a core node (by naming convention)
is_core_node(Node) ->
    NodeStr = atom_to_list(Node),
    lists:prefix("core", NodeStr) orelse lists:prefix("iris_core", NodeStr).

%% Cancel any pending timer for a node
cancel_pending_timer(Node, State) ->
    case maps:get(Node, State#state.pending_nodes, undefined) of
        undefined ->
            State;
        {_RetryCount, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            State#state{pending_nodes = maps:remove(Node, State#state.pending_nodes)}
    end.

%% Schedule retry with exponential backoff
maybe_schedule_retry(Node, RetryCount, State) when RetryCount < State#state.max_retries ->
    Backoff = ?RETRY_BACKOFF_MS * (1 bsl RetryCount),  %% Exponential backoff
    logger:info("Scheduling retry ~p for node ~p in ~pms", [RetryCount + 1, Node, Backoff]),
    TimerRef = erlang:send_after(Backoff, self(), {check_replication, Node, RetryCount + 1}),
    PendingNodes = maps:put(Node, {RetryCount + 1, TimerRef}, State#state.pending_nodes),
    State#state{pending_nodes = PendingNodes};
maybe_schedule_retry(Node, RetryCount, State) ->
    logger:error("Max retries (~p) exceeded for node ~p", [RetryCount, Node]),
    State.

%% Perform the actual replication
do_replication() ->
    try
        %% Call iris_core's replication function
        case whereis(iris_core) of
            undefined ->
                %% iris_core not running as a named process, try direct call
                iris_core:init_cross_region_replication();
            _ ->
                iris_core:init_cross_region_replication()
        end,
        ok
    catch
        Class:Reason:Stack ->
            logger:error("Replication failed: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.
