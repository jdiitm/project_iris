-module(iris_mailbox_monitor).

%% =============================================================================
%% Process Mailbox Health Monitor
%% =============================================================================
%% Per PRINCIPAL_AUDIT_REPORT.md Section 6.2 (Falsification Criteria):
%% - Poll process_info(Pid, message_queue_len) for critical processes
%% - Alert when any gen_server > 10,000 messages
%% - Export as Prometheus gauge
%%
%% This helps detect process bottlenecks before they cause system collapse.
%% =============================================================================

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register_process/2, unregister_process/1]).
-export([get_all_mailbox_lengths/0, get_max_mailbox/0]).
-export([check_health/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL_MS, 1000).       %% Check every 1 second
-define(WARNING_THRESHOLD, 5000).       %% Warn at 5000 messages
-define(CRITICAL_THRESHOLD, 10000).     %% Alert at 10000 messages

-record(state, {
    monitored = #{} :: #{atom() => pid()},
    check_timer :: reference() | undefined,
    last_check = #{} :: map(),
    alerts = [] :: list()
}).

%% =============================================================================
%% API
%% =============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a process to be monitored
-spec register_process(atom(), pid()) -> ok.
register_process(Name, Pid) ->
    gen_server:cast(?SERVER, {register, Name, Pid}).

%% @doc Unregister a process from monitoring
-spec unregister_process(atom()) -> ok.
unregister_process(Name) ->
    gen_server:cast(?SERVER, {unregister, Name}).

%% @doc Get mailbox lengths for all monitored processes
-spec get_all_mailbox_lengths() -> #{atom() => integer()}.
get_all_mailbox_lengths() ->
    gen_server:call(?SERVER, get_all_mailbox_lengths).

%% @doc Get the maximum mailbox length across all monitored processes
-spec get_max_mailbox() -> {atom(), integer()} | {none, 0}.
get_max_mailbox() ->
    gen_server:call(?SERVER, get_max_mailbox).

%% @doc Check health status (returns ok | {warning, list()} | {critical, list()})
-spec check_health() -> ok | {warning, list()} | {critical, list()}.
check_health() ->
    gen_server:call(?SERVER, check_health).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Auto-register known critical processes
    auto_register_known_processes(),
    
    %% Start periodic check
    TimerRef = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_mailboxes),
    
    logger:info("Mailbox monitor started (warning=~p, critical=~p)", 
                [?WARNING_THRESHOLD, ?CRITICAL_THRESHOLD]),
    
    {ok, #state{
        monitored = #{},
        check_timer = TimerRef,
        last_check = #{},
        alerts = []
    }}.

handle_call(get_all_mailbox_lengths, _From, State) ->
    Lengths = check_all_mailboxes(State#state.monitored),
    {reply, Lengths, State#state{last_check = Lengths}};

handle_call(get_max_mailbox, _From, State) ->
    Lengths = check_all_mailboxes(State#state.monitored),
    Result = case maps:to_list(Lengths) of
        [] -> {none, 0};
        List ->
            {MaxName, MaxLen} = lists:foldl(fun({N, L}, {_, AccL} = Acc) ->
                if L > AccL -> {N, L};
                   true -> Acc
                end
            end, {none, 0}, List),
            {MaxName, MaxLen}
    end,
    {reply, Result, State#state{last_check = Lengths}};

handle_call(check_health, _From, State) ->
    Lengths = check_all_mailboxes(State#state.monitored),
    
    Critical = [{N, L} || {N, L} <- maps:to_list(Lengths), L >= ?CRITICAL_THRESHOLD],
    Warning = [{N, L} || {N, L} <- maps:to_list(Lengths), 
               L >= ?WARNING_THRESHOLD, L < ?CRITICAL_THRESHOLD],
    
    Result = case {Critical, Warning} of
        {[], []} -> ok;
        {[], W} -> {warning, W};
        {C, _} -> {critical, C}
    end,
    
    {reply, Result, State#state{last_check = Lengths}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register, Name, Pid}, State) ->
    NewMonitored = maps:put(Name, Pid, State#state.monitored),
    logger:debug("Registered process ~p (pid ~p) for mailbox monitoring", [Name, Pid]),
    {noreply, State#state{monitored = NewMonitored}};

handle_cast({unregister, Name}, State) ->
    NewMonitored = maps:remove(Name, State#state.monitored),
    {noreply, State#state{monitored = NewMonitored}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_mailboxes, State) ->
    %% Check all mailboxes
    Lengths = check_all_mailboxes(State#state.monitored),
    
    %% Update Prometheus metrics
    lists:foreach(fun({Name, Length}) ->
        MetricName = list_to_atom("iris_mailbox_" ++ atom_to_list(Name)),
        iris_metrics:set(MetricName, Length)
    end, maps:to_list(Lengths)),
    
    %% Export max mailbox
    MaxLen = case maps:values(Lengths) of
        [] -> 0;
        Vals -> lists:max(Vals)
    end,
    iris_metrics:set(iris_mailbox_max, MaxLen),
    
    %% Check for alerts
    NewAlerts = lists:filtermap(fun({Name, Length}) ->
        if
            Length >= ?CRITICAL_THRESHOLD ->
                logger:error("CRITICAL: Process ~p mailbox at ~p messages (threshold: ~p)",
                            [Name, Length, ?CRITICAL_THRESHOLD]),
                {true, {critical, Name, Length}};
            Length >= ?WARNING_THRESHOLD ->
                logger:warning("WARNING: Process ~p mailbox at ~p messages (threshold: ~p)",
                              [Name, Length, ?WARNING_THRESHOLD]),
                {true, {warning, Name, Length}};
            true ->
                false
        end
    end, maps:to_list(Lengths)),
    
    %% Reschedule
    TimerRef = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_mailboxes),
    
    {noreply, State#state{
        check_timer = TimerRef,
        last_check = Lengths,
        alerts = NewAlerts
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.check_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

check_all_mailboxes(Monitored) ->
    maps:fold(fun(Name, Pid, Acc) ->
        case get_mailbox_length(Pid) of
            undefined -> Acc;
            Length -> maps:put(Name, Length, Acc)
        end
    end, #{}, Monitored).

get_mailbox_length(Pid) when is_pid(Pid) ->
    try
        case erlang:process_info(Pid, message_queue_len) of
            {message_queue_len, Len} -> Len;
            undefined -> undefined
        end
    catch
        _:_ -> undefined
    end.

auto_register_known_processes() ->
    %% Register well-known processes from iris_core
    KnownProcesses = [
        {iris_core, iris_core},
        {iris_shard, iris_shard},
        {iris_router_pool, iris_router_pool},
        {iris_flow_controller, iris_flow_controller},
        {iris_core_registry, iris_core_registry},
        {iris_presence, iris_presence},
        {iris_registry_ets, iris_registry_ets},
        {iris_metrics, iris_metrics}
    ],
    
    lists:foreach(fun({Name, RegName}) ->
        case whereis(RegName) of
            undefined -> ok;
            Pid -> 
                gen_server:cast(?SERVER, {register, Name, Pid})
        end
    end, KnownProcesses).
