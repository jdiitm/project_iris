-module(iris_mailbox_guard).

%% =============================================================================
%% Bounded Mailbox Protection
%% =============================================================================
%% Purpose: Monitor and protect process mailboxes from overflow.
%% Design:
%% 1. Periodic monitoring of critical process mailboxes
%% 2. Warning and action thresholds
%% 3. Overflow handling: message shedding or process restart
%% =============================================================================

-behaviour(gen_server).

-export([start_link/0]).
-export([register_process/2, register_process/3]).
-export([unregister_process/1]).
-export([check_mailbox/1]).
-export([get_stats/0]).

%% AQM: CoDel (Controlled Delay) algorithm -- pure functions for unit testing
-export([codel_new/0, codel_new/1, codel_check/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL_MS, 5000).
-define(DEFAULT_WARNING_THRESHOLD, 1000).
-define(DEFAULT_CRITICAL_THRESHOLD, 10000).
-define(DEFAULT_DROP_THRESHOLD, 50000).

-record(process_config, {
    pid :: pid(),
    name :: atom() | binary(),
    warning_threshold :: integer(),
    critical_threshold :: integer(),
    drop_threshold :: integer(),
    on_critical :: shed | pause | restart,
    last_warning :: integer() | undefined
}).

-record(state, {
    processes = #{} :: map(),  %% Pid => #process_config{}
    check_timer :: reference(),
    warnings_issued = 0 :: integer(),
    drops_performed = 0 :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a process for mailbox monitoring
-spec register_process(pid(), atom() | binary()) -> ok.
register_process(Pid, Name) ->
    register_process(Pid, Name, #{}).

-spec register_process(pid(), atom() | binary(), map()) -> ok.
register_process(Pid, Name, Opts) ->
    gen_server:cast(?SERVER, {register, Pid, Name, Opts}).

%% @doc Unregister a process from monitoring
-spec unregister_process(pid()) -> ok.
unregister_process(Pid) ->
    gen_server:cast(?SERVER, {unregister, Pid}).

%% @doc Check a specific process's mailbox
-spec check_mailbox(pid()) -> {ok, integer()} | {warning, integer()} | {critical, integer()}.
check_mailbox(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Len} ->
            if
                Len > ?DEFAULT_CRITICAL_THRESHOLD -> {critical, Len};
                Len > ?DEFAULT_WARNING_THRESHOLD -> {warning, Len};
                true -> {ok, Len}
            end;
        undefined ->
            {ok, 0}
    end.

%% @doc Get monitoring stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    TRef = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_all),
    {ok, #state{check_timer = TRef}}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        monitored_processes => maps:size(State#state.processes),
        warnings_issued => State#state.warnings_issued,
        drops_performed => State#state.drops_performed,
        process_details => get_process_details(State#state.processes)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register, Pid, Name, Opts}, State = #state{processes = Procs}) ->
    Config = #process_config{
        pid = Pid,
        name = Name,
        warning_threshold = maps:get(warning_threshold, Opts, ?DEFAULT_WARNING_THRESHOLD),
        critical_threshold = maps:get(critical_threshold, Opts, ?DEFAULT_CRITICAL_THRESHOLD),
        drop_threshold = maps:get(drop_threshold, Opts, ?DEFAULT_DROP_THRESHOLD),
        on_critical = maps:get(on_critical, Opts, shed)
    },
    %% Monitor the process
    erlang:monitor(process, Pid),
    NewProcs = maps:put(Pid, Config, Procs),
    {noreply, State#state{processes = NewProcs}};

handle_cast({unregister, Pid}, State = #state{processes = Procs}) ->
    NewProcs = maps:remove(Pid, Procs),
    {noreply, State#state{processes = NewProcs}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_all, State) ->
    NewState = check_all_processes(State),
    TRef = erlang:send_after(?CHECK_INTERVAL_MS, self(), check_all),
    {noreply, NewState#state{check_timer = TRef}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{processes = Procs}) ->
    NewProcs = maps:remove(Pid, Procs),
    {noreply, State#state{processes = NewProcs}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal: Mailbox Checking
%% =============================================================================

check_all_processes(State = #state{processes = Procs}) ->
    {NewProcs, Warnings, Drops} = maps:fold(fun(Pid, Config, {AccProcs, AccWarn, AccDrop}) ->
        case check_process_mailbox(Pid, Config) of
            {ok, NewConfig} ->
                {maps:put(Pid, NewConfig, AccProcs), AccWarn, AccDrop};
            {warning, NewConfig} ->
                {maps:put(Pid, NewConfig, AccProcs), AccWarn + 1, AccDrop};
            {critical, NewConfig, DropsPerformed} ->
                {maps:put(Pid, NewConfig, AccProcs), AccWarn + 1, AccDrop + DropsPerformed};
            dead ->
                {AccProcs, AccWarn, AccDrop}
        end
    end, {#{}, 0, 0}, Procs),
    
    State#state{
        processes = NewProcs,
        warnings_issued = State#state.warnings_issued + Warnings,
        drops_performed = State#state.drops_performed + Drops
    }.

check_process_mailbox(Pid, Config = #process_config{
    name = Name,
    warning_threshold = WarnThreshold,
    critical_threshold = CritThreshold,
    drop_threshold = DropThreshold,
    on_critical = OnCritical
}) ->
    case process_info(Pid, message_queue_len) of
        undefined ->
            dead;
        {message_queue_len, Len} when Len > DropThreshold ->
            %% Emergency: Drop messages
            logger:error("Mailbox EMERGENCY for ~p (~p): ~p msgs (threshold: ~p)", 
                        [Name, Pid, Len, DropThreshold]),
            Dropped = perform_emergency_drop(Pid, Len, DropThreshold),
            Now = os:system_time(millisecond),
            {critical, Config#process_config{last_warning = Now}, Dropped};
        {message_queue_len, Len} when Len > CritThreshold ->
            %% Critical: Take action
            Now = os:system_time(millisecond),
            case should_warn(Config, Now) of
                true ->
                    logger:warning("Mailbox CRITICAL for ~p (~p): ~p msgs", [Name, Pid, Len]),
                    handle_critical(Pid, OnCritical),
                    {critical, Config#process_config{last_warning = Now}, 0};
                false ->
                    {critical, Config, 0}
            end;
        {message_queue_len, Len} when Len > WarnThreshold ->
            %% Warning
            Now = os:system_time(millisecond),
            case should_warn(Config, Now) of
                true ->
                    logger:warning("Mailbox warning for ~p (~p): ~p msgs", [Name, Pid, Len]),
                    iris_backpressure:signal_overload(),
                    {warning, Config#process_config{last_warning = Now}};
                false ->
                    {ok, Config}
            end;
        {message_queue_len, _Len} ->
            {ok, Config}
    end.

should_warn(#process_config{last_warning = undefined}, _Now) ->
    true;
should_warn(#process_config{last_warning = Last}, Now) ->
    %% Only warn every 10 seconds
    Now - Last > 10000.

handle_critical(_Pid, shed) ->
    %% Signal backpressure - message shedding handled by flow controller
    iris_backpressure:signal_overload(),
    ok;
handle_critical(_Pid, pause) ->
    %% Could suspend the process temporarily
    iris_backpressure:signal_overload(),
    ok;
handle_critical(Pid, restart) ->
    %% Restart via supervisor (last resort)
    logger:error("Mailbox guard requesting restart of ~p", [Pid]),
    exit(Pid, mailbox_overflow).

perform_emergency_drop(Pid, CurrentLen, TargetLen) ->
    %% Drop oldest messages until under threshold
    %% This is aggressive but prevents OOM crashes
    ToDrop = CurrentLen - TargetLen,
    drop_messages(Pid, ToDrop, 0).

drop_messages(_Pid, 0, Dropped) ->
    Dropped;
drop_messages(Pid, Remaining, Dropped) ->
    %% Send a message to the process to drop its oldest message
    %% The process needs to handle this
    Pid ! {iris_mailbox_guard, drop_oldest},
    drop_messages(Pid, Remaining - 1, Dropped + 1).

get_process_details(Procs) ->
    maps:fold(fun(Pid, #process_config{name = Name}, Acc) ->
        Len = case process_info(Pid, message_queue_len) of
            {message_queue_len, L} -> L;
            undefined -> 0
        end,
        maps:put(Name, #{pid => Pid, queue_len => Len}, Acc)
    end, #{}, Procs).

%% =============================================================================
%% CoDel (Controlled Delay) Active Queue Management
%% =============================================================================
%% Reference: Nichols & Jacobson, "Controlling Queue Delay" (2012)
%%
%% CoDel tracks the MINIMUM sojourn time (time a message sits in the queue)
%% over a sliding interval. If the minimum sojourn stays above a target for
%% an entire interval, CoDel enters dropping mode. Drop rate increases with
%% 1/sqrt(count) to converge quickly. Dropping stops immediately when sojourn
%% drops below target.
%%
%% Pure functions: codel_new/0,1 and codel_check/3 are stateless and testable.
%% =============================================================================

%% @doc Create a new CoDel state with default parameters.
-spec codel_new() -> map().
codel_new() ->
    codel_new(#{}).

%% @doc Create a new CoDel state with custom parameters.
%% Options: target_ms (default 5), interval_ms (default 100).
-spec codel_new(map()) -> map().
codel_new(Opts) ->
    #{
        target_ms      => maps:get(target_ms, Opts, 5),
        interval_ms    => maps:get(interval_ms, Opts, 100),
        first_above_time => 0,      %% When sojourn first exceeded target (0 = not tracking)
        drop_next      => 0,        %% Next scheduled drop time
        drop_count     => 0,        %% Consecutive drops in current episode
        dropping       => false     %% Currently in dropping mode?
    }.

%% @doc CoDel check: given current sojourn time and wall clock, decide ok or drop.
%% SojournMs: how long the head-of-line message has been in the queue (milliseconds).
%% NowMs: current monotonic time in milliseconds.
%% Returns {ok, NewState} or {drop, NewState}.
-spec codel_check(non_neg_integer(), non_neg_integer(), map()) -> {ok | drop, map()}.
codel_check(SojournMs, NowMs, State) ->
    #{target_ms := Target, interval_ms := Interval,
      first_above_time := FirstAbove,
      dropping := Dropping} = State,

    case SojournMs < Target of
        true ->
            %% Below target: reset tracking, exit dropping mode
            State1 = State#{first_above_time => 0, dropping => false},
            {ok, State1};
        false ->
            %% Above target
            State1 = case FirstAbove of
                0 ->
                    %% First time above target in this window: start tracking
                    State#{first_above_time => NowMs + Interval};
                _ ->
                    State
            end,
            case Dropping of
                true ->
                    %% Already dropping: check if it's time for the next drop
                    codel_dropping(NowMs, State1);
                false ->
                    %% Not dropping yet: check if we've been above target for a full interval
                    codel_not_dropping(NowMs, State1)
            end
    end.

%% In non-dropping state: enter dropping if above target for full interval
codel_not_dropping(NowMs, State = #{first_above_time := FirstAbove}) ->
    case NowMs >= FirstAbove andalso FirstAbove > 0 of
        true ->
            %% Been above target for a full interval: enter dropping mode
            #{interval_ms := Interval} = State,
            State1 = State#{
                dropping => true,
                drop_count => 1,
                drop_next => NowMs + codel_control_law(Interval, 1)
            },
            {drop, State1};
        false ->
            {ok, State}
    end.

%% In dropping state: schedule drops at 1/sqrt(count) intervals
codel_dropping(NowMs, State = #{drop_next := DropNext, drop_count := Count,
                                 interval_ms := Interval}) ->
    case NowMs >= DropNext of
        true ->
            %% Time for next drop
            NewCount = Count + 1,
            State1 = State#{
                drop_count => NewCount,
                drop_next => NowMs + codel_control_law(Interval, NewCount)
            },
            {drop, State1};
        false ->
            %% Not yet time for next drop
            {ok, State}
    end.

%% CoDel control law: interval / sqrt(count)
%% This makes drops increasingly aggressive as the queue stays full.
codel_control_law(Interval, Count) ->
    round(Interval / math:sqrt(Count)).
