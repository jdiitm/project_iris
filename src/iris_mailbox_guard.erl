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
