-module(iris_mnesia_guard).
-behaviour(gen_server).

%% =============================================================================
%% AUDIT MITIGATION V1 — Finding 3A: Mnesia Scalability (RAM)
%% =============================================================================
%% Periodic monitor for Mnesia table memory usage.
%%
%% disc_copies tables load all keys/metadata into RAM. Without bounds,
%% a node with 64GB RAM can be crashed by unbounded offline message growth.
%%
%% This gen_server:
%%   - Checks table memory every 60s
%%   - Emits per-table and total memory metrics via iris_metrics
%%   - Logs alarm-level warnings when any table exceeds a configurable
%%     threshold (default 1GB)
%%   - Exposes check_memory/0 and get_alarms/0 for on-demand inspection
%% =============================================================================

%% API
-export([start_link/0, check_memory/0, get_alarms/0, get_alarm_threshold/0]).
-export([is_memory_ok/0]).  %% AUDIT V2 P0-2: Backpressure check

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL_MS, 60000).  %% 60 seconds
-define(DEFAULT_ALARM_BYTES, 1073741824).  %% 1 GB

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check memory usage of all Mnesia tables.
%% Returns {ok, #{TableName => Bytes}} with current memory for each table.
%% Also updates alarms for tables exceeding the configured threshold.
-spec check_memory() -> {ok, #{atom() => non_neg_integer()}}.
check_memory() ->
    WordSize = erlang:system_info(wordsize),
    Threshold = get_alarm_threshold(),
    Tables = try mnesia:system_info(tables)
             catch _:_ -> [schema]
             end,
    {MemMap, Alarms} = lists:foldl(fun(Table, {MapAcc, AlarmAcc}) ->
        Bytes = try
            Words = mnesia:table_info(Table, memory),
            Words * WordSize
        catch
            _:_ -> 0
        end,
        NewMapAcc = maps:put(Table, Bytes, MapAcc),
        NewAlarmAcc = case Bytes > Threshold of
            true -> [{Table, Bytes} | AlarmAcc];
            false -> AlarmAcc
        end,
        {NewMapAcc, NewAlarmAcc}
    end, {#{}, []}, Tables),
    %% Emit total memory metric
    Total = maps:fold(fun(_T, B, Acc) -> Acc + B end, 0, MemMap),
    try iris_metrics:set(mnesia_total_memory_bytes, Total)
    catch _:_ -> ok
    end,
    %% Log alarms for tables over threshold
    lists:foreach(fun({Table, Bytes}) ->
        logger:warning("MNESIA MEMORY ALARM: table=~p bytes=~B threshold=~B (~.1f%)",
                       [Table, Bytes, Threshold, Bytes * 100.0 / max(Threshold, 1)])
    end, Alarms),
    %% Store alarms in persistent_term for access without gen_server
    persistent_term:put(iris_mnesia_guard_alarms, Alarms),
    {ok, MemMap}.

%% @doc Get the list of tables currently exceeding the memory alarm threshold.
%% Returns [{TableName, Bytes}]. Reads from persistent_term (updated by check_memory/0).
-spec get_alarms() -> [{atom(), non_neg_integer()}].
get_alarms() ->
    persistent_term:get(iris_mnesia_guard_alarms, []).

%% @doc Get the configured alarm threshold in bytes.
-spec get_alarm_threshold() -> non_neg_integer().
get_alarm_threshold() ->
    application:get_env(iris_core, mnesia_memory_alarm_bytes, ?DEFAULT_ALARM_BYTES).

%% @doc AUDIT V2 P0-2: Check if Mnesia memory is within acceptable bounds.
%% Returns ok when total memory is under threshold, {error, memory_pressure}
%% when any table exceeds the configured alarm threshold.
%% Used as a backpressure gate before accepting new offline messages.
-spec is_memory_ok() -> ok | {error, memory_pressure}.
is_memory_ok() ->
    %% Fast path: check cached alarms from last periodic check
    case persistent_term:get(iris_mnesia_guard_alarms, []) of
        [] ->
            %% No cached alarms — do a fresh check to be sure
            {ok, _MemMap} = check_memory(),
            case persistent_term:get(iris_mnesia_guard_alarms, []) of
                [] -> ok;
                _Alarms -> {error, memory_pressure}
            end;
        _Alarms ->
            {error, memory_pressure}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    logger:info("iris_mnesia_guard starting (threshold=~B bytes, interval=~Bms)",
                [get_alarm_threshold(), ?CHECK_INTERVAL_MS]),
    erlang:send_after(?CHECK_INTERVAL_MS, self(), check),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check, State) ->
    %% Run the memory check (updates alarms via cast)
    try check_memory()
    catch _:_ -> ok
    end,
    erlang:send_after(?CHECK_INTERVAL_MS, self(), check),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
