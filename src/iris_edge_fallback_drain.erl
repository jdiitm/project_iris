-module(iris_edge_fallback_drain).
-behaviour(gen_server).

%% =============================================================================
%% B-1: Edge Fallback Drain Server
%% =============================================================================
%% Periodically drains messages from iris_edge_pending_offline ETS table
%% to core nodes. Messages are forwarded in batches and only deleted from
%% ETS on successful delivery. Failed messages remain for retry.
%% =============================================================================

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    forward_fn :: fun((binary(), binary()) -> ok | {error, term()}),
    drain_interval_ms :: pos_integer(),
    batch_size :: pos_integer(),
    timer_ref :: reference() | undefined
}).

-define(DEFAULT_DRAIN_INTERVAL_MS, 5000).
-define(DEFAULT_BATCH_SIZE, 100).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    start_link(#{}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(Opts) ->
    ForwardFn = maps:get(forward_fn, Opts, fun default_forward/2),
    IntervalMs = maps:get(drain_interval_ms, Opts,
        application:get_env(iris_edge, edge_fallback_drain_interval_ms,
                            ?DEFAULT_DRAIN_INTERVAL_MS)),
    BatchSize = maps:get(batch_size, Opts, ?DEFAULT_BATCH_SIZE),

    %% Ensure the ETS table exists
    case ets:whereis(iris_edge_pending_offline) of
        undefined ->
            ets:new(iris_edge_pending_offline, [named_table, public, bag]);
        _ -> ok
    end,

    TRef = erlang:send_after(IntervalMs, self(), drain_tick),
    {ok, #state{
        forward_fn = ForwardFn,
        drain_interval_ms = IntervalMs,
        batch_size = BatchSize,
        timer_ref = TRef
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(drain_tick, State = #state{drain_interval_ms = IntervalMs}) ->
    NewState = do_drain(State),
    %% Emit pending gauge metric
    emit_pending_gauge(),
    TRef = erlang:send_after(IntervalMs, self(), drain_tick),
    {noreply, NewState#state{timer_ref = TRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer_ref = TRef}) ->
    case TRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TRef)
    end,
    ok.

%% =============================================================================
%% Internal
%% =============================================================================

do_drain(State = #state{forward_fn = ForwardFn, batch_size = BatchSize}) ->
    case ets:whereis(iris_edge_pending_offline) of
        undefined -> State;
        _ ->
            %% Get a batch of entries
            Entries = get_batch(BatchSize),
            drain_entries(Entries, ForwardFn),
            State
    end.

get_batch(BatchSize) ->
    try
        %% Use ets:match to get entries, limit by BatchSize
        All = ets:match_object(iris_edge_pending_offline, {'_', '_', '_'}),
        lists:sublist(All, BatchSize)
    catch
        _:_ -> []
    end.

drain_entries([], _ForwardFn) ->
    ok;
drain_entries([{User, Msg, _Ts} = Entry | Rest], ForwardFn) ->
    case ForwardFn(User, Msg) of
        ok ->
            %% Successfully forwarded — delete this specific entry
            ets:delete_object(iris_edge_pending_offline, Entry),
            drain_entries(Rest, ForwardFn);
        {error, _Reason} ->
            %% Failed — leave in ETS for retry next tick
            iris_metrics:inc(edge_fallback_drain_failures),
            ok
    end.

emit_pending_gauge() ->
    Size = case ets:whereis(iris_edge_pending_offline) of
        undefined -> 0;
        _ -> ets:info(iris_edge_pending_offline, size)
    end,
    iris_metrics:set(edge_fallback_pending, Size).

default_forward(User, Msg) ->
    %% Default: attempt RPC to any available core node
    CoreNodes = application:get_env(iris_edge, core_nodes, []),
    forward_to_core(CoreNodes, User, Msg).

forward_to_core([], _User, _Msg) ->
    {error, no_core_available};
forward_to_core([Node | Rest], User, Msg) ->
    case rpc:call(Node, iris_core, store_offline_durable, [User, [Msg]], 5000) of
        {badrpc, _} -> forward_to_core(Rest, User, Msg);
        ok -> ok;
        _ -> forward_to_core(Rest, User, Msg)
    end.
