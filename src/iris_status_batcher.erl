-module(iris_status_batcher).
-behaviour(gen_server).

-export([start_link/1, submit/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    id,
    buffer = #{} :: map(),
    count = 0 :: integer(),
    timer_ref
}).

-define(BATCH_SIZE, 1000).
-define(FLUSH_INTERVAL, 500).
-define(POOL_SIZE, 100).
%% H-5 AUDIT MITIGATION: Hard upper bound on buffer size.
%% Defense-in-depth against unbounded growth if flushes fail.
-define(MAX_BUFFER_SIZE, 10000).

%% API

start_link(Id) ->
    gen_server:start_link({local, name(Id)}, ?MODULE, [Id], []).

submit(User, Status) ->
    %% Hash user to worker
    WorkerId = erlang:phash2(User, ?POOL_SIZE),
    gen_server:cast(name(WorkerId), {update, User, Status}).

name(Id) ->
    list_to_atom("iris_status_batcher_" ++ integer_to_list(Id)).

%% Callbacks

init([Id]) ->
    process_flag(trap_exit, true),
    %% Don't start timer immediately, wait for first item? No, keep it simple.
    TRef = erlang:send_after(?FLUSH_INTERVAL, self(), flush),
    {ok, #state{id = Id, timer_ref = TRef}}.

handle_cast({update, User, _Status}, State = #state{buffer = Buff, count = Count}) ->
    %% H-5 AUDIT MITIGATION: Drop items when buffer exceeds MAX_BUFFER_SIZE
    if Count >= ?MAX_BUFFER_SIZE ->
        logger:warning("~p: buffer full (~p items), dropping update for ~p",
                       [?MODULE, Count, User]),
        {noreply, State};
    true ->
        Timestamp = os:system_time(seconds),
        NewBuff = maps:put(User, Timestamp, Buff),
        NewCount = Count + 1,
        
        if NewCount >= ?BATCH_SIZE ->
            flush(NewBuff),
            {noreply, reset_state(State)};
        true ->
            {noreply, State#state{buffer = NewBuff, count = NewCount}}
        end
    end.

%% B-7 AUDIT MITIGATION: Handle EXIT signals from linked processes.
%% Without this, EXIT messages cause gen_server termination because trap_exit
%% converts them to messages but no clause handles them.
handle_info({'EXIT', Pid, Reason}, State) ->
    logger:warning("~p received EXIT from ~p: ~p", [?MODULE, Pid, Reason]),
    {noreply, State};

handle_info(flush, State = #state{buffer = Buff, count = Count}) ->
    if Count > 0 ->
        flush(Buff);
    true -> ok
    end,
    {noreply, reset_state(State)}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, #state{buffer = Buff, count = Count}) ->
    if Count > 0 ->
        io:format("Batcher terminating. Flushing ~p items...~n", [Count]),
        flush(Buff);
    true -> ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

flush(Buffer) ->
    %% Bulk Write to Mnesia
    F = fun() ->
        maps:fold(fun(User, Timestamp, _) ->
            mnesia:write({user_status, User, Timestamp})
        end, ok, Buffer)
    end,
    case mnesia:activity(sync_transaction, F) of
        ok -> ok;
        {atomic, ok} -> ok;
        {atomic, Result} -> io:format("Batch Write Result: ~p~n", [Result]);
        {aborted, Reason} -> io:format("Batch Write Aborted: ~p~n", [Reason]);
        Err -> io:format("Batch Write Error: ~p~n", [Err])
    end.

reset_state(State) ->
    erlang:cancel_timer(State#state.timer_ref),
    TRef = erlang:send_after(?FLUSH_INTERVAL, self(), flush),
    State#state{buffer = #{}, count = 0, timer_ref = TRef}.
