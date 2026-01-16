-module(iris_msg_batcher).
-behaviour(gen_server).

%% =============================================================================
%% Network Message Batching
%% =============================================================================
%% Purpose: Batch outgoing messages to reduce network overhead.
%% Design:
%% 1. Accumulate messages per destination for short window
%% 2. Flush on batch size or timeout
%% 3. Reduces TCP packet overhead and context switches
%% =============================================================================

-export([start_link/0]).
-export([send/3, send_immediate/3]).
-export([flush/1, flush_all/0]).
-export([get_stats/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(BATCH_SIZE, 50).           %% Max messages per batch
-define(BATCH_TIMEOUT_MS, 5).      %% Max wait before flush (5ms)
-define(MAX_PENDING, 10000).       %% Max pending across all destinations

-record(state, {
    batches = #{} :: map(),        %% Pid => {[Msgs], Timer, Size}
    total_pending = 0 :: integer(),
    batches_sent = 0 :: integer(),
    messages_batched = 0 :: integer(),
    messages_immediate = 0 :: integer()
}).

-record(batch, {
    messages = [] :: list(),
    timer :: reference() | undefined,
    size = 0 :: integer(),
    created :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Queue a message for batching (may delay up to BATCH_TIMEOUT_MS)
-spec send(pid(), term(), binary()) -> ok.
send(Pid, MsgType, Data) ->
    gen_server:cast(?SERVER, {queue, Pid, MsgType, Data}).

%% @doc Send immediately without batching (for urgent messages)
-spec send_immediate(pid(), term(), binary()) -> ok.
send_immediate(Pid, MsgType, Data) ->
    Pid ! {MsgType, Data},
    gen_server:cast(?SERVER, immediate_sent),
    ok.

%% @doc Flush pending messages for a specific destination
-spec flush(pid()) -> ok.
flush(Pid) ->
    gen_server:cast(?SERVER, {flush, Pid}).

%% @doc Flush all pending batches
-spec flush_all() -> ok.
flush_all() ->
    gen_server:call(?SERVER, flush_all).

%% @doc Get batching stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(flush_all, _From, State) ->
    NewState = do_flush_all(State),
    {reply, ok, NewState};

handle_call(get_stats, _From, State) ->
    Stats = #{
        pending_batches => maps:size(State#state.batches),
        total_pending => State#state.total_pending,
        batches_sent => State#state.batches_sent,
        messages_batched => State#state.messages_batched,
        messages_immediate => State#state.messages_immediate,
        efficiency => calculate_efficiency(State)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({queue, Pid, MsgType, Data}, State) ->
    NewState = add_to_batch(Pid, MsgType, Data, State),
    {noreply, NewState};

handle_cast({flush, Pid}, State) ->
    NewState = do_flush_pid(Pid, State),
    {noreply, NewState};

handle_cast(immediate_sent, State) ->
    {noreply, State#state{messages_immediate = State#state.messages_immediate + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({batch_timeout, Pid}, State) ->
    NewState = do_flush_pid(Pid, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Flush all on shutdown
    do_flush_all(State),
    ok.

%% =============================================================================
%% Internal: Batching Logic
%% =============================================================================

add_to_batch(Pid, MsgType, Data, State = #state{batches = Batches, total_pending = Total}) ->
    %% Check global limit
    case Total >= ?MAX_PENDING of
        true ->
            %% System under pressure - send immediately
            Pid ! {MsgType, Data},
            State#state{messages_immediate = State#state.messages_immediate + 1};
        false ->
            Batch = maps:get(Pid, Batches, #batch{created = os:system_time(millisecond)}),
            NewBatch = add_message(Batch, MsgType, Data, Pid),
            
            %% Check if batch is full
            case NewBatch#batch.size >= ?BATCH_SIZE of
                true ->
                    %% Flush immediately
                    flush_batch(Pid, NewBatch),
                    cancel_timer(NewBatch#batch.timer),
                    State#state{
                        batches = maps:remove(Pid, Batches),
                        total_pending = Total - NewBatch#batch.size + 1,
                        batches_sent = State#state.batches_sent + 1,
                        messages_batched = State#state.messages_batched + NewBatch#batch.size
                    };
                false ->
                    %% Store batch
                    NewBatches = maps:put(Pid, NewBatch, Batches),
                    State#state{
                        batches = NewBatches,
                        total_pending = Total + 1
                    }
            end
    end.

add_message(Batch = #batch{messages = Msgs, size = Size, timer = Timer}, MsgType, Data, Pid) ->
    %% Start timer if this is first message
    NewTimer = case Timer of
        undefined ->
            erlang:send_after(?BATCH_TIMEOUT_MS, self(), {batch_timeout, Pid});
        T -> T
    end,
    Batch#batch{
        messages = [{MsgType, Data} | Msgs],
        size = Size + 1,
        timer = NewTimer
    }.

flush_batch(Pid, #batch{messages = Msgs}) when length(Msgs) == 0 ->
    ok;
flush_batch(Pid, #batch{messages = Msgs}) when length(Msgs) == 1 ->
    %% Single message - send directly
    [{MsgType, Data}] = Msgs,
    Pid ! {MsgType, Data};
flush_batch(Pid, #batch{messages = Msgs}) ->
    %% Multiple messages - send as batch
    %% Reverse to maintain order
    OrderedMsgs = lists:reverse(Msgs),
    Pid ! {batch_msgs, OrderedMsgs}.

cancel_timer(undefined) -> ok;
cancel_timer(TRef) -> erlang:cancel_timer(TRef).

do_flush_pid(Pid, State = #state{batches = Batches}) ->
    case maps:get(Pid, Batches, undefined) of
        undefined ->
            State;
        Batch ->
            flush_batch(Pid, Batch),
            cancel_timer(Batch#batch.timer),
            State#state{
                batches = maps:remove(Pid, Batches),
                total_pending = State#state.total_pending - Batch#batch.size,
                batches_sent = State#state.batches_sent + 1,
                messages_batched = State#state.messages_batched + Batch#batch.size
            }
    end.

do_flush_all(State = #state{batches = Batches}) ->
    maps:fold(fun(Pid, Batch, _) ->
        flush_batch(Pid, Batch),
        cancel_timer(Batch#batch.timer)
    end, ok, Batches),
    
    TotalMsgs = lists:sum([B#batch.size || {_, B} <- maps:to_list(Batches)]),
    State#state{
        batches = #{},
        total_pending = 0,
        batches_sent = State#state.batches_sent + maps:size(Batches),
        messages_batched = State#state.messages_batched + TotalMsgs
    }.

calculate_efficiency(#state{messages_batched = 0}) ->
    0.0;
calculate_efficiency(#state{batches_sent = 0}) ->
    0.0;
calculate_efficiency(#state{batches_sent = Batches, messages_batched = Msgs}) ->
    %% Average messages per batch
    round((Msgs / Batches) * 10) / 10.
