-module(iris_msg_sequence).
-behaviour(gen_server).

%% =============================================================================
%% Message Sequence Tracker: FIFO Ordering Enforcement (RFC FR-5)
%% =============================================================================
%% This module tracks sequence numbers per (sender, recipient) pair to ensure
%% FIFO message delivery across distributed nodes.
%%
%% Design:
%% - Each sender maintains a monotonic counter per recipient
%% - Receivers track last-seen sequence per sender
%% - Out-of-order messages are logged but delivered (hardened AP)
%% - Duplicates (same sequence) are dropped
%%
%% RFC Compliance:
%% - FR-5: Messages between two users MUST be delivered in order
%% - Hardened AP: We detect and log violations but don't block delivery
%% =============================================================================

-export([start_link/0]).
-export([next_sequence/2, validate_sequence/3, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(SENDER_TABLE, iris_msg_seq_sender).    %% Tracks outgoing sequences
-define(RECEIVER_TABLE, iris_msg_seq_receiver). %% Tracks incoming sequences
-define(CLEANUP_INTERVAL, 300000).  %% Cleanup idle pairs every 5 minutes
-define(IDLE_TIMEOUT, 3600000).     %% Remove pairs idle for 1 hour

-record(state, {
    cleanup_timer :: reference(),
    out_of_order_count = 0 :: integer(),
    duplicate_count = 0 :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get next sequence number for (sender, recipient) pair
%% Called by sender when creating a message
-spec next_sequence(binary(), binary()) -> integer().
next_sequence(Sender, Recipient) ->
    Key = {Sender, Recipient},
    %% Atomically increment and return new value
    ets:update_counter(?SENDER_TABLE, Key, {2, 1}, {Key, 0}).

%% @doc Validate sequence number for incoming message
%% Returns: {ok, in_order} | {ok, out_of_order, Gap} | {duplicate, SeqNo}
-spec validate_sequence(binary(), binary(), integer()) -> 
    {ok, in_order} | {ok, out_of_order, integer()} | {duplicate, integer()}.
validate_sequence(Sender, Recipient, SeqNo) ->
    Key = {Sender, Recipient},
    Now = os:system_time(millisecond),
    
    case ets:lookup(?RECEIVER_TABLE, Key) of
        [] ->
            %% First message from this sender - accept and record
            ets:insert(?RECEIVER_TABLE, {Key, SeqNo, Now}),
            {ok, in_order};
        
        [{Key, LastSeq, _LastTime}] when SeqNo =:= LastSeq ->
            %% Duplicate message
            gen_server:cast(?SERVER, duplicate),
            {duplicate, SeqNo};
        
        [{Key, LastSeq, _LastTime}] when SeqNo =:= LastSeq + 1 ->
            %% Perfect: next in sequence
            ets:insert(?RECEIVER_TABLE, {Key, SeqNo, Now}),
            {ok, in_order};
        
        [{Key, LastSeq, _LastTime}] when SeqNo > LastSeq + 1 ->
            %% Gap detected - some messages may have been lost
            Gap = SeqNo - LastSeq - 1,
            logger:warning("Sequence gap detected: sender=~p, recipient=~p, expected=~p, got=~p, gap=~p",
                          [Sender, Recipient, LastSeq + 1, SeqNo, Gap]),
            gen_server:cast(?SERVER, {out_of_order, Gap}),
            ets:insert(?RECEIVER_TABLE, {Key, SeqNo, Now}),
            {ok, out_of_order, Gap};
        
        [{Key, LastSeq, _LastTime}] when SeqNo < LastSeq ->
            %% Old message received after newer one (reordering)
            Gap = LastSeq - SeqNo,
            logger:warning("Out-of-order message: sender=~p, recipient=~p, current=~p, received=~p",
                          [Sender, Recipient, LastSeq, SeqNo]),
            gen_server:cast(?SERVER, {out_of_order, Gap}),
            %% Don't update LastSeq - keep the highest seen
            {ok, out_of_order, -Gap}
    end.

%% @doc Get sequence tracking stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% Create ETS tables for sequence tracking
    ets:new(?SENDER_TABLE, [
        set,
        named_table,
        public,
        {write_concurrency, true}
    ]),
    
    ets:new(?RECEIVER_TABLE, [
        set,
        named_table,
        public,
        {write_concurrency, true}
    ]),
    
    %% Start cleanup timer
    TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    logger:info("Message Sequence Tracker started"),
    {ok, #state{cleanup_timer = TRef}}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        sender_pairs => ets:info(?SENDER_TABLE, size),
        receiver_pairs => ets:info(?RECEIVER_TABLE, size),
        out_of_order_count => State#state.out_of_order_count,
        duplicate_count => State#state.duplicate_count
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(duplicate, State) ->
    {noreply, State#state{duplicate_count = State#state.duplicate_count + 1}};

handle_cast({out_of_order, _Gap}, State) ->
    {noreply, State#state{out_of_order_count = State#state.out_of_order_count + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Remove idle sender/receiver pairs
    Now = os:system_time(millisecond),
    Cutoff = Now - ?IDLE_TIMEOUT,
    
    %% Cleanup receiver table (has timestamps)
    cleanup_receiver_table(Cutoff),
    
    %% Sender table doesn't have timestamps - could add LRU eviction
    %% For now, just report size
    SenderSize = ets:info(?SENDER_TABLE, size),
    ReceiverSize = ets:info(?RECEIVER_TABLE, size),
    
    case SenderSize + ReceiverSize > 0 of
        true ->
            logger:debug("Sequence tracker: ~p sender pairs, ~p receiver pairs",
                        [SenderSize, ReceiverSize]);
        false -> ok
    end,
    
    %% Reschedule
    TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {noreply, State#state{cleanup_timer = TRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

cleanup_receiver_table(Cutoff) ->
    cleanup_fold(ets:first(?RECEIVER_TABLE), Cutoff, 0).

cleanup_fold('$end_of_table', _Cutoff, Removed) ->
    Removed;
cleanup_fold(Key, Cutoff, Removed) ->
    Next = ets:next(?RECEIVER_TABLE, Key),
    case ets:lookup(?RECEIVER_TABLE, Key) of
        [{Key, _SeqNo, LastTime}] when LastTime < Cutoff ->
            ets:delete(?RECEIVER_TABLE, Key),
            cleanup_fold(Next, Cutoff, Removed + 1);
        _ ->
            cleanup_fold(Next, Cutoff, Removed)
    end.
