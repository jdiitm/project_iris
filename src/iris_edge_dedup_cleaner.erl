-module(iris_edge_dedup_cleaner).
-behaviour(gen_server).

%% Periodic cleanup for the iris_edge_dedup ETS table.
%% Prevents unbounded memory growth on edge nodes by evicting
%% entries older than TTL and enforcing a hard cap on table size.

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% Exported for direct testing of cleanup logic
-export([cleanup_now/0]).

-define(SERVER, ?MODULE).
-define(TABLE, iris_edge_dedup).
-define(TTL_MS, 300000).         %% 5 minutes
-define(MAX_ENTRIES, 1000000).   %% 1M entries
-define(CLEANUP_INTERVAL, 60000). %% 60 seconds

-record(state, {
    cleanup_timer :: reference(),
    last_removed = 0 :: non_neg_integer()
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {ok, #state{cleanup_timer = TRef}}.

handle_call(cleanup_now, _From, State) ->
    {Removed, NewState} = do_cleanup(State),
    {reply, {ok, Removed}, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    {_Removed, NewState} = do_cleanup(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Public: trigger cleanup synchronously (for testing)
-spec cleanup_now() -> {ok, non_neg_integer()}.
cleanup_now() ->
    gen_server:call(?SERVER, cleanup_now).

%% Internal cleanup logic
do_cleanup(State) ->
    case ets:info(?TABLE, name) of
        ?TABLE ->
            Now = os:system_time(millisecond),
            Cutoff = Now - ?TTL_MS,
            {_Kept, Removed} = cleanup_expired(ets:first(?TABLE), Cutoff, 0, 0),
            %% Cap enforcement: evict oldest if still over limit
            Size = ets:info(?TABLE, size),
            Evicted = case Size > ?MAX_ENTRIES of
                true  -> evict_oldest(Size - ?MAX_ENTRIES);
                false -> 0
            end,
            Total = Removed + Evicted,
            case Total > 0 of
                true ->
                    logger:info("edge_dedup cleanup: removed ~p expired, evicted ~p over cap", [Removed, Evicted]);
                false ->
                    ok
            end,
            TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
            {Total, State#state{cleanup_timer = TRef, last_removed = Total}};
        undefined ->
            %% Table does not exist (not on an edge node)
            TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
            {0, State#state{cleanup_timer = TRef, last_removed = 0}}
    end.

cleanup_expired('$end_of_table', _Cutoff, Kept, Removed) ->
    {Kept, Removed};
cleanup_expired(Key, Cutoff, Kept, Removed) ->
    Next = ets:next(?TABLE, Key),
    case ets:lookup(?TABLE, Key) of
        [{Key, Timestamp}] when Timestamp < Cutoff ->
            ets:delete(?TABLE, Key),
            cleanup_expired(Next, Cutoff, Kept, Removed + 1);
        _ ->
            cleanup_expired(Next, Cutoff, Kept + 1, Removed)
    end.

evict_oldest(0) -> 0;
evict_oldest(Count) ->
    evict_n(ets:first(?TABLE), Count, 0).

evict_n('$end_of_table', _Remaining, Evicted) ->
    Evicted;
evict_n(_Key, 0, Evicted) ->
    Evicted;
evict_n(Key, Remaining, Evicted) ->
    Next = ets:next(?TABLE, Key),
    ets:delete(?TABLE, Key),
    evict_n(Next, Remaining - 1, Evicted + 1).
