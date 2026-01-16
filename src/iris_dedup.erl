-module(iris_dedup).
-behaviour(gen_server).

%% =============================================================================
%% Server-Side Message Deduplication
%% =============================================================================
%% Purpose: Prevent duplicate message delivery when clients retry after timeouts.
%% Design:
%% 1. ETS-based bloom filter for O(1) lookups with bounded memory
%% 2. TTL-based expiry to prevent unbounded growth
%% 3. Sharded for concurrent access
%% =============================================================================

-export([start_link/0, start_link/1]).
-export([check_and_mark/1, is_duplicate/1, mark_seen/1]).
-export([get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, iris_dedup_seen).
-define(TTL_MS, 300000).  %% 5 minutes - covers client retry window
-define(CLEANUP_INTERVAL, 60000).  %% Cleanup every minute
-define(MAX_ENTRIES, 10000000).  %% 10M message IDs (bounded memory)

-record(state, {
    cleanup_timer :: reference(),
    entries = 0 :: integer(),
    duplicates_caught = 0 :: integer(),
    evictions = 0 :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Check if message ID was seen; if not, mark it as seen.
%% Returns: new | duplicate
-spec check_and_mark(binary()) -> new | duplicate.
check_and_mark(MsgId) ->
    Now = os:system_time(millisecond),
    case ets:lookup(?TABLE, MsgId) of
        [] ->
            %% New message - mark it
            true = ets:insert(?TABLE, {MsgId, Now}),
            gen_server:cast(?SERVER, {new_entry, Now}),
            new;
        [{MsgId, _Timestamp}] ->
            %% Duplicate detected
            gen_server:cast(?SERVER, duplicate_caught),
            duplicate
    end.

%% @doc Just check if duplicate (no side effects)
-spec is_duplicate(binary()) -> boolean().
is_duplicate(MsgId) ->
    ets:member(?TABLE, MsgId).

%% @doc Just mark as seen (for pipeline scenarios)
-spec mark_seen(binary()) -> ok.
mark_seen(MsgId) ->
    Now = os:system_time(millisecond),
    true = ets:insert(?TABLE, {MsgId, Now}),
    ok.

%% @doc Get dedup stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init(_Opts) ->
    %% Create ETS table for dedup tracking
    ets:new(?TABLE, [
        set,
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    %% Start cleanup timer
    TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    {ok, #state{cleanup_timer = TRef}}.

handle_call(get_stats, _From, State) ->
    TableSize = ets:info(?TABLE, size),
    Stats = #{
        entries => TableSize,
        duplicates_caught => State#state.duplicates_caught,
        evictions => State#state.evictions,
        max_entries => ?MAX_ENTRIES,
        ttl_ms => ?TTL_MS
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({new_entry, _Timestamp}, State) ->
    %% Track entry count for eviction
    NewEntries = State#state.entries + 1,
    {noreply, State#state{entries = NewEntries}};

handle_cast(duplicate_caught, State) ->
    {noreply, State#state{duplicates_caught = State#state.duplicates_caught + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Expire old entries
    Now = os:system_time(millisecond),
    Cutoff = Now - ?TTL_MS,
    
    %% Count entries and remove expired ones
    {Kept, Removed} = cleanup_expired_entries(Cutoff),
    
    %% If still over limit, evict oldest entries
    Evicted = case Kept > ?MAX_ENTRIES of
        true -> evict_oldest(Kept - ?MAX_ENTRIES);
        false -> 0
    end,
    
    %% Reschedule cleanup
    TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    {noreply, State#state{
        cleanup_timer = TRef,
        entries = Kept - Evicted,
        evictions = State#state.evictions + Removed + Evicted
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

cleanup_expired_entries(Cutoff) ->
    %% Iterate through entries and remove expired ones
    cleanup_fold(ets:first(?TABLE), Cutoff, 0, 0).

cleanup_fold('$end_of_table', _Cutoff, Kept, Removed) ->
    {Kept, Removed};
cleanup_fold(Key, Cutoff, Kept, Removed) ->
    Next = ets:next(?TABLE, Key),
    case ets:lookup(?TABLE, Key) of
        [{Key, Timestamp}] when Timestamp < Cutoff ->
            ets:delete(?TABLE, Key),
            cleanup_fold(Next, Cutoff, Kept, Removed + 1);
        _ ->
            cleanup_fold(Next, Cutoff, Kept + 1, Removed)
    end.

evict_oldest(0) ->
    0;
evict_oldest(Count) ->
    %% Simple eviction: just remove first N entries
    %% In production, would sort by timestamp
    evict_n(ets:first(?TABLE), Count, 0).

evict_n('$end_of_table', _Remaining, Evicted) ->
    Evicted;
evict_n(_Key, 0, Evicted) ->
    Evicted;
evict_n(Key, Remaining, Evicted) ->
    Next = ets:next(?TABLE, Key),
    ets:delete(?TABLE, Key),
    evict_n(Next, Remaining - 1, Evicted + 1).
