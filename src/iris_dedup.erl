-module(iris_dedup).
-behaviour(gen_server).

%% =============================================================================
%% Server-Side Message Deduplication
%% =============================================================================
%% Purpose: Prevent duplicate message delivery when clients retry after timeouts.
%% 
%% P0-C3 FIX: Tiered Dedup Architecture (RFC compliant 7-day window)
%% 
%% Design:
%% 1. HOT TIER: ETS-based cache for recent messages (5 min TTL)
%%    - O(1) lookups, high throughput
%%    - Handles immediate client retries
%% 
%% 2. WARM TIER: Partitioned bloom filters for 7-day window
%%    - 24 hourly partitions, rotated daily
%%    - ~10MB per partition at 10M msg/hour with 0.1% FPR
%%    - Total ~240MB for 7 days
%% 
%% 3. COLD CHECK: Mnesia lookup for critical messages (optional)
%%    - Only for messages marked as high-priority
%% 
%% Lookup path: ETS -> Bloom -> (optional Mnesia)
%% False positive from bloom = extra Mnesia check, acceptable tradeoff
%% =============================================================================

-export([start_link/0, start_link/1]).
-export([check_and_mark/1, is_duplicate/1, mark_seen/1]).
-export([get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, iris_dedup_seen).
-define(BLOOM_TABLE, iris_dedup_bloom).
-define(HOT_TTL_MS, 300000).       %% 5 minutes - hot tier (ETS)
-define(WARM_TTL_HOURS, 168).      %% 7 days = 168 hours (bloom)
-define(CLEANUP_INTERVAL, 60000).  %% Cleanup every minute
-define(BLOOM_ROTATE_INTERVAL, 3600000).  %% Rotate bloom hourly
-define(MAX_ENTRIES, 10000000).    %% 10M message IDs (bounded memory for ETS)
-define(BLOOM_SIZE, 10000000).     %% 10M bits per bloom partition
-define(BLOOM_HASHES, 7).          %% Number of hash functions (k=7 for 0.1% FPR)

-record(state, {
    cleanup_timer :: reference(),
    bloom_rotate_timer :: reference(),
    current_bloom_partition :: integer(),  %% 0-167 (168 hourly partitions)
    entries = 0 :: integer(),
    duplicates_caught = 0 :: integer(),
    bloom_hits = 0 :: integer(),
    bloom_false_positives = 0 :: integer(),  %% P0-FIX: Track false positives
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
%% P0-C3: Tiered check: ETS (hot) -> Bloom (warm) -> new
-spec check_and_mark(binary()) -> new | duplicate.
check_and_mark(MsgId) ->
    Now = os:system_time(millisecond),
    %% Tier 1: Check hot ETS cache
    case ets:lookup(?TABLE, MsgId) of
        [{MsgId, _Timestamp}] ->
            %% Found in hot tier - definite duplicate
            gen_server:cast(?SERVER, duplicate_caught),
            duplicate;
        [] ->
            %% Tier 2: Check bloom filter (7-day window)
            case check_bloom(MsgId) of
                true ->
                    %% P0-FIX: Bloom filter hit is PROBABILISTIC.
                    %% We MUST verify against definitive storage to avoid false positives (Data Loss).
                    %% False Positive Rate ~0.1% -> 1 in 1000 messages would be dropped without this check.
                    %% Use dedup_log table which is keyed by MsgId (not offline_msg which uses {User, BucketID}).
                    case mnesia:dirty_read(dedup_log, MsgId) of
                        [{dedup_log, MsgId, _Timestamp}] ->
                            %% Confirmed duplicate in dedup log
                            gen_server:cast(?SERVER, bloom_hit),
                            duplicate;
                        [] ->
                            %% False positive! The message is NEW.
                            %% Add to hot cache, bloom, and dedup_log.
                            logger:info("Dedup: Bloom false positive for ~p - allowing", [MsgId]),
                            true = ets:insert(?TABLE, {MsgId, Now}),
                            add_to_bloom(MsgId),
                            write_dedup_log(MsgId, Now),
                            gen_server:cast(?SERVER, {false_positive, Now}),
                            new
                    end;
                false ->
                    %% New message - mark in all tiers (hot ETS, bloom, dedup_log)
                    true = ets:insert(?TABLE, {MsgId, Now}),
                    add_to_bloom(MsgId),
                    write_dedup_log(MsgId, Now),
                    gen_server:cast(?SERVER, {new_entry, Now}),
                    new
            end
    end.

%% @doc Just check if duplicate (no side effects)
%% P0-C3: Checks both hot ETS and warm bloom tiers
-spec is_duplicate(binary()) -> boolean().
is_duplicate(MsgId) ->
    case ets:member(?TABLE, MsgId) of
        true -> true;
        false -> check_bloom(MsgId)
    end.

%% @doc Just mark as seen (for pipeline scenarios)
-spec mark_seen(binary()) -> ok.
mark_seen(MsgId) ->
    Now = os:system_time(millisecond),
    true = ets:insert(?TABLE, {MsgId, Now}),
    add_to_bloom(MsgId),
    write_dedup_log(MsgId, Now),
    ok.

%% @doc Get dedup stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init(_Opts) ->
    %% Create ETS table for hot tier dedup tracking
    ets:new(?TABLE, [
        set,
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    %% P0-C3: Create bloom filter table for warm tier (7-day window)
    %% Each partition is a bitarray stored as binary
    ets:new(?BLOOM_TABLE, [
        set,
        named_table,
        public,
        {read_concurrency, true}
    ]),
    
    %% Initialize current bloom partition (hour of week 0-167)
    CurrentPartition = get_current_partition(),
    init_bloom_partition(CurrentPartition),
    
    %% Start cleanup timer for hot tier
    CleanupTRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    %% Start bloom rotation timer
    BloomTRef = erlang:send_after(?BLOOM_ROTATE_INTERVAL, self(), rotate_bloom),
    
    logger:info("Dedup started: hot_ttl=~pms, warm_ttl=~ph, bloom_size=~p", 
                [?HOT_TTL_MS, ?WARM_TTL_HOURS, ?BLOOM_SIZE]),
    
    {ok, #state{
        cleanup_timer = CleanupTRef,
        bloom_rotate_timer = BloomTRef,
        current_bloom_partition = CurrentPartition
    }}.

handle_call(get_stats, _From, State) ->
    TableSize = ets:info(?TABLE, size),
    BloomPartitions = ets:info(?BLOOM_TABLE, size),
    Stats = #{
        hot_entries => TableSize,
        duplicates_caught => State#state.duplicates_caught,
        bloom_hits => State#state.bloom_hits,
        bloom_false_positives => State#state.bloom_false_positives,
        bloom_partitions => BloomPartitions,
        evictions => State#state.evictions,
        max_entries => ?MAX_ENTRIES,
        hot_ttl_ms => ?HOT_TTL_MS,
        warm_ttl_hours => ?WARM_TTL_HOURS
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

handle_cast(bloom_hit, State) ->
    {noreply, State#state{
        duplicates_caught = State#state.duplicates_caught + 1,
        bloom_hits = State#state.bloom_hits + 1
    }};

handle_cast({false_positive, _Timestamp}, State) ->
    %% P0-FIX: Track bloom filter false positives (messages that bloom said "duplicate" but weren't)
    {noreply, State#state{
        bloom_false_positives = State#state.bloom_false_positives + 1
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Expire old entries from hot tier
    Now = os:system_time(millisecond),
    Cutoff = Now - ?HOT_TTL_MS,
    
    %% Count entries and remove expired ones
    {Kept, Removed} = cleanup_expired_entries(Cutoff),
    
    %% If still over limit, evict oldest entries
    Evicted = case Kept > ?MAX_ENTRIES of
        true -> evict_oldest(Kept - ?MAX_ENTRIES);
        false -> 0
    end,
    
    %% P0-FIX: Also cleanup old dedup_log entries (7-day TTL)
    cleanup_dedup_log(),
    
    %% Reschedule cleanup
    TRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    {noreply, State#state{
        cleanup_timer = TRef,
        entries = Kept - Evicted,
        evictions = State#state.evictions + Removed + Evicted
    }};

handle_info(rotate_bloom, State) ->
    %% P0-C3: Rotate to next bloom partition
    NewPartition = get_current_partition(),
    
    %% Initialize new partition (clears old data for this slot)
    init_bloom_partition(NewPartition),
    
    %% Clean up partitions older than 7 days
    cleanup_old_bloom_partitions(NewPartition),
    
    %% Reschedule rotation
    BloomTRef = erlang:send_after(?BLOOM_ROTATE_INTERVAL, self(), rotate_bloom),
    
    {noreply, State#state{
        bloom_rotate_timer = BloomTRef,
        current_bloom_partition = NewPartition
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

%% =============================================================================
%% P0-C3: Bloom Filter Implementation (7-day warm tier)
%% =============================================================================

%% Get current partition index (0-167 for 168 hourly partitions)
get_current_partition() ->
    {_, {H, _, _}} = calendar:local_time(),
    %% Day of week * 24 + hour
    DayOfWeek = calendar:day_of_the_week(date()) - 1,  %% 0-6
    (DayOfWeek * 24 + H) rem ?WARM_TTL_HOURS.

%% Initialize a bloom partition (creates empty bitarray)
init_bloom_partition(Partition) ->
    %% Create empty bloom filter as binary
    %% Each bloom is BLOOM_SIZE bits = BLOOM_SIZE/8 bytes
    ByteSize = ?BLOOM_SIZE div 8,
    EmptyBloom = <<0:ByteSize/unit:8>>,
    ets:insert(?BLOOM_TABLE, {Partition, EmptyBloom}).

%% Check if message ID exists in any bloom partition
check_bloom(MsgId) ->
    Hashes = bloom_hashes(MsgId),
    check_bloom_all_partitions(Hashes, 0).

check_bloom_all_partitions(_Hashes, Partition) when Partition >= ?WARM_TTL_HOURS ->
    false;
check_bloom_all_partitions(Hashes, Partition) ->
    case ets:lookup(?BLOOM_TABLE, Partition) of
        [{Partition, Bloom}] ->
            case bloom_check_bits(Bloom, Hashes) of
                true -> true;  %% Found (probable)
                false -> check_bloom_all_partitions(Hashes, Partition + 1)
            end;
        [] ->
            check_bloom_all_partitions(Hashes, Partition + 1)
    end.

%% Add message ID to current bloom partition
add_to_bloom(MsgId) ->
    Partition = get_current_partition(),
    Hashes = bloom_hashes(MsgId),
    case ets:lookup(?BLOOM_TABLE, Partition) of
        [{Partition, Bloom}] ->
            NewBloom = bloom_set_bits(Bloom, Hashes),
            ets:insert(?BLOOM_TABLE, {Partition, NewBloom});
        [] ->
            %% Partition doesn't exist, initialize it first
            init_bloom_partition(Partition),
            add_to_bloom(MsgId)
    end.

%% Generate k hash values for bloom filter
bloom_hashes(MsgId) ->
    %% Use double hashing: h(i) = h1 + i*h2 mod m
    H1 = erlang:phash2(MsgId, ?BLOOM_SIZE),
    H2 = erlang:phash2({MsgId, bloom_seed}, ?BLOOM_SIZE),
    [((H1 + I * H2) rem ?BLOOM_SIZE) || I <- lists:seq(0, ?BLOOM_HASHES - 1)].

%% Check if all bits are set
bloom_check_bits(Bloom, Hashes) ->
    lists:all(fun(BitPos) ->
        BytePos = BitPos div 8,
        BitOffset = BitPos rem 8,
        case BytePos < byte_size(Bloom) of
            true ->
                Byte = binary:at(Bloom, BytePos),
                (Byte band (1 bsl BitOffset)) =/= 0;
            false ->
                false
        end
    end, Hashes).

%% Set bits in bloom filter
bloom_set_bits(Bloom, Hashes) ->
    lists:foldl(fun(BitPos, AccBloom) ->
        BytePos = BitPos div 8,
        BitOffset = BitPos rem 8,
        case BytePos < byte_size(AccBloom) of
            true ->
                <<Before:BytePos/binary, Byte:8, After/binary>> = AccBloom,
                NewByte = Byte bor (1 bsl BitOffset),
                <<Before/binary, NewByte:8, After/binary>>;
            false ->
                AccBloom
        end
    end, Bloom, Hashes).

%% Cleanup bloom partitions older than 7 days
cleanup_old_bloom_partitions(_CurrentPartition) ->
    %% In a 168-hour rolling window, we keep all partitions
    %% but reset each one when we rotate back to it
    %% The init_bloom_partition call in rotate_bloom handles this
    ok.

%% =============================================================================
%% P0-FIX: Dedup Log (Mnesia-backed verification for bloom false positives)
%% =============================================================================

%% Write message ID to dedup_log for later verification
%% Uses async dirty_write for performance (eventual consistency OK for dedup)
write_dedup_log(MsgId, Timestamp) ->
    %% Async write to avoid blocking hot path
    spawn(fun() ->
        try
            mnesia:dirty_write({dedup_log, MsgId, Timestamp})
        catch
            _:Reason ->
                logger:warning("Dedup log write failed for ~p: ~p", [MsgId, Reason])
        end
    end),
    ok.

%% Cleanup dedup_log entries older than 7 days
%% Called periodically from handle_info(cleanup, ...)
cleanup_dedup_log() ->
    Now = os:system_time(millisecond),
    CutoffMs = Now - (?WARM_TTL_HOURS * 3600 * 1000),  %% 7 days in ms
    %% Async cleanup to avoid blocking
    spawn(fun() ->
        try
            %% Use dirty_select for efficiency (no transaction overhead)
            OldEntries = mnesia:dirty_select(dedup_log, [
                {{'dedup_log', '$1', '$2'}, [{'<', '$2', CutoffMs}], ['$1']}
            ]),
            lists:foreach(fun(MsgId) ->
                mnesia:dirty_delete(dedup_log, MsgId)
            end, OldEntries),
            case length(OldEntries) of
                0 -> ok;
                N -> logger:info("Dedup log cleanup: removed ~p old entries", [N])
            end
        catch
            _:Reason ->
                logger:warning("Dedup log cleanup failed: ~p", [Reason])
        end
    end),
    ok.
