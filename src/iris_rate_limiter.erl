-module(iris_rate_limiter).
-behaviour(gen_server).

%% =============================================================================
%% Per-User Token Bucket Rate Limiter
%% =============================================================================
%% Design:
%% 1. Token bucket algorithm for smooth rate limiting
%% 2. ETS-based for O(1) per-request overhead
%% 3. Configurable per-user limits
%% 4. Global fallback limits for unknown users
%% =============================================================================

-export([start_link/0, start_link/1]).
-export([check/1, check/2, allow/1, allow/2]).
-export([get_stats/0, get_user_tokens/1]).
%% HOT-002 FIX: Destination rate limiting to protect hot recipients
-export([check_destination/1, check_destination/2, get_destination_stats/1]).
-export([promote_destination/2, is_destination_hot/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% RFC NFR-17: Distributed rate limit gossip
-export([merge_remote_counters/1]).

-define(SERVER, ?MODULE).
-define(TABLE, iris_rate_limit_buckets).

%% Default limits (configurable via application env)
%% RFC Section 10.1: 5 msg/sec sustained, 20 msg/sec burst (10s window)
-define(DEFAULT_RATE, 5).          %% 5 msgs/second sustained
-define(DEFAULT_BURST, 20).        %% Burst capacity (20 msgs/sec for 10s)
-define(REFILL_INTERVAL, 100).     %% Refill every 100ms

-define(GOSSIP_INTERVAL, 1000).    %% RFC NFR-17: Gossip every 1 second
-define(GOSSIP_PG_GROUP, iris_rate_limit_gossip).

-record(state, {
    refill_timer :: reference(),
    gossip_timer :: reference() | undefined,
    total_allowed = 0 :: integer(),
    total_rejected = 0 :: integer(),
    remote_counters = #{} :: map()  %% #{User => RemoteTokensUsed}
}).

-record(bucket, {
    user :: binary(),
    tokens :: float(),
    rate :: integer(),      %% tokens per second
    burst :: integer(),     %% max tokens (bucket size)
    last_refill :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Check and consume token for user. Returns allow | {deny, RetryAfterMs}.
-spec check(binary()) -> allow | {deny, integer()}.
check(User) ->
    check(User, 1).

-spec check(binary(), integer()) -> allow | {deny, integer()}.
check(User, Tokens) ->
    Now = os:system_time(millisecond),
    
    %% Get or create bucket
    Bucket = get_or_create_bucket(User, Now),
    
    %% Refill tokens based on elapsed time
    RefilledBucket = refill_bucket(Bucket, Now),
    
    %% Try to consume tokens
    case consume_tokens(RefilledBucket, Tokens) of
        {ok, NewBucket} ->
            save_bucket(NewBucket),
            gen_server:cast(?SERVER, allowed),
            allow;
        {not_enough, CurrentTokens} ->
            %% Calculate retry-after
            TokensNeeded = Tokens - CurrentTokens,
            RefillRate = RefilledBucket#bucket.rate / 1000,  %% per ms
            RetryAfter = round(TokensNeeded / max(0.001, RefillRate)),
            gen_server:cast(?SERVER, rejected),
            {deny, max(10, min(RetryAfter, 60000))}
    end.

%% @doc Simpler API that just returns boolean
-spec allow(binary()) -> boolean().
allow(User) ->
    check(User) == allow.

-spec allow(binary(), integer()) -> boolean().
allow(User, Tokens) ->
    check(User, Tokens) == allow.

%% @doc Get rate limiter stats
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Get tokens for a specific user (for debugging)
get_user_tokens(User) ->
    case ets:lookup(?TABLE, User) of
        [Bucket] -> Bucket#bucket.tokens;
        [] -> undefined
    end.

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init(_Opts) ->
    %% Create ETS table for rate limit buckets
    ets:new(?TABLE, [
        set,
        named_table,
        public,
        {keypos, #bucket.user},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    
    %% Start periodic refill/cleanup timer
    TRef = erlang:send_after(?REFILL_INTERVAL * 10, self(), cleanup),
    
    %% RFC NFR-17: Join pg group for distributed rate limit gossip
    GossipTimer = try
        pg:join(?GOSSIP_PG_GROUP, self()),
        erlang:send_after(?GOSSIP_INTERVAL, self(), gossip_counters)
    catch Class:Reason ->
        logger:warning("iris_rate_limiter: pg join failed (single-node mode): ~p:~p", [Class, Reason]),
        undefined
    end,
    
    {ok, #state{refill_timer = TRef, gossip_timer = GossipTimer}}.

handle_call(get_stats, _From, State) ->
    BucketCount = ets:info(?TABLE, size),
    Stats = #{
        active_buckets => BucketCount,
        total_allowed => State#state.total_allowed,
        total_rejected => State#state.total_rejected,
        default_rate => get_default_rate(),
        default_burst => get_default_burst()
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(allowed, State) ->
    {noreply, State#state{total_allowed = State#state.total_allowed + 1}};

handle_cast(rejected, State) ->
    {noreply, State#state{total_rejected = State#state.total_rejected + 1}};

%% RFC NFR-17: Receive remote counters from another edge node
handle_cast({remote_counters, RemoteNode, RemoteCounters}, State) ->
    %% Merge remote counters: for each user, subtract remote usage from local bucket
    %% This implements eventual-consistency distributed rate limiting
    NewRemote = maps:merge(State#state.remote_counters,
        maps:from_list([{User, Used} || {User, Used} <- RemoteCounters])),
    apply_remote_counters(RemoteCounters),
    {noreply, State#state{remote_counters = NewRemote}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Remove buckets for users who haven't been seen in a while
    Now = os:system_time(millisecond),
    Cutoff = Now - 300000,  %% 5 minute idle timeout
    
    cleanup_idle_buckets(Cutoff),
    
    %% Reschedule
    TRef = erlang:send_after(?REFILL_INTERVAL * 10, self(), cleanup),
    {noreply, State#state{refill_timer = TRef}};

%% RFC NFR-17: Periodic gossip of local rate counters to other edge nodes
handle_info(gossip_counters, State) ->
    %% Collect local counter snapshot (users with depleted tokens)
    LocalCounters = collect_local_counters(),
    
    %% Broadcast to all other members in the pg group
    try
        Members = pg:get_members(?GOSSIP_PG_GROUP),
        OtherMembers = [M || M <- Members, M =/= self()],
        [gen_server:cast(M, {remote_counters, node(), LocalCounters}) || M <- OtherMembers]
    catch Class:Reason ->
        logger:warning("iris_rate_limiter: gossip broadcast failed: ~p:~p", [Class, Reason]),
        ok
    end,
    
    %% Reschedule
    NewGossipTimer = erlang:send_after(?GOSSIP_INTERVAL, self(), gossip_counters),
    {noreply, State#state{gossip_timer = NewGossipTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

get_or_create_bucket(User, Now) ->
    case ets:lookup(?TABLE, User) of
        [Bucket] -> Bucket;
        [] ->
            %% Create new bucket with full tokens
            Rate = get_user_rate(User),
            Burst = get_user_burst(User),
            #bucket{
                user = User,
                tokens = float(Burst),
                rate = Rate,
                burst = Burst,
                last_refill = Now
            }
    end.

refill_bucket(Bucket = #bucket{tokens = Tokens, rate = Rate, burst = Burst, last_refill = LastRefill}, Now) ->
    Elapsed = Now - LastRefill,
    if Elapsed =< 0 ->
        Bucket;
    true ->
        %% Add tokens based on elapsed time
        RefillAmount = (Rate / 1000) * Elapsed,  %% tokens per millisecond
        NewTokens = min(float(Burst), Tokens + RefillAmount),
        Bucket#bucket{tokens = NewTokens, last_refill = Now}
    end.

consume_tokens(Bucket = #bucket{tokens = Tokens}, Requested) when Tokens >= Requested ->
    {ok, Bucket#bucket{tokens = Tokens - Requested}};
consume_tokens(#bucket{tokens = Tokens}, _Requested) ->
    {not_enough, Tokens}.

save_bucket(Bucket) ->
    true = ets:insert(?TABLE, Bucket),
    ok.

get_user_rate(User) ->
    %% Could look up per-user rate from config/DB
    %% For now, use global default
    case get_app_env(user_rate_limits) of
        {ok, Limits} ->
            maps:get(User, Limits, get_default_rate());
        undefined ->
            get_default_rate()
    end.

get_user_burst(User) ->
    case get_app_env(user_burst_limits) of
        {ok, Limits} ->
            maps:get(User, Limits, get_default_burst());
        undefined ->
            get_default_burst()
    end.

get_default_rate() ->
    case get_app_env(rate_limit_default) of
        {ok, V} -> V;
        undefined -> ?DEFAULT_RATE
    end.

get_default_burst() ->
    case get_app_env(rate_burst_default) of
        {ok, V} -> V;
        undefined -> ?DEFAULT_BURST
    end.

%% Read config from the application that is actually running on this node.
%% iris_rate_limiter is supervised by iris_edge_sup, so iris_edge env is
%% authoritative. Fall back to iris_core for single-node / test setups.
get_app_env(Key) ->
    case application:get_env(iris_edge, Key) of
        {ok, _} = Ok -> Ok;
        undefined -> application:get_env(iris_core, Key)
    end.

cleanup_idle_buckets(Cutoff) ->
    cleanup_idle_fold(ets:first(?TABLE), Cutoff).

cleanup_idle_fold('$end_of_table', _Cutoff) ->
    ok;
cleanup_idle_fold(User, Cutoff) ->
    Next = ets:next(?TABLE, User),
    case ets:lookup(?TABLE, User) of
        [#bucket{last_refill = LastRefill}] when LastRefill < Cutoff ->
            ets:delete(?TABLE, User);
        _ ->
            ok
    end,
    cleanup_idle_fold(Next, Cutoff).

%% =============================================================================
%% RFC NFR-17: Distributed Rate Limit Counter Gossip
%% =============================================================================

%% Collect local counters: list of {User, TokensUsed} for active users
collect_local_counters() ->
    collect_local_fold(ets:first(?TABLE), []).

collect_local_fold('$end_of_table', Acc) ->
    Acc;
collect_local_fold(User, Acc) ->
    Next = ets:next(?TABLE, User),
    case ets:lookup(?TABLE, User) of
        [#bucket{burst = Burst, tokens = Tokens}] when Burst - Tokens > 0 ->
            collect_local_fold(Next, [{User, round(Burst - Tokens)} | Acc]);
        _ ->
            collect_local_fold(Next, Acc)
    end.

%% Apply remote counters to local buckets: reduce local tokens by remote usage
apply_remote_counters([]) -> ok;
apply_remote_counters([{User, RemoteUsed} | Rest]) ->
    case ets:lookup(?TABLE, User) of
        [Bucket = #bucket{tokens = Tokens}] ->
            %% Reduce local tokens by remote usage (floor at 0)
            NewTokens = max(0.0, Tokens - RemoteUsed),
            save_bucket(Bucket#bucket{tokens = NewTokens});
        [] ->
            ok  %% User not active locally -- ignore
    end,
    apply_remote_counters(Rest).

%% @doc Merge remote counters from another node (called via RPC or gossip)
merge_remote_counters(RemoteCounters) ->
    gen_server:cast(?SERVER, {remote_counters, unknown, RemoteCounters}).

%% =============================================================================
%% HOT-002 FIX: Destination Rate Limiting
%% =============================================================================
%% Protect hot recipients (celebrities/VIPs) from being overwhelmed by limiting
%% the rate of incoming messages per destination user. This prevents:
%% 1. Memory exhaustion on Edge nodes handling celebrity mailboxes
%% 2. Cluster crashes during fan-in scenarios (stress_global_fan_in)
%% 3. Login failures for users with large offline queues (stress_hotspot)
%%
%% Destination limits are HIGHER than sender limits (celebrities expect traffic).
%% Limits can be dynamically promoted for known hot users.
%% =============================================================================

-define(DEST_TABLE, iris_dest_rate_buckets).
-define(HOT_DEST_TABLE, iris_hot_destinations).
-define(DEFAULT_DEST_RATE, 10000).    %% 10K msgs/sec per destination (high)
-define(DEFAULT_DEST_BURST, 50000).   %% 50K burst capacity
-define(HOT_DEST_RATE, 100000).       %% 100K msgs/sec for promoted hot users
-define(HOT_DEST_BURST, 500000).      %% 500K burst for hot users

-record(dest_bucket, {
    user :: binary(),
    tokens :: float(),
    rate :: integer(),
    burst :: integer(),
    last_refill :: integer(),
    total_received :: integer()       %% Track total for hot detection
}).

%% @doc Check if a message can be delivered to destination user
%% Returns allow | {deny, RetryAfterMs} | {throttle, ReducedRate}
-spec check_destination(binary()) -> allow | {deny, integer()} | {throttle, integer()}.
check_destination(DestUser) ->
    check_destination(DestUser, 1).

-spec check_destination(binary(), integer()) -> allow | {deny, integer()} | {throttle, integer()}.
check_destination(DestUser, Tokens) ->
    %% Ensure destination table exists
    ensure_dest_table(),
    
    Now = os:system_time(millisecond),
    Bucket = get_or_create_dest_bucket(DestUser, Now),
    RefilledBucket = refill_dest_bucket(Bucket, Now),
    
    case consume_dest_tokens(RefilledBucket, Tokens) of
        {ok, NewBucket} ->
            %% Update total received count for hot detection
            UpdatedBucket = NewBucket#dest_bucket{
                total_received = NewBucket#dest_bucket.total_received + Tokens
            },
            save_dest_bucket(UpdatedBucket),
            
            %% Check if this user is becoming hot (auto-promote)
            maybe_auto_promote(UpdatedBucket),
            allow;
        {not_enough, CurrentTokens} ->
            %% Calculate retry-after
            TokensNeeded = Tokens - CurrentTokens,
            RefillRate = RefilledBucket#dest_bucket.rate / 1000,
            RetryAfter = round(TokensNeeded / max(0.001, RefillRate)),
            
            logger:warning("HOT-002: Destination ~p rate limited (tokens=~p, need=~p)", 
                          [DestUser, CurrentTokens, Tokens]),
            {deny, max(10, min(RetryAfter, 60000))}
    end.

%% @doc Manually promote a destination to hot status with custom bucket count
%% This increases their rate limit significantly
-spec promote_destination(binary(), integer()) -> ok.
promote_destination(DestUser, BucketMultiplier) ->
    ensure_dest_table(),
    ensure_hot_table(),
    
    %% Mark as hot destination
    ets:insert(?HOT_DEST_TABLE, {DestUser, BucketMultiplier, os:system_time(millisecond)}),
    
    %% Upgrade their bucket limits
    Now = os:system_time(millisecond),
    Rate = ?HOT_DEST_RATE * BucketMultiplier,
    Burst = ?HOT_DEST_BURST * BucketMultiplier,
    
    NewBucket = #dest_bucket{
        user = DestUser,
        tokens = float(Burst),
        rate = Rate,
        burst = Burst,
        last_refill = Now,
        total_received = 0
    },
    save_dest_bucket(NewBucket),
    
    logger:info("HOT-002: Promoted ~p to hot destination (rate=~p, burst=~p)", 
               [DestUser, Rate, Burst]),
    ok.

%% @doc Check if a destination is marked as hot
-spec is_destination_hot(binary()) -> boolean().
is_destination_hot(DestUser) ->
    ensure_hot_table(),
    case ets:lookup(?HOT_DEST_TABLE, DestUser) of
        [{DestUser, _, _}] -> true;
        [] -> false
    end.

%% @doc Get stats for a specific destination
-spec get_destination_stats(binary()) -> map() | undefined.
get_destination_stats(DestUser) ->
    ensure_dest_table(),
    case ets:lookup(?DEST_TABLE, DestUser) of
        [#dest_bucket{tokens = T, rate = R, burst = B, total_received = Total}] ->
            #{
                tokens => T,
                rate => R,
                burst => B,
                total_received => Total,
                is_hot => is_destination_hot(DestUser)
            };
        [] ->
            undefined
    end.

%% Internal: Ensure destination rate limit table exists
ensure_dest_table() ->
    case ets:whereis(?DEST_TABLE) of
        undefined ->
            try
                ets:new(?DEST_TABLE, [
                    set,
                    named_table,
                    public,
                    {keypos, #dest_bucket.user},
                    {read_concurrency, true},
                    {write_concurrency, true}
                ])
            catch
                error:badarg -> ok  %% Already exists (race condition)
            end;
        _ -> ok
    end.

ensure_hot_table() ->
    case ets:whereis(?HOT_DEST_TABLE) of
        undefined ->
            try
                ets:new(?HOT_DEST_TABLE, [
                    set,
                    named_table,
                    public,
                    {read_concurrency, true}
                ])
            catch
                error:badarg -> ok
            end;
        _ -> ok
    end.

get_or_create_dest_bucket(DestUser, Now) ->
    case ets:lookup(?DEST_TABLE, DestUser) of
        [Bucket] -> Bucket;
        [] ->
            %% Check if hot destination
            {Rate, Burst} = case is_destination_hot(DestUser) of
                true -> {?HOT_DEST_RATE, ?HOT_DEST_BURST};
                false -> {?DEFAULT_DEST_RATE, ?DEFAULT_DEST_BURST}
            end,
            #dest_bucket{
                user = DestUser,
                tokens = float(Burst),
                rate = Rate,
                burst = Burst,
                last_refill = Now,
                total_received = 0
            }
    end.

refill_dest_bucket(Bucket = #dest_bucket{tokens = Tokens, rate = Rate, 
                                          burst = Burst, last_refill = LastRefill}, Now) ->
    Elapsed = Now - LastRefill,
    if Elapsed =< 0 ->
        Bucket;
    true ->
        RefillAmount = (Rate / 1000) * Elapsed,
        NewTokens = min(float(Burst), Tokens + RefillAmount),
        Bucket#dest_bucket{tokens = NewTokens, last_refill = Now}
    end.

consume_dest_tokens(Bucket = #dest_bucket{tokens = Tokens}, Requested) when Tokens >= Requested ->
    {ok, Bucket#dest_bucket{tokens = Tokens - Requested}};
consume_dest_tokens(#dest_bucket{tokens = Tokens}, _Requested) ->
    {not_enough, Tokens}.

save_dest_bucket(Bucket) ->
    true = ets:insert(?DEST_TABLE, Bucket),
    ok.

%% Auto-promote users receiving high traffic to hot status
maybe_auto_promote(#dest_bucket{user = User, total_received = Total}) when Total > 100000 ->
    %% User has received 100K+ messages - auto-promote
    case is_destination_hot(User) of
        true -> ok;
        false ->
            logger:info("HOT-002: Auto-promoting ~p to hot destination (received ~p msgs)", [User, Total]),
            promote_destination(User, 10)
    end;
maybe_auto_promote(_) ->
    ok.
