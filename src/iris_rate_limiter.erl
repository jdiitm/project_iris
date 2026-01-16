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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, iris_rate_limit_buckets).

%% Default limits (configurable via application env)
-define(DEFAULT_RATE, 100).        %% 100 msgs/second
-define(DEFAULT_BURST, 500).       %% Burst capacity
-define(REFILL_INTERVAL, 100).     %% Refill every 100ms

-record(state, {
    refill_timer :: reference(),
    total_allowed = 0 :: integer(),
    total_rejected = 0 :: integer()
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
    
    {ok, #state{refill_timer = TRef}}.

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
    case application:get_env(iris_core, user_rate_limits) of
        {ok, Limits} ->
            maps:get(User, Limits, get_default_rate());
        undefined ->
            get_default_rate()
    end.

get_user_burst(User) ->
    case application:get_env(iris_core, user_burst_limits) of
        {ok, Limits} ->
            maps:get(User, Limits, get_default_burst());
        undefined ->
            get_default_burst()
    end.

get_default_rate() ->
    application:get_env(iris_core, rate_limit_default, ?DEFAULT_RATE).

get_default_burst() ->
    application:get_env(iris_core, rate_burst_default, ?DEFAULT_BURST).

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
