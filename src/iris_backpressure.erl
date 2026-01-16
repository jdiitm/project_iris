-module(iris_backpressure).

%% =============================================================================
%% Backpressure Integration Layer
%% =============================================================================
%% Purpose: Unified backpressure interface integrating flow_controller,
%% rate_limiter, and circuit_breaker for end-to-end backpressure.
%% 
%% Design:
%% 1. Single entry point for all backpressure checks
%% 2. Propagates backpressure signals to clients
%% 3. Monitors system health and adjusts thresholds dynamically
%% =============================================================================

-export([check/1, check/2]).
-export([should_accept_connection/0]).
-export([get_client_delay/1]).
-export([signal_overload/0, signal_recovery/0]).
-export([get_status/0]).

%% Request types
-define(TYPE_LOGIN, login).
-define(TYPE_MESSAGE, message).
-define(TYPE_BATCH, batch).
-define(TYPE_STATUS, status).

%% =============================================================================
%% API
%% =============================================================================

%% @doc Check if a request should proceed. Returns action to take.
%% Result: {allow, Metadata} | {delay, DelayMs, Reason} | {reject, Reason}
-spec check(atom()) -> {allow, map()} | {delay, integer(), atom()} | {reject, atom()}.
check(RequestType) ->
    check(RequestType, #{}).

-spec check(atom(), map()) -> {allow, map()} | {delay, integer(), atom()} | {reject, atom()}.
check(RequestType, Opts) ->
    User = maps:get(user, Opts, undefined),
    
    %% Layer 1: Flow controller (system-wide)
    FlowResult = check_flow_controller(User),
    
    case FlowResult of
        {deny, Reason, RetryAfter} ->
            handle_denial(RequestType, Reason, RetryAfter);
        {allow, Level} ->
            %% Layer 2: Rate limiter (per-user) - only for write operations
            case should_rate_limit(RequestType) of
                true ->
                    check_rate_limiter(User, Level, Opts);
                false ->
                    {allow, #{level => Level, rate_limited => false}}
            end
    end.

%% @doc Check if we should accept new connections
-spec should_accept_connection() -> boolean().
should_accept_connection() ->
    case whereis(iris_flow_controller) of
        undefined -> true;
        _ ->
            Level = iris_flow_controller:get_level(),
            %% Reject new connections at CRITICAL level
            Level < 4  %% Not CRITICAL
    end.

%% @doc Get delay to impose on client (for slow-down backpressure)
-spec get_client_delay(binary()) -> integer().
get_client_delay(User) ->
    case whereis(iris_flow_controller) of
        undefined -> 0;
        _ ->
            Level = iris_flow_controller:get_level(),
            %% Escalating delays based on pressure level
            case Level of
                1 -> 0;      %% NORMAL
                2 -> 50;     %% SLOW: 50ms delay
                3 -> 200;    %% SHED: 200ms delay
                4 -> 1000    %% CRITICAL: 1s delay
            end
    end.

%% @doc Signal system overload (called by monitors)
signal_overload() ->
    %% Could escalate flow controller level here
    logger:warning("Backpressure: Overload signal received"),
    ok.

%% @doc Signal recovery from overload
signal_recovery() ->
    logger:info("Backpressure: Recovery signal received"),
    ok.

%% @doc Get comprehensive backpressure status
-spec get_status() -> map().
get_status() ->
    FlowStats = get_flow_stats(),
    RateStats = get_rate_stats(),
    
    #{
        flow_controller => FlowStats,
        rate_limiter => RateStats,
        accepting_connections => should_accept_connection(),
        recommended_client_delay => get_client_delay(undefined)
    }.

%% =============================================================================
%% Internal: Flow Controller Integration
%% =============================================================================

check_flow_controller(User) ->
    case whereis(iris_flow_controller) of
        undefined -> 
            {allow, 1};  %% Default to NORMAL if not running
        _ ->
            iris_flow_controller:check_admission(User)
    end.

get_flow_stats() ->
    case whereis(iris_flow_controller) of
        undefined -> #{status => not_running};
        _ -> iris_flow_controller:get_stats()
    end.

%% =============================================================================
%% Internal: Rate Limiter Integration
%% =============================================================================

should_rate_limit(?TYPE_LOGIN) -> true;
should_rate_limit(?TYPE_MESSAGE) -> true;
should_rate_limit(?TYPE_BATCH) -> true;
should_rate_limit(?TYPE_STATUS) -> false;
should_rate_limit(_) -> true.

check_rate_limiter(undefined, Level, _Opts) ->
    {allow, #{level => Level, rate_limited => false}};
check_rate_limiter(User, Level, Opts) ->
    TokensNeeded = maps:get(tokens, Opts, 1),
    case whereis(iris_rate_limiter) of
        undefined ->
            {allow, #{level => Level, rate_limited => false}};
        _ ->
            case iris_rate_limiter:check(User, TokensNeeded) of
                allow ->
                    {allow, #{level => Level, rate_limited => false}};
                {deny, RetryAfter} ->
                    {delay, RetryAfter, rate_limited}
            end
    end.

get_rate_stats() ->
    case whereis(iris_rate_limiter) of
        undefined -> #{status => not_running};
        _ -> iris_rate_limiter:get_stats()
    end.

%% =============================================================================
%% Internal: Denial Handling
%% =============================================================================

handle_denial(RequestType, Reason, RetryAfter) ->
    case Reason of
        system_overload ->
            %% Critical - reject outright
            {reject, system_overload};
        load_shed ->
            %% Shed load - reject non-critical, delay critical
            case is_critical_request(RequestType) of
                true -> {delay, RetryAfter, load_shed};
                false -> {reject, load_shed}
            end;
        rate_limited ->
            %% Rate limited - delay
            {delay, RetryAfter, rate_limited};
        _ ->
            %% Default: delay
            {delay, RetryAfter, Reason}
    end.

is_critical_request(?TYPE_LOGIN) -> true;
is_critical_request(?TYPE_MESSAGE) -> true;
is_critical_request(_) -> false.
