-module(iris_ingress_byte_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Ingress Byte Guard (Micro-Burst Protection)
%% =============================================================================
%%
%% Attack Vector: "The Micro-Burst"
%% An attacker sends 100MB in 100ms. The flow controller polls every 200ms.
%% The burst is admitted before the controller reacts, causing Edge OOM.
%%
%% Mitigation: Per-socket byte counting in the hot path.
%% iris_ingress_byte_guard tracks bytes received per 1-second window.
%% If bytes exceed max_ingress_bytes_per_sec (default 1MB/s), the
%% connection is terminated immediately — no polling delay.
%%
%% Tests verify:
%%   1. Module exports check_bytes/1
%%   2. Under-limit traffic is allowed
%%   3. Over-limit traffic is rejected
%%   4. Counter resets each window
%%   5. Metric emitted on rejection
%% =============================================================================

-define(METRICS_TABLE, iris_metrics_table).

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

ensure_metrics_table() ->
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]);
        _ -> ok
    end.

get_metric(Key) ->
    case ets:lookup(?METRICS_TABLE, Key) of
        [{_, Val}] -> Val;
        [] -> 0
    end.

%% =============================================================================
%% Test: Module exports check_bytes/1 and reset/0
%% =============================================================================

exports_check_bytes_test() ->
    Exports = iris_ingress_byte_guard:module_info(exports),
    ?assert(lists:member({check_bytes, 1}, Exports)),
    ?assert(lists:member({reset, 0}, Exports)).

%% =============================================================================
%% Test: Under-limit traffic is allowed
%% =============================================================================

under_limit_traffic_allowed_test() ->
    iris_ingress_byte_guard:reset(),
    %% Default limit is 1MB/s. Send 100 bytes — well under.
    Result = iris_ingress_byte_guard:check_bytes(100),
    ?assertEqual(ok, Result).

%% =============================================================================
%% Test: Over-limit traffic is rejected
%% =============================================================================

over_limit_traffic_rejected_test() ->
    iris_ingress_byte_guard:reset(),
    %% Set a low limit for testing
    application:set_env(iris_edge, max_ingress_bytes_per_sec, 1000),
    try
        %% Send 500 bytes — ok
        ?assertEqual(ok, iris_ingress_byte_guard:check_bytes(500)),
        %% Send another 600 bytes — total 1100, over 1000 limit
        Result = iris_ingress_byte_guard:check_bytes(600),
        ?assertEqual({error, byte_limit_exceeded}, Result)
    after
        application:unset_env(iris_edge, max_ingress_bytes_per_sec)
    end.

%% =============================================================================
%% Test: Counter resets each 1-second window
%% =============================================================================

counter_resets_per_window_test() ->
    iris_ingress_byte_guard:reset(),
    application:set_env(iris_edge, max_ingress_bytes_per_sec, 1000),
    try
        %% Fill to 900 bytes
        ?assertEqual(ok, iris_ingress_byte_guard:check_bytes(900)),
        %% Simulate window expiry by resetting
        iris_ingress_byte_guard:reset(),
        %% Should be allowed again since counter reset
        ?assertEqual(ok, iris_ingress_byte_guard:check_bytes(900))
    after
        application:unset_env(iris_edge, max_ingress_bytes_per_sec)
    end.

%% =============================================================================
%% Test: Metric emitted on byte limit rejection
%% =============================================================================

metric_emitted_on_byte_reject_test() ->
    ensure_metrics_table(),
    catch ets:insert(?METRICS_TABLE, {ingress_byte_limit_rejects, 0}),
    iris_ingress_byte_guard:reset(),
    application:set_env(iris_edge, max_ingress_bytes_per_sec, 100),
    try
        Before = get_metric(ingress_byte_limit_rejects),
        %% Exceed the limit
        iris_ingress_byte_guard:check_bytes(50),
        iris_ingress_byte_guard:check_bytes(60),
        After = get_metric(ingress_byte_limit_rejects),
        ?assert(After > Before)
    after
        application:unset_env(iris_edge, max_ingress_bytes_per_sec)
    end.
