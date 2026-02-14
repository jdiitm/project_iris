-module(iris_rpc).
-export([call/4, call/5, cast/4]).

%% AUDIT 6.3: RPC wrapper with error handling, metrics, and circuit breaker.
%% AUDIT P0-2: Wraps {badrpc, Reason} into {error, {rpc_failed, Node, Reason}}
%% so callers get a clean error tuple instead of raw badrpc propagation.

-spec call(node(), module(), atom(), [term()]) -> term() | {error, {rpc_failed, node(), term()}}.
call(Node, Mod, Fun, Args) ->
    call(Node, Mod, Fun, Args, 5000).

-spec call(node(), module(), atom(), [term()], timeout()) -> term() | {error, {rpc_failed, node(), term()}}.
call(Node, Mod, Fun, Args, Timeout) ->
    iris_metrics:inc(rpc_calls_total),
    case rpc:call(Node, Mod, Fun, Args, Timeout) of
        {badrpc, Reason} ->
            iris_metrics:inc(rpc_errors_total),
            iris_circuit_breaker:record_failure(Node),
            {error, {rpc_failed, Node, Reason}};
        Result ->
            iris_circuit_breaker:record_success(Node),
            Result
    end.

%% AUDIT V2 P1-1: rpc:cast is fire-and-forget — it always returns 'true'
%% regardless of whether the remote node received or processed the call.
%% There is no way to detect per-cast failure. We emit rpc_casts_unmonitored
%% so dashboards can alert on cast volume as a proxy for potential issues.
-spec cast(node(), module(), atom(), [term()]) -> true.
cast(Node, Mod, Fun, Args) ->
    iris_metrics:inc(rpc_casts_total),
    iris_metrics:inc(rpc_casts_unmonitored),
    rpc:cast(Node, Mod, Fun, Args).
