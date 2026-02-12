-module(iris_rpc).
-export([call/4, call/5, cast/4]).

%% AUDIT 6.3: Thin RPC wrapper with metric observability.
%% Tracks call/cast counts per target node via iris_metrics.
%% Rate-limit enforcement can be added later by checking a threshold here.

-spec call(node(), module(), atom(), [term()]) -> term().
call(Node, Mod, Fun, Args) ->
    call(Node, Mod, Fun, Args, 5000).

-spec call(node(), module(), atom(), [term()], timeout()) -> term().
call(Node, Mod, Fun, Args, Timeout) ->
    iris_metrics:inc(rpc_calls_total),
    rpc:call(Node, Mod, Fun, Args, Timeout).

-spec cast(node(), module(), atom(), [term()]) -> true.
cast(Node, Mod, Fun, Args) ->
    iris_metrics:inc(rpc_casts_total),
    rpc:cast(Node, Mod, Fun, Args).
