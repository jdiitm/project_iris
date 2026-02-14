-module(iris_ingress_byte_guard).

%% =============================================================================
%% AUDIT MITIGATION: Per-Socket Ingress Byte Guard (Attack Vector 3)
%% =============================================================================
%% The flow controller (iris_flow_controller) polls system memory every 200ms.
%% An attacker can send 100MB in <200ms, OOM-ing the Edge node before
%% backpressure kicks in.
%%
%% This module provides per-socket byte counting in the hot path.
%% It tracks bytes received per 1-second sliding window using the process
%% dictionary (zero allocation, no gen_server). If bytes exceed the
%% configurable limit (default 1MB/s), it returns {error, byte_limit_exceeded}.
%%
%% Usage in iris_edge_conn:handle_socket_data/2:
%%   case iris_ingress_byte_guard:check_bytes(byte_size(Bin)) of
%%       ok -> proceed;
%%       {error, byte_limit_exceeded} -> {stop, byte_limit, Data}
%%   end
%% =============================================================================

-export([check_bytes/1, reset/0, get_limit/0]).

-define(DEFAULT_MAX_BYTES_PER_SEC, 1048576).  %% 1 MB/s default
-define(WINDOW_MS, 1000).                      %% 1 second window

%% @doc Check if receiving `ByteCount` bytes would exceed the per-second limit.
%% Returns ok | {error, byte_limit_exceeded}.
%% Uses process dictionary for zero-allocation tracking.
-spec check_bytes(non_neg_integer()) -> ok | {error, byte_limit_exceeded}.
check_bytes(ByteCount) ->
    Now = os:system_time(millisecond),
    MaxBytes = get_limit(),

    {WindowStart, AccBytes} = case get(iris_byte_guard) of
        undefined -> {Now, 0};
        {WS, AB} -> {WS, AB}
    end,

    %% Check if current window has expired
    case Now - WindowStart >= ?WINDOW_MS of
        true ->
            %% New window — reset counter
            put(iris_byte_guard, {Now, ByteCount}),
            ok;
        false ->
            NewAcc = AccBytes + ByteCount,
            case NewAcc > MaxBytes of
                true ->
                    %% Byte limit exceeded — emit metric and reject
                    try iris_metrics:inc(ingress_byte_limit_rejects)
                    catch _:_ -> ok
                    end,
                    {error, byte_limit_exceeded};
                false ->
                    put(iris_byte_guard, {WindowStart, NewAcc}),
                    ok
            end
    end.

%% @doc Reset the byte counter. Called on connection init or for testing.
-spec reset() -> ok.
reset() ->
    erase(iris_byte_guard),
    ok.

%% @doc Get the configured max bytes per second.
-spec get_limit() -> non_neg_integer().
get_limit() ->
    case application:get_env(iris_edge, max_ingress_bytes_per_sec) of
        {ok, V} when is_integer(V), V > 0 -> V;
        _ ->
            case application:get_env(iris_core, max_ingress_bytes_per_sec) of
                {ok, V} when is_integer(V), V > 0 -> V;
                _ -> ?DEFAULT_MAX_BYTES_PER_SEC
            end
    end.
