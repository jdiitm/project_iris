-module(iris_edge_fallback_drain_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% B-1: Edge Local Fallback Drain Tests
%% =============================================================================
%% Requirement: Messages stored in iris_edge_pending_offline ETS must be
%% drained to core nodes by a background gen_server. Messages must not be
%% silently lost. Failed drains must retain messages for retry.
%% =============================================================================

setup() ->
    %% Create the fallback ETS table if not exists
    case ets:whereis(iris_edge_pending_offline) of
        undefined ->
            ets:new(iris_edge_pending_offline, [named_table, public, bag]);
        _ ->
            ets:delete_all_objects(iris_edge_pending_offline)
    end,
    %% Start metrics if not running
    case whereis(iris_metrics) of
        undefined ->
            catch iris_metrics:start_link();
        _ -> ok
    end,
    ok.

cleanup(_) ->
    %% Stop drain server if running
    case whereis(iris_edge_fallback_drain) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    %% Clear ETS
    case ets:whereis(iris_edge_pending_offline) of
        undefined -> ok;
        _ -> ets:delete_all_objects(iris_edge_pending_offline)
    end.

%% =============================================================================
%% Test: drain flushes pending messages to core via provided callback
%% =============================================================================
drain_flushes_pending_to_core_test() ->
    setup(),
    try
        %% Insert test messages into fallback ETS
        Now = os:system_time(millisecond),
        ets:insert(iris_edge_pending_offline, {<<"user1">>, <<"msg1">>, Now}),
        ets:insert(iris_edge_pending_offline, {<<"user1">>, <<"msg2">>, Now}),
        ets:insert(iris_edge_pending_offline, {<<"user2">>, <<"msg3">>, Now}),
        ?assertEqual(3, ets:info(iris_edge_pending_offline, size)),

        %% Start drain with a mock forward function that collects results
        Self = self(),
        ForwardFn = fun(User, Msg) ->
            Self ! {forwarded, User, Msg},
            ok
        end,
        {ok, Pid} = iris_edge_fallback_drain:start_link(#{
            forward_fn => ForwardFn,
            drain_interval_ms => 100,
            batch_size => 100
        }),

        %% Wait for drain tick to process
        timer:sleep(300),

        %% All messages should have been forwarded
        ?assertEqual(0, ets:info(iris_edge_pending_offline, size)),

        %% Collect forwarded messages
        Forwarded = collect_messages([]),
        ?assertEqual(3, length(Forwarded)),

        gen_server:stop(Pid)
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: drain retries on core failure (messages not dropped)
%% =============================================================================
drain_retries_on_core_failure_test() ->
    setup(),
    try
        Now = os:system_time(millisecond),
        ets:insert(iris_edge_pending_offline, {<<"user1">>, <<"msg1">>, Now}),
        ?assertEqual(1, ets:info(iris_edge_pending_offline, size)),

        %% Forward function that always fails
        FailFn = fun(_User, _Msg) -> {error, core_unreachable} end,
        {ok, Pid} = iris_edge_fallback_drain:start_link(#{
            forward_fn => FailFn,
            drain_interval_ms => 100,
            batch_size => 100
        }),

        %% Wait for a few drain ticks
        timer:sleep(350),

        %% Messages should still be in ETS (not dropped on failure)
        ?assert(ets:info(iris_edge_pending_offline, size) >= 1),

        gen_server:stop(Pid)
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: drain respects batch_size limit
%% =============================================================================
drain_respects_batch_size_test() ->
    setup(),
    try
        Now = os:system_time(millisecond),
        %% Insert 10 messages
        lists:foreach(fun(I) ->
            User = list_to_binary("user_" ++ integer_to_list(I)),
            Msg = list_to_binary("msg_" ++ integer_to_list(I)),
            ets:insert(iris_edge_pending_offline, {User, Msg, Now})
        end, lists:seq(1, 10)),
        ?assertEqual(10, ets:info(iris_edge_pending_offline, size)),

        %% Track how many messages are forwarded per tick
        Self = self(),
        ForwardFn = fun(User, Msg) ->
            Self ! {forwarded, User, Msg},
            ok
        end,
        {ok, Pid} = iris_edge_fallback_drain:start_link(#{
            forward_fn => ForwardFn,
            drain_interval_ms => 100,
            batch_size => 3
        }),

        %% Wait for enough ticks to drain all (10 msgs / 3 batch = 4 ticks)
        timer:sleep(600),

        %% All messages should eventually be drained
        ?assertEqual(0, ets:info(iris_edge_pending_offline, size)),

        gen_server:stop(Pid)
    after
        cleanup(ok)
    end.

%% =============================================================================
%% Test: drain emits pending gauge metric
%% =============================================================================
drain_emits_pending_metric_test() ->
    setup(),
    try
        Now = os:system_time(millisecond),
        ets:insert(iris_edge_pending_offline, {<<"user1">>, <<"msg1">>, Now}),

        ForwardFn = fun(_User, _Msg) -> ok end,
        {ok, Pid} = iris_edge_fallback_drain:start_link(#{
            forward_fn => ForwardFn,
            drain_interval_ms => 100,
            batch_size => 100
        }),

        %% Wait for drain to run
        timer:sleep(250),

        %% Check that the metric was set (gauge should exist in metrics)
        Metrics = iris_metrics:get_metrics(),
        %% The drain server should have set edge_fallback_pending gauge
        ?assert(maps:is_key(edge_fallback_pending, Metrics)),

        gen_server:stop(Pid)
    after
        cleanup(ok)
    end.

%% Helper to collect forwarded messages from mailbox
collect_messages(Acc) ->
    receive
        {forwarded, User, Msg} ->
            collect_messages([{User, Msg} | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.
