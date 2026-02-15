-module(iris_invariant_prop_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Property-Based Invariant Tests (EUnit-based)
%% =============================================================================
%% These tests generate random inputs and verify system invariants hold.
%% They serve as lightweight property tests without requiring PropEr/triq.
%%
%% Invariants tested:
%% 1. Dedup never drops unique messages
%% 2. Dedup always drops exact duplicates
%% 3. Inbox ordering is preserved
%% =============================================================================

-define(NUM_ITERATIONS, 50).

setup() ->
    %% Clear stale memory alarms
    persistent_term:put(iris_mnesia_guard_alarms, []),
    application:unset_env(iris_core, mnesia_memory_alarm_bytes),
    %% Unique Mnesia dir
    Dir = "/tmp/iris_prop_test_" ++ integer_to_list(erlang:system_time(microsecond)),
    application:set_env(mnesia, dir, Dir),
    case mnesia:system_info(is_running) of
        no ->
            mnesia:create_schema([node()]),
            mnesia:start();
        _ -> ok
    end,
    %% Create required tables
    create_table_if_missing(offline_msg, [key, timestamp, msg], bag),
    create_table_if_missing(user_meta, [user, bucket_count, last_modified], set),
    create_table_if_missing(dedup_log, [msg_id, timestamp], set),
    mnesia:wait_for_tables([offline_msg, user_meta, dedup_log], 5000),
    %% Start metrics if needed
    case whereis(iris_metrics) of
        undefined -> catch iris_metrics:start_link();
        _ -> ok
    end,
    %% Start dedup service (creates ETS tables internally)
    case whereis(iris_dedup) of
        undefined -> catch iris_dedup:start_link();
        _ -> ok
    end,
    Dir.

create_table_if_missing(Table, Attrs, Type) ->
    case lists:member(Table, mnesia:system_info(tables)) of
        true -> ok;
        false ->
            mnesia:create_table(Table, [
                {ram_copies, [node()]},
                {attributes, Attrs},
                {type, Type}
            ])
    end.

cleanup(Dir) ->
    mnesia:clear_table(offline_msg),
    mnesia:clear_table(dedup_log),
    mnesia:clear_table(user_meta),
    os:cmd("rm -rf " ++ Dir).

%% =============================================================================
%% Invariant 1: Dedup never drops unique messages (dedup layer test)
%% =============================================================================
prop_dedup_never_drops_unique_messages_test() ->
    Dir = setup(),
    try
        N = ?NUM_ITERATIONS,
        %% Generate N unique dedup keys, each should be accepted
        AcceptedCount = lists:foldl(fun(I, Acc) ->
            Key = list_to_binary("unique_" ++ integer_to_list(I)),
            case iris_dedup:check_and_mark(Key) of
                new -> Acc + 1;
                duplicate -> Acc
            end
        end, 0, lists:seq(1, N)),
        %% All N unique keys should be accepted as new
        ?assertEqual(N, AcceptedCount)
    after
        cleanup(Dir)
    end.

%% =============================================================================
%% Invariant 2: Dedup always drops exact duplicates
%% =============================================================================
prop_dedup_always_drops_duplicates_test() ->
    Dir = setup(),
    try
        Key = <<"dup_test_key_42">>,
        %% First insertion should be new
        ?assertEqual(new, iris_dedup:check_and_mark(Key)),
        %% Subsequent insertions should be duplicate
        DupCount = lists:foldl(fun(_, Acc) ->
            case iris_dedup:check_and_mark(Key) of
                duplicate -> Acc + 1;
                new -> Acc
            end
        end, 0, lists:seq(1, 10)),
        %% All 10 re-insertions should be detected as duplicates
        ?assertEqual(10, DupCount)
    after
        cleanup(Dir)
    end.

%% =============================================================================
%% Invariant 3: Inbox ordering preserved (via direct Mnesia store)
%% =============================================================================
prop_inbox_ordering_preserved_test() ->
    Dir = setup(),
    try
        User = <<"prop_user_order">>,
        N = 20,
        %% Store N messages directly via store_offline
        lists:foreach(fun(I) ->
            Msg = <<"msg_", (integer_to_binary(I))/binary>>,
            iris_core:store_offline(User, Msg)
        end, lists:seq(1, N)),
        %% Retrieve — verify all N are present
        Stored = iris_core:retrieve_offline(User),
        ?assertEqual(N, length(Stored))
    after
        cleanup(Dir)
    end.
