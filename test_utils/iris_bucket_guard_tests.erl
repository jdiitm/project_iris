-module(iris_bucket_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Bucket Count Guard Tests
%% =============================================================================
%% Written BEFORE implementation. These tests define the contract:
%%   - set_bucket_count/2 MUST reject decreases (prevents data stranding)
%%   - set_bucket_count/2 MUST allow increases (scaling up is safe)
%%   - set_bucket_count/2 MUST allow setting same value (idempotent)
%% =============================================================================

%% Setup: create a minimal Mnesia schema with user_meta table
setup() ->
    %% Stop any existing Mnesia instance
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user_meta, [
        {ram_copies, [node()]},
        {attributes, [user, bucket_count, last_modified]}
    ]),
    mnesia:wait_for_tables([user_meta], 5000),
    ok.

cleanup(_) ->
    application:stop(mnesia),
    ok.

%% Test: Decreasing bucket count is rejected
reject_decrease_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        User = <<"alice">>,
        %% Set initial count to 4
        {atomic, ok} = mnesia:transaction(fun() ->
            mnesia:write({user_meta, User, 4, os:system_time(second)})
        end),
        %% Attempting to decrease to 2 must fail
        Result = iris_core:set_bucket_count(User, 2),
        ?assertMatch({error, {bucket_count_decrease, 4, 2}}, Result)
    end}.

%% Test: Increasing bucket count is allowed
allow_increase_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        User = <<"bob">>,
        %% Set initial count to 2
        {atomic, ok} = mnesia:transaction(fun() ->
            mnesia:write({user_meta, User, 2, os:system_time(second)})
        end),
        %% Increase to 4 must succeed
        Result = iris_core:set_bucket_count(User, 4),
        ?assertMatch({atomic, ok}, Result),
        %% Verify it was stored
        ?assertEqual(4, iris_core:get_bucket_count(User))
    end}.

%% Test: Setting same value is idempotent (allowed)
allow_same_value_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        User = <<"carol">>,
        %% Set initial count to 3
        {atomic, ok} = mnesia:transaction(fun() ->
            mnesia:write({user_meta, User, 3, os:system_time(second)})
        end),
        %% Same value must succeed
        Result = iris_core:set_bucket_count(User, 3),
        ?assertMatch({atomic, ok}, Result)
    end}.

%% Test: First-time set (no existing record) is allowed
allow_first_set_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        User = <<"dave">>,
        %% No existing record — setting any value must succeed
        Result = iris_core:set_bucket_count(User, 5),
        ?assertMatch({atomic, ok}, Result),
        ?assertEqual(5, iris_core:get_bucket_count(User))
    end}.
