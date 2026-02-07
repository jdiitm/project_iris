-module(iris_block_report_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-9 (RL-3): User Block/Report Tests
%%
%% RFC-001 v4.0 FR-8b: Users can block/unblock and report other users.
%% Blocked users cannot send messages to the blocker.
%%
%% Tests verify:
%% 1. block_user stores block
%% 2. is_blocked returns true after block
%% 3. unblock restores messaging
%% 4. Block is directional (A blocks B != B blocks A)
%% 5. report_user stores report
%% 6. Report does not auto-block
%% 7. get_blocked returns list
%%
%% Pattern: follows Mnesia-backed ETS test patterns.
%% =============================================================================

setup() ->
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    case mnesia:create_table(user_blocks, [
        {ram_copies, [node()]},
        {attributes, [key, blocker, blocked, created_at]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_blocks}} -> ok
    end,
    case mnesia:create_table(user_reports, [
        {ram_copies, [node()]},
        {attributes, [key, reporter, reported, reason, created_at]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, user_reports}} -> ok
    end,
    mnesia:wait_for_tables([user_blocks, user_reports], 5000),
    ok.

cleanup(_) ->
    catch mnesia:delete_table(user_blocks),
    catch mnesia:delete_table(user_reports),
    application:stop(mnesia).

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_block_report_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Block user stores", fun test_block_user_stores/0},
      {"Block prevents messaging", fun test_block_prevents_messaging/0},
      {"Unblock restores", fun test_unblock_user_restores/0},
      {"Block is directional", fun test_block_is_directional/0},
      {"Report stores", fun test_report_user_stores/0},
      {"Report does not block", fun test_report_does_not_block/0},
      {"Get blocked list", fun test_get_blocked_list/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_block_user_stores() ->
    Result = iris_user_safety:block_user(<<"alice">>, <<"bob">>),
    ?assertEqual(ok, Result),
    ?assert(iris_user_safety:is_blocked(<<"alice">>, <<"bob">>)).

test_block_prevents_messaging() ->
    iris_user_safety:block_user(<<"blocker1">>, <<"sender1">>),
    Result = iris_user_safety:check_can_message(<<"sender1">>, <<"blocker1">>),
    ?assertEqual({error, blocked}, Result).

test_unblock_user_restores() ->
    iris_user_safety:block_user(<<"unblocker">>, <<"unblocked">>),
    ?assert(iris_user_safety:is_blocked(<<"unblocker">>, <<"unblocked">>)),
    iris_user_safety:unblock_user(<<"unblocker">>, <<"unblocked">>),
    ?assertNot(iris_user_safety:is_blocked(<<"unblocker">>, <<"unblocked">>)),
    ?assertEqual(ok, iris_user_safety:check_can_message(<<"unblocked">>, <<"unblocker">>)).

test_block_is_directional() ->
    iris_user_safety:block_user(<<"dir_a">>, <<"dir_b">>),
    ?assert(iris_user_safety:is_blocked(<<"dir_a">>, <<"dir_b">>)),
    ?assertNot(iris_user_safety:is_blocked(<<"dir_b">>, <<"dir_a">>)).

test_report_user_stores() ->
    Result = iris_user_safety:report_user(<<"reporter">>, <<"reported">>, <<"spam">>),
    ?assertEqual(ok, Result).

test_report_does_not_block() ->
    iris_user_safety:report_user(<<"rep_a">>, <<"rep_b">>, <<"abuse">>),
    %% Report alone should not block messaging
    Result = iris_user_safety:check_can_message(<<"rep_b">>, <<"rep_a">>),
    ?assertEqual(ok, Result).

test_get_blocked_list() ->
    iris_user_safety:block_user(<<"list_user">>, <<"blocked1">>),
    iris_user_safety:block_user(<<"list_user">>, <<"blocked2">>),
    List = iris_user_safety:get_blocked(<<"list_user">>),
    ?assert(is_list(List)),
    ?assert(lists:member(<<"blocked1">>, List)),
    ?assert(lists:member(<<"blocked2">>, List)).
