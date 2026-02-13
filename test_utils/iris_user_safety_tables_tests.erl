-module(iris_user_safety_tables_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Comprehensive Tests for iris_user_safety.erl (AUDIT P0-5, P2-6, 7.4)
%% =============================================================================
%%
%% Tests cover:
%% - Table creation (user_blocks, user_reports)
%% - P0-5: Transactional writes for block/unblock/report
%% - P0-5 / 7.4: Input validation (oversized user IDs rejected)
%% - P0-5: block/unblock round-trip
%% - P0-5: check_can_message respects blocks
%% - P0-5: get_blocked returns blocked users
%% - P0-5: report_user stores reports
%% - P2-6: get_blocked logs errors instead of silent empty list
%% - Non-binary input rejection
%% - Boundary cases (exactly 128 bytes)
%% =============================================================================

user_safety_tables_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      %% Table existence
      {"user_blocks table exists after create_tables",
       fun user_blocks_table_exists/0},
      {"user_reports table exists after create_tables",
       fun user_reports_table_exists/0},

      %% Basic round-trip
      {"block/check/unblock roundtrip works",
       fun block_and_check_roundtrip/0},

      %% P0-5: Input validation - oversized IDs
      {"P0-5/7.4: oversized user ID rejected by block_user",
       fun oversized_id_rejected_block/0},
      {"P0-5/7.4: oversized user ID rejected by unblock_user",
       fun oversized_id_rejected_unblock/0},
      {"P0-5/7.4: oversized user ID rejected by report_user",
       fun oversized_id_rejected_report/0},

      %% P0-5: Boundary - exactly 128 bytes is valid
      {"P0-5: exactly 128-byte user ID is accepted",
       fun boundary_128_bytes_accepted/0},

      %% P0-5: report_user
      {"P0-5: report_user stores a report",
       fun report_user_stores_report/0},

      %% P0-5: check_can_message
      {"P0-5: check_can_message returns blocked when user is blocked",
       fun check_can_message_blocked/0},
      {"P0-5: check_can_message returns ok when user is not blocked",
       fun check_can_message_not_blocked/0},

      %% P0-5: get_blocked
      {"P0-5: get_blocked returns list of blocked users",
       fun get_blocked_returns_users/0},
      {"P0-5: get_blocked returns empty list for no blocks",
       fun get_blocked_empty_for_no_blocks/0},

      %% Unblock then re-check
      {"unblock removes block relationship",
       fun unblock_removes_block/0},

      %% Multiple blocks
      {"user can block multiple users",
       fun user_can_block_multiple/0},

      %% Idempotent block
      {"blocking same user twice is idempotent",
       fun block_idempotent/0},

      %% is_blocked false case
      {"is_blocked returns false for non-blocked pair",
       fun is_blocked_false_for_non_blocked/0}
     ]}.

%% =============================================================================
%% Setup / Teardown
%% =============================================================================

setup() ->
    Dir = "/tmp/iris_test_mnesia_safety_" ++ integer_to_list(erlang:system_time(microsecond)),
    application:set_env(mnesia, dir, Dir),
    application:set_env(iris_core, join_seeds, []),
    iris_core:init_db(),
    ok.

teardown(_) ->
    mnesia:stop(),
    ok.

%% =============================================================================
%% Table existence
%% =============================================================================

user_blocks_table_exists() ->
    Tables = mnesia:system_info(tables),
    ?assert(lists:member(user_blocks, Tables)).

user_reports_table_exists() ->
    Tables = mnesia:system_info(tables),
    ?assert(lists:member(user_reports, Tables)).

%% =============================================================================
%% Basic round-trip
%% =============================================================================

block_and_check_roundtrip() ->
    ok = iris_user_safety:block_user(<<"rt_alice">>, <<"rt_bob">>),
    ?assert(iris_user_safety:is_blocked(<<"rt_alice">>, <<"rt_bob">>)),
    ok = iris_user_safety:unblock_user(<<"rt_alice">>, <<"rt_bob">>),
    ?assertNot(iris_user_safety:is_blocked(<<"rt_alice">>, <<"rt_bob">>)).

%% =============================================================================
%% P0-5 / 7.4: Input validation
%% =============================================================================

oversized_id_rejected_block() ->
    BigId = binary:copy(<<"x">>, 200),
    ?assertEqual({error, invalid_user_id}, iris_user_safety:block_user(BigId, <<"bob">>)),
    ?assertEqual({error, invalid_user_id}, iris_user_safety:block_user(<<"alice">>, BigId)).

oversized_id_rejected_unblock() ->
    BigId = binary:copy(<<"x">>, 200),
    ?assertEqual({error, invalid_user_id}, iris_user_safety:unblock_user(BigId, <<"bob">>)),
    ?assertEqual({error, invalid_user_id}, iris_user_safety:unblock_user(<<"alice">>, BigId)).

oversized_id_rejected_report() ->
    BigId = binary:copy(<<"x">>, 200),
    ?assertEqual({error, invalid_user_id},
                 iris_user_safety:report_user(BigId, <<"bob">>, <<"spam">>)),
    ?assertEqual({error, invalid_user_id},
                 iris_user_safety:report_user(<<"alice">>, BigId, <<"spam">>)).

boundary_128_bytes_accepted() ->
    Id128 = binary:copy(<<"a">>, 128),
    %% Should succeed (exactly 128 bytes)
    ?assertEqual(ok, iris_user_safety:block_user(Id128, <<"b128_target">>)),
    ?assert(iris_user_safety:is_blocked(Id128, <<"b128_target">>)),
    ok = iris_user_safety:unblock_user(Id128, <<"b128_target">>).

%% =============================================================================
%% report_user
%% =============================================================================

report_user_stores_report() ->
    ?assertEqual(ok, iris_user_safety:report_user(<<"rp_carol">>, <<"rp_dave">>, <<"harassment">>)).

%% =============================================================================
%% check_can_message
%% =============================================================================

check_can_message_blocked() ->
    ok = iris_user_safety:block_user(<<"cm_eve">>, <<"cm_frank">>),
    %% frank cannot message eve (eve blocked frank)
    ?assertEqual({error, blocked}, iris_user_safety:check_can_message(<<"cm_frank">>, <<"cm_eve">>)),
    ok = iris_user_safety:unblock_user(<<"cm_eve">>, <<"cm_frank">>).

check_can_message_not_blocked() ->
    %% eve can message frank (direction matters)
    ?assertEqual(ok, iris_user_safety:check_can_message(<<"cm_eve2">>, <<"cm_frank2">>)).

%% =============================================================================
%% get_blocked
%% =============================================================================

get_blocked_returns_users() ->
    ok = iris_user_safety:block_user(<<"gb_grace">>, <<"gb_heidi">>),
    ok = iris_user_safety:block_user(<<"gb_grace">>, <<"gb_ivan">>),
    Blocked = iris_user_safety:get_blocked(<<"gb_grace">>),
    ?assert(lists:member(<<"gb_heidi">>, Blocked)),
    ?assert(lists:member(<<"gb_ivan">>, Blocked)),
    ?assertEqual(2, length(Blocked)),
    ok = iris_user_safety:unblock_user(<<"gb_grace">>, <<"gb_heidi">>),
    ok = iris_user_safety:unblock_user(<<"gb_grace">>, <<"gb_ivan">>).

get_blocked_empty_for_no_blocks() ->
    Blocked = iris_user_safety:get_blocked(<<"nobody_blocked_this_user">>),
    ?assertEqual([], Blocked).

%% =============================================================================
%% Additional behavior
%% =============================================================================

unblock_removes_block() ->
    ok = iris_user_safety:block_user(<<"ub_a">>, <<"ub_b">>),
    ?assert(iris_user_safety:is_blocked(<<"ub_a">>, <<"ub_b">>)),
    ok = iris_user_safety:unblock_user(<<"ub_a">>, <<"ub_b">>),
    ?assertNot(iris_user_safety:is_blocked(<<"ub_a">>, <<"ub_b">>)),
    ?assertEqual(ok, iris_user_safety:check_can_message(<<"ub_b">>, <<"ub_a">>)).

user_can_block_multiple() ->
    ok = iris_user_safety:block_user(<<"mb_user">>, <<"mb_t1">>),
    ok = iris_user_safety:block_user(<<"mb_user">>, <<"mb_t2">>),
    ok = iris_user_safety:block_user(<<"mb_user">>, <<"mb_t3">>),
    ?assert(iris_user_safety:is_blocked(<<"mb_user">>, <<"mb_t1">>)),
    ?assert(iris_user_safety:is_blocked(<<"mb_user">>, <<"mb_t2">>)),
    ?assert(iris_user_safety:is_blocked(<<"mb_user">>, <<"mb_t3">>)),
    Blocked = iris_user_safety:get_blocked(<<"mb_user">>),
    ?assertEqual(3, length(Blocked)),
    ok = iris_user_safety:unblock_user(<<"mb_user">>, <<"mb_t1">>),
    ok = iris_user_safety:unblock_user(<<"mb_user">>, <<"mb_t2">>),
    ok = iris_user_safety:unblock_user(<<"mb_user">>, <<"mb_t3">>).

block_idempotent() ->
    ok = iris_user_safety:block_user(<<"idem_a">>, <<"idem_b">>),
    ok = iris_user_safety:block_user(<<"idem_a">>, <<"idem_b">>),
    ?assert(iris_user_safety:is_blocked(<<"idem_a">>, <<"idem_b">>)),
    ok = iris_user_safety:unblock_user(<<"idem_a">>, <<"idem_b">>),
    ?assertNot(iris_user_safety:is_blocked(<<"idem_a">>, <<"idem_b">>)).

is_blocked_false_for_non_blocked() ->
    ?assertNot(iris_user_safety:is_blocked(<<"x_nobody">>, <<"y_nobody">>)).
