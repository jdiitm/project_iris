-module(iris_user_safety_tables_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Phase 4 TDD: Mnesia tables required by iris_user_safety must be created
%% =============================================================================
%% RED:  create_tables/1 does not create user_blocks or user_reports.
%% GREEN: Add table creation to iris_core:create_tables/1.
%% =============================================================================

%% Use a test generator to set up / tear down Mnesia once for the suite.
user_safety_tables_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"user_blocks table exists after create_tables",
       fun user_blocks_table_exists/0},
      {"user_reports table exists after create_tables",
       fun user_reports_table_exists/0},
      {"block/check roundtrip works",
       fun block_and_check_roundtrip/0}
     ]}.

setup() ->
    %% Use a fresh temp directory so we don't pollute the workspace
    Dir = "/tmp/iris_test_mnesia_" ++ integer_to_list(erlang:system_time(microsecond)),
    application:set_env(mnesia, dir, Dir),
    %% No join_seeds = standalone seed node path in init_db/0
    application:set_env(iris_core, join_seeds, []),
    %% init_db/0 will: create schema, start mnesia, call create_tables/1
    iris_core:init_db(),
    ok.

teardown(_) ->
    mnesia:stop(),
    ok.

user_blocks_table_exists() ->
    Tables = mnesia:system_info(tables),
    ?assert(lists:member(user_blocks, Tables)).

user_reports_table_exists() ->
    Tables = mnesia:system_info(tables),
    ?assert(lists:member(user_reports, Tables)).

block_and_check_roundtrip() ->
    iris_user_safety:block_user(<<"alice">>, <<"bob">>),
    ?assert(iris_user_safety:is_blocked(<<"alice">>, <<"bob">>)),
    iris_user_safety:unblock_user(<<"alice">>, <<"bob">>),
    ?assertNot(iris_user_safety:is_blocked(<<"alice">>, <<"bob">>)).
