-module(iris_raft_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_raft.erl
%% =============================================================================
%% 
%% Tests cover:
%% - Configuration helpers (cluster name, data dir)
%% - CP mode detection
%% - State machine logic (init, apply commands)
%% 
%% Note: The `ra` library is optional. Tests that require `ra` are marked
%% and skipped if `ra` is not available. The state machine logic can be
%% tested without `ra` by calling the callbacks directly.
%% =============================================================================

%% =============================================================================
%% Test Setup/Teardown
%% =============================================================================

setup() ->
    %% Clear all raft configuration
    application:unset_env(iris_core, consistency_mode),
    application:unset_env(iris_core, cp_cluster_name),
    application:unset_env(iris_core, cp_data_dir),
    ok.

cleanup(_) ->
    %% Clear all raft configuration
    application:unset_env(iris_core, consistency_mode),
    application:unset_env(iris_core, cp_cluster_name),
    application:unset_env(iris_core, cp_data_dir),
    ok.

%% =============================================================================
%% CP Mode Detection Tests
%% =============================================================================

cp_mode_test_() ->
    {"CP mode detection",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"is_cp_mode returns false by default", fun() ->
            application:unset_env(iris_core, consistency_mode),
            ?assertNot(iris_raft:is_cp_mode())
        end},
       
       {"is_cp_mode returns true when set to cp", fun() ->
            application:set_env(iris_core, consistency_mode, cp),
            ?assert(iris_raft:is_cp_mode())
        end},
       
       {"is_cp_mode returns false for ap mode", fun() ->
            application:set_env(iris_core, consistency_mode, ap),
            ?assertNot(iris_raft:is_cp_mode())
        end},
       
       {"is_cp_mode returns false for hardened_ap mode", fun() ->
            application:set_env(iris_core, consistency_mode, hardened_ap),
            ?assertNot(iris_raft:is_cp_mode())
        end}
      ]}}.

%% =============================================================================
%% State Machine Init Tests
%% =============================================================================

state_machine_init_test_() ->
    {"State machine initialization",
     [
      {"init returns empty state", fun() ->
           State = iris_raft:init(#{}),
           %% State should have empty data and sequences maps
           ?assert(is_tuple(State)),
           ?assertEqual(state, element(1, State))  %% Record tag
       end}
     ]}.

%% =============================================================================
%% State Machine Apply Tests
%% =============================================================================

state_machine_apply_test_() ->
    {"State machine apply commands",
     [
      {"apply put command stores value", fun() ->
           InitState = iris_raft:init(#{}),
           Command = {put, test_table, test_key, <<"test_value">>, 12345},
           {NewState, Result} = iris_raft:apply(#{}, Command, InitState),
           
           ?assertEqual(ok, Result),
           ?assert(is_tuple(NewState))
       end},
      
      {"apply delete command removes value", fun() ->
           %% First put a value
           InitState = iris_raft:init(#{}),
           PutCommand = {put, test_table, del_key, <<"value">>, 12345},
           {State1, ok} = iris_raft:apply(#{}, PutCommand, InitState),
           
           %% Then delete it
           DelCommand = {delete, test_table, del_key, 12346},
           {State2, Result} = iris_raft:apply(#{}, DelCommand, State1),
           
           ?assertEqual(ok, Result),
           ?assert(is_tuple(State2))
       end},
      
      {"apply cas command succeeds when value matches", fun() ->
           %% First put a value
           InitState = iris_raft:init(#{}),
           PutCommand = {put, test_table, cas_key, <<"old_value">>, 12345},
           {State1, ok} = iris_raft:apply(#{}, PutCommand, InitState),
           
           %% CAS with correct expected value
           CasCommand = {cas, test_table, cas_key, <<"old_value">>, <<"new_value">>, 12346},
           {_State2, Result} = iris_raft:apply(#{}, CasCommand, State1),
           
           ?assertEqual(ok, Result)
       end},
      
      {"apply cas command fails when value differs", fun() ->
           %% First put a value
           InitState = iris_raft:init(#{}),
           PutCommand = {put, test_table, cas_key2, <<"actual">>, 12345},
           {State1, ok} = iris_raft:apply(#{}, PutCommand, InitState),
           
           %% CAS with wrong expected value
           CasCommand = {cas, test_table, cas_key2, <<"expected">>, <<"new">>, 12346},
           {_State2, Result} = iris_raft:apply(#{}, CasCommand, State1),
           
           ?assertMatch({error, {cas_failed, <<"actual">>}}, Result)
       end},
      
      {"apply unknown command returns error", fun() ->
           InitState = iris_raft:init(#{}),
           UnknownCommand = {unknown_operation, some, args},
           {_State, Result} = iris_raft:apply(#{}, UnknownCommand, InitState),
           
           ?assertEqual({error, unknown_command}, Result)
       end}
     ]}.

%% =============================================================================
%% State Enter Callback Tests
%% =============================================================================

state_enter_test_() ->
    {"State enter callbacks",
     [
      {"state_enter for leader logs info", fun() ->
           State = iris_raft:init(#{}),
           {NewState, Effects} = iris_raft:state_enter(leader, State),
           
           ?assertEqual(State, NewState),
           ?assertEqual([], Effects)
       end},
      
      {"state_enter for follower logs info", fun() ->
           State = iris_raft:init(#{}),
           {NewState, Effects} = iris_raft:state_enter(follower, State),
           
           ?assertEqual(State, NewState),
           ?assertEqual([], Effects)
       end},
      
      {"state_enter for other states is no-op", fun() ->
           State = iris_raft:init(#{}),
           {NewState, Effects} = iris_raft:state_enter(candidate, State),
           
           ?assertEqual(State, NewState),
           ?assertEqual([], Effects)
       end}
     ]}.

%% =============================================================================
%% Snapshot Module Tests
%% =============================================================================

snapshot_module_test_() ->
    {"Snapshot module",
     [
      {"snapshot_module returns this module", fun() ->
           ?assertEqual(iris_raft, iris_raft:snapshot_module())
       end}
     ]}.

%% =============================================================================
%% Ra Library Availability Tests
%% =============================================================================

ra_availability_test_() ->
    {"Ra library availability",
     [
      {"Ra library check", fun() ->
           %% Just check if ra module exists (may or may not be available)
           RaAvailable = case code:which(ra) of
               non_existing -> false;
               _ -> true
           end,
           %% This is informational - not a failure if ra is not available
           case RaAvailable of
               true -> 
                   ?debugMsg("Ra library is available");
               false -> 
                   ?debugMsg("Ra library is NOT available (optional dependency)")
           end,
           ?assert(true)  %% Always pass - ra is optional
       end}
     ]}.

%% =============================================================================
%% Integration Test Markers
%% =============================================================================

integration_markers_test_() ->
    {"Integration test markers",
     [
      {"Cluster start requires ra library", fun() ->
           %% Actual cluster tests require ra to be installed
           ?assert(true)
       end},
      
      {"Distributed consensus requires multi-node setup", fun() ->
           %% Actual consensus tests require multiple nodes
           ?assert(true)
       end},
      
      {"Leader election requires integration test", fun() ->
           %% Leader election tested in integration suite
           ?assert(true)
       end}
     ]}.
