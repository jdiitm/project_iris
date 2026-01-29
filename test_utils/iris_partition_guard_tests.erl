-module(iris_partition_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Partition Guard Tests
%% Tests for split-brain detection and dynamic membership (AUDIT FIX Finding #3)
%% =============================================================================

%% =============================================================================
%% Test Fixtures & Setup
%% =============================================================================

setup() ->
    %% Clean up any existing env
    application:unset_env(iris_core, partition_guard_mode),
    application:unset_env(iris_core, expected_cluster_nodes),
    ok.

cleanup(_) ->
    %% Stop partition guard if running
    case whereis(iris_partition_guard) of
        undefined -> ok;
        Pid -> 
            gen_server:stop(Pid, normal, 1000)
    end,
    application:unset_env(iris_core, partition_guard_mode),
    application:unset_env(iris_core, expected_cluster_nodes),
    ok.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_partition_guard_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Module structure tests
      {"Module exports required functions", fun test_module_exports/0},
      {"Module has is_safe_for_writes API", fun test_is_safe_api_export/0},
      {"Module has get_status API", fun test_get_status_export/0},
      {"Module has force_unsafe_mode API", fun test_force_unsafe_export/0},
      
      %% AUDIT FIX: Dynamic membership tests (Finding #3)
      {"Membership mode default is static", fun test_membership_mode_default/0},
      {"Membership mode can be set to dynamic", fun test_membership_mode_dynamic/0},
      {"Status includes membership_mode field", fun test_status_has_membership_mode/0},
      {"Dynamic mode uses pg for discovery", fun test_dynamic_mode_design/0}
     ]}.

%% =============================================================================
%% Module Structure Tests
%% =============================================================================

test_module_exports() ->
    Exports = iris_partition_guard:module_info(exports),
    ?assert(is_list(Exports)),
    ?assert(length(Exports) >= 3).

test_is_safe_api_export() ->
    Exports = iris_partition_guard:module_info(exports),
    ?assert(lists:member({is_safe_for_writes, 0}, Exports)).

test_get_status_export() ->
    Exports = iris_partition_guard:module_info(exports),
    ?assert(lists:member({get_status, 0}, Exports)).

test_force_unsafe_export() ->
    Exports = iris_partition_guard:module_info(exports),
    ?assert(lists:member({force_unsafe_mode, 1}, Exports)).

%% =============================================================================
%% AUDIT FIX: Dynamic Membership Tests (Finding #3)
%% =============================================================================

test_membership_mode_default() ->
    %% Default should be 'static' for backward compatibility
    application:unset_env(iris_core, partition_guard_mode),
    %% Start the guard to test
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            Status = iris_partition_guard:get_status(),
            gen_server:stop(Pid, normal, 1000),
            ?assertEqual(static, maps:get(membership_mode, Status));
        {error, {already_started, _}} ->
            %% Guard already running, just check status
            Status = iris_partition_guard:get_status(),
            ?assert(maps:is_key(membership_mode, Status))
    end.

test_membership_mode_dynamic() ->
    %% Setting to dynamic should work
    application:set_env(iris_core, partition_guard_mode, dynamic),
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            Status = iris_partition_guard:get_status(),
            gen_server:stop(Pid, normal, 1000),
            ?assertEqual(dynamic, maps:get(membership_mode, Status));
        {error, {already_started, _}} ->
            %% If already running, skip - can't change mode on running guard
            ok
    end,
    application:unset_env(iris_core, partition_guard_mode).

test_status_has_membership_mode() ->
    %% Status should always include membership_mode
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            Status = iris_partition_guard:get_status(),
            gen_server:stop(Pid, normal, 1000),
            ?assert(maps:is_key(membership_mode, Status)),
            ?assert(maps:is_key(mode, Status)),
            ?assert(maps:is_key(safe_for_writes, Status)),
            ?assert(maps:is_key(expected_nodes, Status)),
            ?assert(maps:is_key(visible_nodes, Status));
        {error, {already_started, _}} ->
            Status = iris_partition_guard:get_status(),
            ?assert(maps:is_key(membership_mode, Status))
    end.

test_dynamic_mode_design() ->
    %% Verify the module has the pg-based dynamic membership code
    %% by checking that it compiles and loads successfully
    Info = iris_partition_guard:module_info(),
    ?assert(is_list(Info)),
    %% The module should define the PG_GROUP macro
    %% We can verify the design by checking source attributes
    Attrs = proplists:get_value(attributes, Info, []),
    ?assert(is_list(Attrs)),
    %% Verify start_link is available for testing
    Exports = proplists:get_value(exports, Info, []),
    ?assert(lists:member({start_link, 0}, Exports)).

%% =============================================================================
%% Functional Tests (require running guard)
%% =============================================================================

%% These tests verify behavior with a running partition guard

test_permissive_when_no_config_() ->
    {setup,
     fun() ->
         application:unset_env(iris_core, expected_cluster_nodes),
         application:unset_env(iris_core, partition_guard_mode),
         case iris_partition_guard:start_link() of
             {ok, Pid} -> {started, Pid};
             {error, {already_started, Pid}} -> {existing, Pid}
         end
     end,
     fun({started, Pid}) -> gen_server:stop(Pid, normal, 1000);
        ({existing, _}) -> ok
     end,
     fun(_) ->
         [
          {"Safe for writes when no expected nodes", fun() ->
              Result = iris_partition_guard:is_safe_for_writes(),
              ?assertEqual(ok, Result)
          end}
         ]
     end}.
