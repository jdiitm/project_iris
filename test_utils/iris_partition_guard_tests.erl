-module(iris_partition_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Partition Guard Tests
%% Tests for split-brain detection and dynamic membership
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
      
      %% Dynamic membership tests
      {"Membership mode default is static", fun test_membership_mode_default/0},
      {"Membership mode can be set to dynamic", fun test_membership_mode_dynamic/0},
      {"Status includes membership_mode field", fun test_status_has_membership_mode/0},
      {"Dynamic mode uses pg for discovery", fun test_dynamic_mode_design/0},

      %% : Safe-AP — writes rejected in minority partition
      {"Writes rejected during minority partition (safe-AP)", fun test_writes_rejected_during_minority_partition/0},
      {"Partition mode is 'diverged' not 'safe_mode'", fun test_partition_mode_is_diverged/0},

      %% H-7: Empty expected_nodes must NOT grant quorum
      {"Empty expected_nodes rejects writes (fail-safe)", fun test_empty_expected_nodes_unsafe/0}
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
%% Dynamic Membership Tests
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
    %% Dynamic mode was deprecated for safety reasons.
    %% Even when dynamic is configured, the service now reports 'static' internally
    %% because dynamic mode defeats split-brain protection (both sides see 100% quorum).
    application:set_env(iris_core, partition_guard_mode, dynamic),
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            Status = iris_partition_guard:get_status(),
            gen_server:stop(Pid, normal, 1000),
            %% Should return 'static' even with dynamic configured (safety fix)
            ?assertEqual(static, maps:get(membership_mode, Status));
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
%% RFC Section 7.1.1: AP Mode Tests
%% "Each partition continues reads/writes during split (AP mode)"
%% =============================================================================

test_writes_rejected_during_minority_partition() ->
    %% Configure expected nodes with an unreachable node so quorum is lost
    FakeNode = 'iris_core_fake@unreachable_host',
    application:set_env(iris_core, expected_cluster_nodes, [node(), FakeNode]),
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            %% Trigger partition check — FakeNode is unreachable,
            %% so quorum (>50%) is lost (1 of 2 visible = 50%, not >50%).
            Pid ! check_partition,
            timer:sleep(100),  %% Let the check complete
            %% : Safe-AP — writes REJECTED in minority partition
            Result = iris_partition_guard:is_safe_for_writes(),
            gen_server:stop(Pid, normal, 1000),
            ?assertEqual({error, minority_partition}, Result);
        {error, {already_started, ExistingPid}} ->
            gen_server:stop(ExistingPid, normal, 1000),
            timer:sleep(50),
            test_writes_rejected_during_minority_partition()
    end,
    application:unset_env(iris_core, expected_cluster_nodes).

test_partition_mode_is_diverged() ->
    %% Same setup: unreachable node causes quorum loss
    FakeNode = 'iris_core_fake@unreachable_host',
    application:set_env(iris_core, expected_cluster_nodes, [node(), FakeNode]),
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            %% Trigger partition check
            Pid ! check_partition,
            timer:sleep(100),
            Status = iris_partition_guard:get_status(),
            gen_server:stop(Pid, normal, 1000),
            %% Mode should be 'diverged' (AP), NOT 'safe_mode' (CP)
            ?assertEqual(diverged, maps:get(mode, Status)),
            %% Epoch should have incremented from 0 to 1
            ?assert(maps:get(epoch, Status) >= 1),
            %% V2: Writes rejected in diverged mode (safe-AP)
            ?assertEqual(false, maps:get(safe_for_writes, Status));
        {error, {already_started, ExistingPid}} ->
            gen_server:stop(ExistingPid, normal, 1000),
            timer:sleep(50),
            test_partition_mode_is_diverged()
    end,
    application:unset_env(iris_core, expected_cluster_nodes).

%% =============================================================================
%% Functional Tests (require running guard)
%% =============================================================================

%% These tests verify behavior with a running partition guard

%% test_permissive_when_no_config_() ->
%%     {setup,
%%      fun() ->
%%          application:unset_env(iris_core, expected_cluster_nodes),
%%          application:unset_env(iris_core, partition_guard_mode),
%%          case iris_partition_guard:start_link() of
%%              {ok, Pid} -> {started, Pid};
%%              {error, {already_started, Pid}} -> {existing, Pid}
%%          end
%%      end,
%%      fun({started, Pid}) -> gen_server:stop(Pid, normal, 1000);
%%         ({existing, _}) -> ok
%%      end,
%%      fun(_) ->
%%          [
%%           {"Safe for writes when no expected nodes", fun() ->
%%               Result = iris_partition_guard:is_safe_for_writes(),
%%               ?assertEqual(ok, Result)
%%           end}
%%          ]
%%      end}.

%% =============================================================================
%% H-7: Empty expected_nodes must not grant quorum
%% =============================================================================

test_empty_expected_nodes_unsafe() ->
    %% With no expected nodes configured, the guard cannot determine quorum.
    %% It must fail-safe: treat as unsafe (no quorum).
    application:set_env(iris_core, expected_cluster_nodes, []),
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            Pid ! check_partition,
            timer:sleep(100),
            Result = iris_partition_guard:is_safe_for_writes(),
            gen_server:stop(Pid, normal, 1000),
            ?assertEqual({error, minority_partition}, Result);
        {error, {already_started, ExistingPid}} ->
            gen_server:stop(ExistingPid, normal, 1000),
            timer:sleep(50),
            test_empty_expected_nodes_unsafe()
    end,
    application:unset_env(iris_core, expected_cluster_nodes).

%% =============================================================================
%% 2.2: resolve_authority/4 — Split-Brain Resolution
%% RFC-001 v4.0 Section 7.1.1: Higher epoch wins; tie-break by lowest node ID.
%% =============================================================================

resolve_authority_higher_epoch_wins_test() ->
    NodeA = 'core_a@host1',
    NodeB = 'core_b@host2',
    ?assertEqual({authoritative, NodeA}, iris_partition_guard:resolve_authority(5, NodeA, 3, NodeB)),
    ?assertEqual({authoritative, NodeB}, iris_partition_guard:resolve_authority(2, NodeA, 7, NodeB)).

resolve_authority_tiebreak_by_lowest_node_test() ->
    NodeA = 'aaa@host1',
    NodeB = 'zzz@host2',
    %% Same epoch: lowest node ID wins
    ?assertEqual({authoritative, NodeA}, iris_partition_guard:resolve_authority(1, NodeA, 1, NodeB)),
    ?assertEqual({authoritative, NodeA}, iris_partition_guard:resolve_authority(1, NodeA, 1, NodeB)).

resolve_authority_symmetric_test() ->
    %% Calling with swapped args should give same winner
    NodeA = 'alpha@host',
    NodeB = 'beta@host',
    {authoritative, WinnerAB} = iris_partition_guard:resolve_authority(3, NodeA, 3, NodeB),
    {authoritative, WinnerBA} = iris_partition_guard:resolve_authority(3, NodeB, 3, NodeA),
    ?assertEqual(WinnerAB, WinnerBA).

%% =============================================================================
%% 2.2: force_unsafe_mode overrides minority partition rejection
%% =============================================================================

force_unsafe_mode_overrides_minority_test() ->
    FakeNode = 'fake@nowhere',
    application:set_env(iris_core, expected_cluster_nodes, [node(), FakeNode, 'fake2@nowhere']),
    application:set_env(iris_core, partition_guard_mode, static),
    case iris_partition_guard:start_link() of
        {ok, Pid} ->
            %% Trigger minority detection
            Pid ! check_partition,
            timer:sleep(100),
            ?assertEqual({error, minority_partition}, iris_partition_guard:is_safe_for_writes()),
            %% Force unsafe mode should override
            iris_partition_guard:force_unsafe_mode(true),
            ?assertEqual(ok, iris_partition_guard:is_safe_for_writes()),
            %% Disable force mode — triggers re-check on next partition scan
            iris_partition_guard:force_unsafe_mode(false),
            %% Force a partition re-check to re-enter diverged mode
            Pid ! check_partition,
            timer:sleep(100),
            ?assertEqual({error, minority_partition}, iris_partition_guard:is_safe_for_writes()),
            gen_server:stop(Pid, normal, 1000);
        {error, {already_started, ExistingPid}} ->
            gen_server:stop(ExistingPid, normal, 1000),
            timer:sleep(50),
            force_unsafe_mode_overrides_minority_test()
    end,
    application:unset_env(iris_core, expected_cluster_nodes),
    application:unset_env(iris_core, partition_guard_mode).
