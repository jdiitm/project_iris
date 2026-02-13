-module(iris_core_characterization_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION P2-2: Characterization Tests for iris_core
%% Pins the public API contract to enable safe future refactoring.
%% Each test exercises an exported function and asserts the output shape.
%% =============================================================================

setup() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        _ ->
            mnesia:start(),
            timer:sleep(100)
    end,
    ok.

cleanup(_) ->
    ok.

%% make_dedup_key/2: Returns a binary key
make_dedup_key_binary_test() ->
    Key = iris_core:make_dedup_key(<<"alice">>, <<"hello">>),
    ?assert(is_binary(Key)),
    ?assert(byte_size(Key) > 0).

%% make_dedup_key/2: Deterministic for same inputs
make_dedup_key_deterministic_test() ->
    K1 = iris_core:make_dedup_key(<<"bob">>, <<"world">>),
    K2 = iris_core:make_dedup_key(<<"bob">>, <<"world">>),
    ?assertEqual(K1, K2).

%% make_dedup_key/2: Different inputs produce different keys
make_dedup_key_unique_test() ->
    K1 = iris_core:make_dedup_key(<<"alice">>, <<"msg1">>),
    K2 = iris_core:make_dedup_key(<<"alice">>, <<"msg2">>),
    ?assertNotEqual(K1, K2).

%% make_dedup_key/2: Handles sequenced messages
make_dedup_key_sequenced_test() ->
    Key = iris_core:make_dedup_key(<<"user">>, {42, <<"payload">>}),
    ?assert(is_binary(Key)),
    ?assert(byte_size(Key) > 0).

%% table_spec/1: Returns {StorageType, Opts} tuple for known tables
table_spec_returns_tuple_test() ->
    {StorageType, Opts} = iris_core:table_spec(offline_msg),
    ?assert(StorageType =:= ram_copies orelse
            StorageType =:= disc_copies orelse
            StorageType =:= disc_only_copies),
    ?assert(is_list(Opts)).

table_spec_presence_test() ->
    {ram_copies, _Opts} = iris_core:table_spec(presence).

%% validate_production_cookie/1: Default cookie returns error in production
validate_production_cookie_test_() ->
    {setup,
     fun() ->
         application:set_env(iris_core, deployment_mode, production),
         ok
     end,
     fun(_) ->
         application:set_env(iris_core, deployment_mode, development),
         ok
     end,
     fun(_) ->
         ?_assertEqual({error, default_cookie_in_production},
                       iris_core:validate_production_cookie(iris_secret))
     end}.

%% validate_consistency_mode/0: hardened_ap is always ok
validate_consistency_mode_ok_test_() ->
    {setup,
     fun() ->
         application:set_env(iris_core, consistency_mode, hardened_ap),
         ok
     end,
     fun(_) -> ok end,
     fun(_) ->
         ?_assertEqual(ok, iris_core:validate_consistency_mode())
     end}.

%% is_core_node/1: Checks node naming convention
is_core_node_test() ->
    ?assert(iris_core:is_core_node('core_us_1@host1')),
    ?assert(iris_core:is_core_node('iris_core@host2')),
    ?assertNot(iris_core:is_core_node('edge_1@host3')).

%% should_overwrite/3: Returns boolean for conflict resolution
should_overwrite_shape_test() ->
    %% Record with newer timestamp should overwrite
    OldRecord = {presence, <<"user1">>, 100},
    NewRecord = {presence, <<"user1">>, 200},
    Result = iris_core:should_overwrite(OldRecord, NewRecord, 3),
    ?assert(is_boolean(Result)).

%% check_mtls_enforcement/1: Returns ok when mtls not enforced
check_mtls_not_enforced_test_() ->
    {setup,
     fun() ->
         application:set_env(iris_core, enforce_mtls, false),
         ok
     end,
     fun(_) ->
         application:unset_env(iris_core, enforce_mtls),
         ok
     end,
     fun(_) ->
         ?_assertEqual(ok, iris_core:check_mtls_enforcement())
     end}.
