-module(iris_store_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Tests for iris_store.erl
%% =============================================================================
%%
%% Tests cover:
%% - Quorum fallback must emit warning metric (quorum_fallback_count)
%% - Best-effort spawn failures must emit error metric (best_effort_write_error)
%% - Key validation rejects oversized/invalid keys
%% =============================================================================

%% =============================================================================
%% Setup / Teardown
%% =============================================================================

setup() ->
    %% Restart iris_metrics fresh to guarantee its ETS table exists.
    %% Prior test fixtures call start_link which links iris_metrics to the
    %% fixture process. When the fixture dies, iris_metrics dies too, destroying
    %% the ETS table. A subsequent whereis/1 can race with this cleanup and
    %% return a stale pid, causing inc/1 to silently no-op.
    %% gen_server:stop with 'normal' reason does not propagate EXIT signals.
    catch gen_server:stop(iris_metrics, normal, 1000),
    case iris_metrics:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Start Mnesia with a test table
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(test_audit_table, [
        {disc_copies, [node()]},
        {attributes, [key, value]}
    ]),
    mnesia:wait_for_tables([test_audit_table], 5000),
    ok.

cleanup(_) ->
    catch mnesia:delete_table(test_audit_table),
    application:stop(mnesia),
    ok.

%% =============================================================================
%% 4.1: Quorum Fallback Observability
%% =============================================================================

quorum_fallback_test_() ->
    {"Quorum fallback must be observable",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"quorum fallback increments quorum_fallback_count metric", fun() ->
            %% Precondition: iris_quorum_write is NOT registered in unit tests
            ?assertEqual(undefined, whereis(iris_quorum_write)),
            %% Seed the counter so it exists in ETS
            iris_metrics:inc(quorum_fallback_count, 0),
            Before = maps:get(quorum_fallback_count, iris_metrics:get_metrics(), 0),
            %% Write with quorum durability -- must fall back to guaranteed
            ok = iris_store:put(test_audit_table, quorum_fb_key, <<"val">>,
                                #{durability => quorum}),
            After = maps:get(quorum_fallback_count, iris_metrics:get_metrics(), 0),
            ?assert(After > Before)
        end}
      ]}}.

%% =============================================================================
%% 7.5: Best-Effort Spawn Error Observability
%% =============================================================================

best_effort_error_test_() ->
    {"Best-effort spawn errors must be observable",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"best_effort write to nonexistent table increments error metric", fun() ->
            %% Seed the counter
            iris_metrics:inc(best_effort_write_error, 0),
            Before = maps:get(best_effort_write_error, iris_metrics:get_metrics(), 0),
            %% Write to a table that does NOT exist -- spawn will fail
            iris_store:put(nonexistent_audit_table, k, v,
                           #{durability => best_effort}),
            %% Wait for the spawned process to execute and fail
            timer:sleep(300),
            After = maps:get(best_effort_write_error, iris_metrics:get_metrics(), 0),
            ?assert(After > Before)
        end}
      ]}}.

%% =============================================================================
%% 7.4: Mnesia Key Validation
%% =============================================================================

key_validation_test_() ->
    {"Mnesia key validation",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"rejects oversized binary key (>1024 bytes)", fun() ->
            BigKey = binary:copy(<<0>>, 2048),
            ?assertEqual({error, {key_too_large, 2048}},
                         iris_store:put(test_audit_table, BigKey, <<"v">>))
        end},
       {"rejects invalid key type (list)", fun() ->
            ?assertMatch({error, {invalid_key_type, _}},
                         iris_store:put(test_audit_table, [1,2,3], <<"v">>))
        end},
       {"accepts valid binary key", fun() ->
            ?assertEqual(ok,
                         iris_store:put(test_audit_table, <<"valid_key">>, <<"v">>))
        end},
       {"accepts valid atom key", fun() ->
            ?assertEqual(ok,
                         iris_store:put(test_audit_table, valid_atom_key, <<"v">>))
        end},
       {"accepts valid integer key", fun() ->
            ?assertEqual(ok,
                         iris_store:put(test_audit_table, 42, <<"v">>))
        end}
      ]}}.
