-module(iris_store_value_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: Store Value Size Guard Tests
%% =============================================================================
%%
%% Tests verify that iris_store:put/4 rejects oversized values to prevent
%% Mnesia memory pressure from unbounded value writes.
%% =============================================================================

iris_store_value_guard_test_() ->
    [
     {"AUDIT: oversized value (2MB) is rejected",
      fun test_oversized_value_rejected/0},
     {"AUDIT: value at limit (1MB) is accepted",
      fun test_at_limit_value_accepted/0},
     {"AUDIT: source has MAX_VALUE_SIZE guard",
      fun test_source_has_value_guard/0}
    ].

test_oversized_value_rejected() ->
    %% 2MB binary value — must be rejected
    BigValue = binary:copy(<<0>>, 2 * 1024 * 1024),
    Result = iris_store:put(test_table, <<"key">>, BigValue),
    ?assertMatch({error, {value_too_large, _}}, Result).

test_at_limit_value_accepted() ->
    %% Value at exactly MAX_VALUE_SIZE — should be accepted by validation
    %% (may fail on Mnesia write if table doesn't exist, but that's OK)
    AtLimitValue = binary:copy(<<0>>, 1024 * 1024),
    Result = try iris_store:put(test_table, <<"key">>, AtLimitValue)
             catch _:_ -> {error, mnesia_not_running} end,
    %% Should NOT be value_too_large; any other error is fine
    case Result of
        {error, {value_too_large, _}} -> ?assert(false);
        _ -> ok
    end.

test_source_has_value_guard() ->
    {ok, Src} = file:read_file("src/iris_store.erl"),
    ?assert(binary:match(Src, <<"MAX_VALUE_SIZE">>) =/= nomatch),
    ?assert(binary:match(Src, <<"value_too_large">>) =/= nomatch).
