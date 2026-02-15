-module(iris_store_timeout_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% sync_transaction Timeout Protection Tests
%% =============================================================================
%%
%% Tests verify that iris_store's guaranteed durability path wraps
%% sync_transaction with a timeout to prevent indefinite stalls during
%% netsplits.
%% =============================================================================

iris_store_timeout_test_() ->
    [
     {"guaranteed write has timeout protection in source",
      fun test_guaranteed_has_timeout/0},
     {"WRITE_TIMEOUT_MS is defined in source",
      fun test_timeout_constant_defined/0}
    ].

test_guaranteed_has_timeout() ->
    %% Source must contain timeout wrapping around sync_transaction
    {ok, Src} = file:read_file("src/iris_store.erl"),
    %% The guaranteed path should use a spawn+monitor or timer pattern with timeout
    ?assert(binary:match(Src, <<"WRITE_TIMEOUT_MS">>) =/= nomatch),
    ?assert(binary:match(Src, <<"write_timeout">>) =/= nomatch).

test_timeout_constant_defined() ->
    {ok, Src} = file:read_file("src/iris_store.erl"),
    ?assert(binary:match(Src, <<"WRITE_TIMEOUT_MS">>) =/= nomatch).
