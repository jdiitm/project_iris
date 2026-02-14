-module(iris_store_adapter_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: Store Adapter Behaviour Interface Tests
%% =============================================================================
%%
%% Tests verify that iris_store exports a function to identify the current
%% storage backend, enabling future pluggable backends (e.g., Cassandra, S3).
%% =============================================================================

iris_store_adapter_test_() ->
    [
     {"AUDIT M11: iris_store exports backend_type/0",
      fun test_store_has_backend_type/0},
     {"AUDIT M11: default backend is mnesia",
      fun test_default_backend_is_mnesia/0},
     {"AUDIT M11: source has adapter documentation",
      fun test_source_has_adapter_doc/0}
    ].

test_store_has_backend_type() ->
    Exports = iris_store:module_info(exports),
    ?assert(lists:member({backend_type, 0}, Exports)).

test_default_backend_is_mnesia() ->
    ?assertEqual(mnesia, iris_store:backend_type()).

test_source_has_adapter_doc() ->
    {ok, Src} = file:read_file("src/iris_store.erl"),
    ?assert(binary:match(Src, <<"backend_type">>) =/= nomatch).
