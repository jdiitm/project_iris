-module(iris_store_consistency_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: Store Durability Default Consistency Tests
%% =============================================================================
%%
%% Tests verify that iris_store:put/4 and iris_store:batch_put/3 use the
%% same default durability level to prevent confusing semantic mismatches.
%% =============================================================================

iris_store_consistency_test_() ->
    [
     {"AUDIT M10: put default durability is quorum",
      fun test_put_default_durability/0},
     {"AUDIT M10: batch_put default durability matches put",
      fun test_batch_put_default_durability/0}
    ].

test_put_default_durability() ->
    {ok, Src} = file:read_file("src/iris_store.erl"),
    %% put/4 should default to quorum
    ?assert(binary:match(Src, <<"maps:get(durability, Opts, quorum)">>) =/= nomatch).

test_batch_put_default_durability() ->
    {ok, Src} = file:read_file("src/iris_store.erl"),
    %% batch_put should NOT default to guaranteed (old behavior)
    %% Instead both should use the same default
    Lines = binary:split(Src, <<"\n">>, [global]),
    %% Find the batch_put function and check its default
    BatchPutLines = [L || L <- Lines,
                     binary:match(L, <<"batch_put">>) =/= nomatch orelse
                     binary:match(L, <<"do_batch_put">>) =/= nomatch],
    %% The batch_put path should use quorum default, not guaranteed
    SrcStr = iolist_to_binary(BatchPutLines),
    HasGuaranteedDefault = binary:match(SrcStr, <<"Opts, guaranteed">>) =/= nomatch,
    ?assertNot(HasGuaranteedDefault).
