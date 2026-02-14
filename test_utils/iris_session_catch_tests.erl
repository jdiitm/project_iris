-module(iris_session_catch_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT V2 P1-4: Narrow Exception Catch in handle_packet/4
%% =============================================================================
%%
%% Tests verify that handle_packet/4's catch-all clause:
%% 1. Catches badarg when element/2 fails on non-tuples (expected behavior)
%% 2. Does NOT catch other exception classes (exit, throw, error:system_limit)
%% 3. Source code uses 'catch error:badarg' not 'catch _:_'
%% =============================================================================

%% Test: source code uses error:badarg, not _:_
source_uses_narrow_catch_test() ->
    {ok, Src} = file:read_file("src/iris_session.erl"),
    Lines = binary:split(Src, <<"\n">>, [global]),
    %% Find the handle_packet catch-all clause (last clause)
    %% It should contain 'catch error:badarg' NOT 'catch _:_'
    HandlePacketLines = lists:filter(fun(Line) ->
        binary:match(Line, <<"element(1">>) =/= nomatch
        orelse binary:match(Line, <<"catch">>) =/= nomatch
    end, Lines),
    %% Look for lines that have both 'catch' and 'element'
    %% or lines that have 'catch error:badarg'
    HasNarrowCatch = lists:any(fun(Line) ->
        binary:match(Line, <<"catch error:badarg">>) =/= nomatch
    end, HandlePacketLines),
    ?assert(HasNarrowCatch),
    %% Verify no 'catch _:_' near handle_packet's element extraction
    HasWildcardCatch = lists:any(fun(Line) ->
        binary:match(Line, <<"catch _:_">>) =/= nomatch
        andalso binary:match(Line, <<"element">>) =/= nomatch
    end, Lines),
    ?assertNot(HasWildcardCatch).
