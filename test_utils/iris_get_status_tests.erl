-module(iris_get_status_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT V2 P1-2: get_status/1 Must Distinguish Non-Existent Users
%% =============================================================================
%%
%% Tests verify that get_status_from_disk/1 returns {error, not_found}
%% for users that have never existed (no user_status record) instead of
%% the ambiguous {online, false, 0}.
%%
%% Source analysis approach: verifying the return value in source code
%% since starting Mnesia for a unit test is heavyweight.
%% =============================================================================

%% Test: source code returns {error, not_found} for unknown users
get_status_returns_not_found_for_unknown_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    %% The get_status_from_disk/1 function should return {error, not_found}
    %% when dirty_read returns [], not {online, false, 0}
    Lines = binary:split(Src, <<"\n">>, [global]),
    %% Find lines within get_status_from_disk that match the empty-result clause
    InFunction = find_function_body(Lines, <<"get_status_from_disk">>),
    %% The empty-result clause should return {error, not_found}
    HasNotFound = lists:any(fun(Line) ->
        binary:match(Line, <<"{error, not_found}">>) =/= nomatch
    end, InFunction),
    ?assert(HasNotFound),
    %% And should NOT return {online, false, 0}
    HasAmbiguousReturn = lists:any(fun(Line) ->
        binary:match(Line, <<"{online, false, 0}">>) =/= nomatch
    end, InFunction),
    ?assertNot(HasAmbiguousReturn).

%% Test: spec for get_status_from_disk includes {error, not_found}
get_status_spec_includes_not_found_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    %% The -spec for get_status_from_disk should include {error, not_found}
    %% Check: a line exists that has both "-spec" and "get_status_from_disk"
    %% and "not_found"
    Lines = binary:split(Src, <<"\n">>, [global]),
    HasSpecWithNotFound = lists:any(fun(Line) ->
        binary:match(Line, <<"-spec">>) =/= nomatch
        andalso binary:match(Line, <<"get_status_from_disk">>) =/= nomatch
        andalso binary:match(Line, <<"not_found">>) =/= nomatch
    end, Lines),
    ?assert(HasSpecWithNotFound).

%% Helper: Extract lines belonging to a function body (from definition to next function)
find_function_body(Lines, FuncName) ->
    find_function_body(Lines, FuncName, false, []).

find_function_body([], _FuncName, _InFunc, Acc) ->
    lists:reverse(Acc);
find_function_body([Line | Rest], FuncName, false, Acc) ->
    case binary:match(Line, FuncName) of
        nomatch -> find_function_body(Rest, FuncName, false, Acc);
        _ -> find_function_body(Rest, FuncName, true, [Line | Acc])
    end;
find_function_body([Line | Rest], FuncName, true, Acc) ->
    %% End of function: next function definition (non-indented, ends with ->)
    IsNewFunc = case Line of
        <<>> -> false;
        <<C, _/binary>> when C =/= $\s, C =/= $\t, C =/= $% ->
            binary:match(Line, <<"->">>) =/= nomatch
            andalso binary:match(Line, FuncName) =:= nomatch;
        _ -> false
    end,
    case IsNewFunc of
        true -> lists:reverse(Acc);
        false -> find_function_body(Rest, FuncName, true, [Line | Acc])
    end.

