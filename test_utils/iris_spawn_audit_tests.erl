-module(iris_spawn_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% Guard tests: assert no bare spawn(fun() calls remain in critical modules.
%% All async work must use iris_async:spawn_monitored for observability.

no_bare_spawn_in_iris_session_test() ->
    {ok, Src} = file:read_file("src/iris_session.erl"),
    Matches = count_bare_spawns(Src),
    ?assertEqual(0, Matches).

no_bare_spawn_in_iris_presence_test() ->
    {ok, Src} = file:read_file("src/iris_presence.erl"),
    Matches = count_bare_spawns(Src),
    ?assertEqual(0, Matches).

no_bare_spawn_in_iris_core_registry_test() ->
    {ok, Src} = file:read_file("src/iris_core_registry.erl"),
    Matches = count_bare_spawns(Src),
    ?assertEqual(0, Matches).

%% Count occurrences of bare spawn(fun() — excludes spawn_monitored and spawn_link
count_bare_spawns(Src) ->
    %% Match "spawn(fun()" but NOT "spawn_monitored" or "spawn_link"
    %% Strategy: count all "spawn(fun()" then subtract "spawn_monitored(", "spawn_link("
    AllSpawns = count_occurrences(Src, <<"spawn(fun()">>),
    %% Bare spawn(fun() is what we're looking for — spawn_monitored uses a different call
    AllSpawns.

count_occurrences(Bin, Pattern) ->
    count_occurrences(Bin, Pattern, 0).

count_occurrences(Bin, Pattern, Acc) ->
    case binary:match(Bin, Pattern) of
        nomatch -> Acc;
        {Pos, Len} ->
            Rest = binary:part(Bin, Pos + Len, byte_size(Bin) - Pos - Len),
            count_occurrences(Rest, Pattern, Acc + 1)
    end.
