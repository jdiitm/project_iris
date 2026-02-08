-module(iris_app_metadata_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Phase 2 TDD: .app.src files must not reference non-existent modules
%% =============================================================================
%% RED:  iris_core.app.src lists iris_rocksdb which has no .erl file.
%% GREEN: Remove phantom modules (or set modules to []).
%% =============================================================================

core_app_has_no_phantom_modules_test() ->
    {ok, [{application, iris_core, Props}]} =
        file:consult("src/iris_core.app.src"),
    Modules = proplists:get_value(modules, Props, []),
    lists:foreach(fun(M) ->
        Path = "src/" ++ atom_to_list(M) ++ ".erl",
        ?assertMatch(true, filelib:is_file(Path),
                     lists:flatten(io_lib:format("Phantom module: ~p (no file ~s)", [M, Path])))
    end, Modules).

core_app_mod_callback_exists_test() ->
    {ok, [{application, iris_core, Props}]} =
        file:consult("src/iris_core.app.src"),
    {Mod, _} = proplists:get_value(mod, Props),
    Path = "src/" ++ atom_to_list(Mod) ++ ".erl",
    ?assert(filelib:is_file(Path)).

edge_app_mod_callback_exists_test() ->
    {ok, [{application, iris_edge, Props}]} =
        file:consult("src/iris_edge.app.src"),
    {Mod, _} = proplists:get_value(mod, Props),
    Path = "src/" ++ atom_to_list(Mod) ++ ".erl",
    ?assert(filelib:is_file(Path)).
