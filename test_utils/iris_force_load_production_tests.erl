-module(iris_force_load_production_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% H-2: Block force_load_table in Production Tests
%% =============================================================================
%% Requirement: force_load_isolated/1 must refuse to run when
%% deployment_mode=production. This prevents stale/divergent data from
%% being silently loaded into a production cluster.
%% =============================================================================

setup() ->
    SavedMode = application:get_env(iris_core, deployment_mode),
    SavedMode.

cleanup(SavedMode) ->
    case SavedMode of
        {ok, Val} -> application:set_env(iris_core, deployment_mode, Val);
        undefined -> application:unset_env(iris_core, deployment_mode)
    end.

%% =============================================================================
%% Test: force_load blocked in production
%% =============================================================================
force_load_blocked_in_production_test() ->
    Saved = setup(),
    try
        application:set_env(iris_core, deployment_mode, production),
        Result = iris_core:force_load_isolated(offline_msg),
        ?assertEqual({error, blocked_in_production}, Result)
    after
        cleanup(Saved)
    end.

%% =============================================================================
%% Test: force_load allowed in development
%% =============================================================================
force_load_allowed_in_development_test() ->
    Saved = setup(),
    try
        application:set_env(iris_core, deployment_mode, development),
        %% In development, force_load_isolated proceeds (not blocked).
        %% It may crash due to Mnesia state in test env — that's fine.
        %% The key assertion: it does NOT return {error, blocked_in_production}.
        Result = try
            iris_core:force_load_isolated(offline_msg)
        catch
            exit:_ -> ok;         %% Mnesia-related crash, expected in test env
            error:_ -> ok
        end,
        ?assertNotEqual({error, blocked_in_production}, Result)
    after
        cleanup(Saved)
    end.
