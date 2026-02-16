#!/usr/bin/env escript
%% -*- erlang -*-
%% =============================================================================
%% AUDIT V2 P0-1: Production Config Validation Script
%% =============================================================================
%% Validates that a production config file is safe to deploy.
%%
%% Checks:
%%   1. deployment_mode = production (both iris_core and iris_edge)
%%   2. expected_cluster_nodes is non-empty
%%   3. join_seeds is non-empty
%%   4. core_nodes is non-empty
%%   5. jwt_secret is not the placeholder value
%%
%% Usage:
%%   escript scripts/validate_production_config.escript config/production.config
%%
%% Exit codes:
%%   0 = all checks pass
%%   1 = one or more checks failed
%%   2 = usage error / file not found
%% =============================================================================

main([ConfigPath]) ->
    case file:consult(ConfigPath) of
        {ok, [Config]} ->
            Errors = validate(Config),
            case Errors of
                [] ->
                    io:format("OK: All production config checks passed.~n"),
                    halt(0);
                _ ->
                    io:format("FAIL: Production config validation failed:~n"),
                    lists:foreach(fun(E) ->
                        io:format("  - ~s~n", [lists:flatten(E)])
                    end, Errors),
                    halt(1)
            end;
        {ok, _Other} ->
            io:format("ERROR: Config file has unexpected structure~n"),
            halt(2);
        {error, Reason} ->
            io:format("ERROR: Cannot read config file ~s: ~p~n", [ConfigPath, Reason]),
            halt(2)
    end;
main(_) ->
    io:format("Usage: escript validate_production_config.escript <config_file>~n"),
    halt(2).

validate(Config) ->
    CoreConfig = proplists:get_value(iris_core, Config, []),
    EdgeConfig = proplists:get_value(iris_edge, Config, []),
    check_deployment_mode(CoreConfig, "iris_core") ++
    check_deployment_mode(EdgeConfig, "iris_edge") ++
    check_non_empty_list(CoreConfig, expected_cluster_nodes, "iris_core.expected_cluster_nodes") ++
    check_non_empty_list(CoreConfig, join_seeds, "iris_core.join_seeds") ++
    check_non_empty_list(EdgeConfig, core_nodes, "iris_edge.core_nodes") ++
    check_jwt_secret(EdgeConfig) ++
    check_metrics_token(CoreConfig).

check_deployment_mode(AppConfig, AppName) ->
    case proplists:get_value(deployment_mode, AppConfig, undefined) of
        production -> [];
        undefined -> [lists:flatten(io_lib:format("~s.deployment_mode is not set (must be 'production')", [AppName]))];
        Other -> [lists:flatten(io_lib:format("~s.deployment_mode is '~p' (must be 'production')", [AppName, Other]))]
    end.

check_non_empty_list(AppConfig, Key, Label) ->
    case proplists:get_value(Key, AppConfig, undefined) of
        undefined -> [lists:flatten(io_lib:format("~s is not set", [Label]))];
        [] -> [lists:flatten(io_lib:format("~s is empty (must contain at least one node)", [Label]))];
        L when is_list(L), length(L) > 0 -> [];
        Other -> [lists:flatten(io_lib:format("~s has unexpected value: ~p", [Label, Other]))]
    end.

check_jwt_secret(EdgeConfig) ->
    case proplists:get_value(jwt_secret, EdgeConfig, undefined) of
        undefined -> [];  %% Not set in config = OK (will use env var)
        <<"REPLACE_WITH_32_BYTE_SECRET_KEY!!">> ->
            ["iris_edge.jwt_secret is the placeholder value (must be replaced or removed)"];
        _ -> []
    end.

check_metrics_token(CoreConfig) ->
    case proplists:get_value(metrics_bearer_token, CoreConfig, undefined) of
        undefined -> [];  %% Not set = OK
        <<"REPLACE_WITH_METRICS_TOKEN">> ->
            ["iris_core.metrics_bearer_token is the placeholder value (must be replaced)"];
        _ -> []
    end.
