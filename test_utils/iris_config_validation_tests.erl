-module(iris_config_validation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Configuration Validation at Startup
%% Verifies that dangerous config values are rejected or clamped.
%% =============================================================================

%% Test: validate_num_acceptors clamps excessive values
acceptor_count_upper_bound_test() ->
    ?assertEqual(10000, iris_edge_app:validate_num_acceptors(100000)),
    ?assertEqual(500, iris_edge_app:validate_num_acceptors(500)),
    ?assertEqual(1, iris_edge_app:validate_num_acceptors(0)),
    ?assertEqual(1, iris_edge_app:validate_num_acceptors(-5)),
    ?assertEqual(10000, iris_edge_app:validate_num_acceptors(10001)).

%% Test: validate_rate_limits rejects inverted rate limits
rate_limit_sanity_test() ->
    %% Burst must be >= rate
    ?assertEqual(ok, iris_edge_app:validate_rate_limits(5, 20)),
    ?assertEqual(ok, iris_edge_app:validate_rate_limits(5, 5)),
    ?assertEqual({error, burst_less_than_rate}, iris_edge_app:validate_rate_limits(20, 5)).

%% Test: validate_tls_cert detects missing cert files
tls_cert_existence_test() ->
    ?assertEqual({error, cert_not_found}, iris_edge_app:validate_tls_cert("/nonexistent/path.pem")).

%% Test: validate_replication_factor rejects zero/negative
replication_factor_bounds_test() ->
    ?assertEqual(ok, iris_edge_app:validate_replication_factor(3)),
    ?assertEqual(ok, iris_edge_app:validate_replication_factor(1)),
    ?assertEqual({error, invalid_replication_factor}, iris_edge_app:validate_replication_factor(0)),
    ?assertEqual({error, invalid_replication_factor}, iris_edge_app:validate_replication_factor(-1)).

%% =============================================================================
%% Production Secret Safety
%% Characterization tests verifying the existing fail-fast chain via source
%% inspection. Cannot call validate_production_config() directly because
%% it calls init:stop(1) which would kill the test node.
%% =============================================================================

%% Test: validate_production_config source code contains jwt_secret_missing exit
%% (Characterization: the fail-fast chain exists and exits with the right reason)
production_mode_rejects_missing_secret_test() ->
    {ok, Source} = file:read_file("src/iris_edge_app.erl"),
    Text = binary_to_list(Source),
    %% Must contain the exit for missing jwt_secret
    ?assertNot(string:find(Text, "exit(jwt_secret_missing)") =:= nomatch),
    %% Must check IRIS_JWT_SECRET env var as fallback
    ?assertNot(string:find(Text, "IRIS_JWT_SECRET") =:= nomatch),
    %% Must check auth_enabled before requiring secret
    ?assertNot(string:find(Text, "auth_enabled") =:= nomatch).

%% Test: production.config file does not contain a plaintext placeholder secret
placeholder_secret_rejected_test() ->
    {ok, Content} = file:read_file("config/production.config"),
    Text = binary_to_list(Content),
    %% The placeholder string must NOT appear uncommented in production config
    Lines = string:split(Text, "\n", all),
    ActiveLines = [L || L <- Lines,
                   not is_comment_line(L)],
    ActiveText = lists:flatten(ActiveLines),
    ?assertNot(string:find(ActiveText, "REPLACE_WITH_32_BYTE_SECRET_KEY") =/= nomatch).

%% Helper: check if a line is a comment (starts with %% after optional whitespace)
is_comment_line(Line) ->
    Trimmed = string:trim(Line, leading),
    case Trimmed of
        [$% | _] -> true;
        _ -> false
    end.
