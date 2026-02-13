-module(iris_config_validation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION P1-3: Configuration Validation at Startup
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
