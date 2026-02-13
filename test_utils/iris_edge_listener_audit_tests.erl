-module(iris_edge_listener_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation Tests for iris_edge_listener.erl (TDD RED phase)
%% =============================================================================
%%
%% Tests cover:
%% - 7.2: TLS certificate expiry detection (days_until_cert_expiry/1)
%% =============================================================================

%% =============================================================================
%% 7.2: Certificate Expiry Detection
%% =============================================================================

cert_expiry_test_() ->
    [
     {"returns integer days for valid cert file", fun() ->
          CertFile = "certs/edge-east-1.pem",
          case filelib:is_file(CertFile) of
              true ->
                  Result = iris_edge_listener:days_until_cert_expiry(CertFile),
                  ?assert(is_integer(Result)),
                  %% This cert expires 2027-01-21; should be > 0 today
                  ?assert(Result > 0);
              false ->
                  %% CI without certs directory
                  ok
          end
      end},

     {"returns negative days for expired cert", fun() ->
          CertFile = "certs/expired.pem",
          case filelib:is_file(CertFile) of
              true ->
                  Result = iris_edge_listener:days_until_cert_expiry(CertFile),
                  ?assert(is_integer(Result)),
                  %% Expired cert (notAfter 2026-01-20) should be negative
                  ?assert(Result < 0);
              false ->
                  ok
          end
      end},

     {"returns error tuple for missing file", fun() ->
          ?assertEqual({error, enoent},
                       iris_edge_listener:days_until_cert_expiry("/nonexistent/cert.pem"))
      end}
    ].
