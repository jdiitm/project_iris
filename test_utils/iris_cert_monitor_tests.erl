-module(iris_cert_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% TLS Certificate Expiry Monitoring Tests
%% =============================================================================
%%
%% RFC requires TLS for all connections. Without certificate expiry monitoring,
%% expired certificates will cause silent outages. iris_cert_monitor provides
%% check_expiry/1 which inspects a PEM-encoded certificate file and returns
%% {ok, DaysLeft} or {error, {expires_soon, DaysLeft}} when expiry is within
%% the warning threshold.
%% =============================================================================

%% =============================================================================
%% Test Helpers: Generate self-signed certificates with specific expiry
%% =============================================================================

%% Generate a self-signed cert that expires in `DaysFromNow` days.
%% Returns the PEM-encoded certificate as a binary.
generate_test_cert(DaysFromNow) ->
    %% Use openssl to generate a short-lived self-signed cert
    TmpKey = "/tmp/iris_test_cert_key.pem",
    TmpCert = "/tmp/iris_test_cert.pem",
    DaysStr = integer_to_list(max(1, DaysFromNow)),
    Cmd = "openssl req -x509 -newkey rsa:2048 -keyout " ++ TmpKey ++
          " -out " ++ TmpCert ++
          " -days " ++ DaysStr ++
          " -nodes -subj '/CN=iris_test' 2>/dev/null",
    os:cmd(Cmd),
    {ok, CertPem} = file:read_file(TmpCert),
    file:delete(TmpKey),
    file:delete(TmpCert),
    CertPem.

write_cert_file(CertPem, Path) ->
    ok = file:write_file(Path, CertPem).

%% =============================================================================
%% Core Tests
%% =============================================================================

cert_expiry_test_() ->
    [
     {"check_expiry returns ok for cert expiring in >30 days", fun() ->
          CertPem = generate_test_cert(365),
          CertFile = "/tmp/iris_test_long_cert.pem",
          write_cert_file(CertPem, CertFile),
          try
              Result = iris_cert_monitor:check_expiry(CertFile),
              ?assertMatch({ok, DaysLeft} when DaysLeft > 30, Result)
          after
              file:delete(CertFile)
          end
      end},

     {"check_expiry returns error for cert expiring in <30 days", fun() ->
          %% Generate cert that expires in 7 days
          CertPem = generate_test_cert(7),
          CertFile = "/tmp/iris_test_short_cert.pem",
          write_cert_file(CertPem, CertFile),
          try
              Result = iris_cert_monitor:check_expiry(CertFile),
              ?assertMatch({error, {expires_soon, DaysLeft}} when DaysLeft =< 30, Result)
          after
              file:delete(CertFile)
          end
      end},

     {"check_expiry returns error for non-existent cert file", fun() ->
          Result = iris_cert_monitor:check_expiry("/tmp/nonexistent_cert.pem"),
          ?assertMatch({error, _}, Result)
      end},

     {"check_expiry with custom threshold", fun() ->
          CertPem = generate_test_cert(60),
          CertFile = "/tmp/iris_test_threshold_cert.pem",
          write_cert_file(CertPem, CertFile),
          try
              %% With 90-day threshold, a 60-day cert should warn
              Result = iris_cert_monitor:check_expiry(CertFile, 90),
              ?assertMatch({error, {expires_soon, _}}, Result)
          after
              file:delete(CertFile)
          end
      end}
    ].
