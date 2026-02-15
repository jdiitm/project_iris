-module(iris_tls_config_tests).
-include_lib("eunit/include/eunit.hrl").

%% Guard tests: no config file may permit TLS 1.2.
%% RFC NFR-14 mandates TLS 1.3 only.

no_tls12_in_test_config_test() ->
    {ok, Src} = file:read_file("config/test_tls.config"),
    ?assertEqual(nomatch, binary:match(Src, <<"tlsv1.2">>)).

no_tls12_in_mtls_config_test() ->
    {ok, Src} = file:read_file("config/test_mtls.config"),
    ?assertEqual(nomatch, binary:match(Src, <<"tlsv1.2">>)).

no_tls12_in_dist_config_test() ->
    {ok, Src} = file:read_file("config/ssl_dist.conf"),
    ?assertEqual(nomatch, binary:match(Src, <<"tlsv1.2">>)).
