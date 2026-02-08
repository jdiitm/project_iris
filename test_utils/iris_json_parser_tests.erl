-module(iris_json_parser_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% TDD: JSON Parser Characterization Tests (Audit Finding 1.2)
%% =============================================================================
%% These tests capture the exact behavior of the JWT JSON parser.
%% Written BEFORE the iolist rewrite to lock in correct behavior.
%% The parser only needs to handle JWT payloads: flat objects with
%% string and integer values. No arrays, booleans, or nested objects.
%% =============================================================================

%% Test: Round-trip encode → decode for a typical JWT payload
jwt_round_trip_test() ->
    Claims = #{
        <<"sub">> => <<"alice">>,
        <<"iss">> => <<"iris">>,
        <<"iat">> => 1700000000,
        <<"exp">> => 1700086400,
        <<"jti">> => <<"abc123">>
    },
    Encoded = iris_auth_json:encode(Claims),
    {ok, Decoded} = iris_auth_json:decode(Encoded),
    ?assertEqual(maps:get(<<"sub">>, Claims), maps:get(<<"sub">>, Decoded)),
    ?assertEqual(maps:get(<<"iss">>, Claims), maps:get(<<"iss">>, Decoded)),
    ?assertEqual(maps:get(<<"iat">>, Claims), maps:get(<<"iat">>, Decoded)),
    ?assertEqual(maps:get(<<"exp">>, Claims), maps:get(<<"exp">>, Decoded)),
    ?assertEqual(maps:get(<<"jti">>, Claims), maps:get(<<"jti">>, Decoded)).

%% Test: Decode a known JSON string (external format)
decode_known_json_test() ->
    Json = <<"{\"sub\":\"bob\",\"exp\":999}">>,
    {ok, Map} = iris_auth_json:decode(Json),
    ?assertEqual(<<"bob">>, maps:get(<<"sub">>, Map)),
    ?assertEqual(999, maps:get(<<"exp">>, Map)).

%% Test: Encode produces valid JSON structure
encode_produces_valid_json_test() ->
    Map = #{<<"key">> => <<"value">>},
    Bin = iris_auth_json:encode(Map),
    ?assertEqual($\{, binary:first(Bin)),
    ?assertEqual($\}, binary:last(Bin)).

%% Test: Empty object
empty_object_test() ->
    {ok, Map} = iris_auth_json:decode(<<"{}">>),
    ?assertEqual(#{}, Map).

%% Test: String with escaped quote
escaped_quote_test() ->
    Json = <<"{\"k\":\"val\\\"ue\"}">>,
    {ok, Map} = iris_auth_json:decode(Json),
    ?assertEqual(<<"val\"ue">>, maps:get(<<"k">>, Map)).

%% Test: Multiple integer values
multiple_integers_test() ->
    Json = <<"{\"a\":1,\"b\":2,\"c\":3}">>,
    {ok, Map} = iris_auth_json:decode(Json),
    ?assertEqual(1, maps:get(<<"a">>, Map)),
    ?assertEqual(2, maps:get(<<"b">>, Map)),
    ?assertEqual(3, maps:get(<<"c">>, Map)).

%% Test: Spaces in JSON (whitespace tolerance)
whitespace_tolerance_test() ->
    Json = <<"{ \"k\" : \"v\" }">>,
    {ok, Map} = iris_auth_json:decode(Json),
    ?assertEqual(<<"v">>, maps:get(<<"k">>, Map)).

%% Test: Invalid JSON returns error
invalid_json_error_test() ->
    ?assertMatch({error, _}, iris_auth_json:decode(<<"not json">>)),
    ?assertMatch({error, _}, iris_auth_json:decode(<<"">>)).

%% Test: Atom values in encode (used for header "typ" => "JWT")
encode_atom_value_test() ->
    Map = #{<<"typ">> => 'JWT'},
    Bin = iris_auth_json:encode(Map),
    {ok, Decoded} = iris_auth_json:decode(Bin),
    ?assertEqual(<<"JWT">>, maps:get(<<"typ">>, Decoded)).
