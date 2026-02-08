-module(iris_auth_json).

%% =============================================================================
%% JWT JSON Encoder/Decoder
%% =============================================================================
%% Minimal JSON encoder/decoder for JWT payloads (flat objects with string
%% and integer values). Extracted from iris_auth.erl for testability.
%%
%% AUDIT FIX (Finding 1.2): Rewritten to use iolists instead of binary
%% append (<<Acc/binary, C>>) in parse_json_string/2. The old code was
%% O(N^2) per string; the new code is O(N). Note: This is a code-quality
%% fix, NOT a security fix — the protocol layer caps JWT input at 256 bytes.
%% =============================================================================

-export([encode/1, decode/1]).

%% =============================================================================
%% Encode
%% =============================================================================

-spec encode(map()) -> binary().
encode(Map) when is_map(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        KEnc = encode_value(K),
        VEnc = encode_value(V),
        [<<KEnc/binary, ":", VEnc/binary>> | Acc]
    end, [], Map),
    <<"{", (iolist_to_binary(lists:join(<<",">>, Pairs)))/binary, "}">>.

encode_value(V) when is_binary(V) ->
    <<"\"", V/binary, "\"">>;
encode_value(V) when is_integer(V) ->
    integer_to_binary(V);
encode_value(V) when is_atom(V) ->
    <<"\"", (atom_to_binary(V))/binary, "\"">>.

%% =============================================================================
%% Decode
%% =============================================================================

-spec decode(binary()) -> {ok, map()} | {error, invalid_json}.
decode(Bin) ->
    try
        {ok, parse_object(Bin)}
    catch
        _:_ -> {error, invalid_json}
    end.

parse_object(<<"{", Rest/binary>>) ->
    parse_pairs(Rest, #{}).

parse_pairs(<<"}", _/binary>>, Acc) ->
    Acc;
parse_pairs(<<",", Rest/binary>>, Acc) ->
    parse_pairs(Rest, Acc);
parse_pairs(<<" ", Rest/binary>>, Acc) ->
    parse_pairs(Rest, Acc);
parse_pairs(<<"\"", Rest/binary>>, Acc) ->
    {Key, Rest2} = parse_string(Rest, []),
    Rest3 = skip_colon(Rest2),
    {Value, Rest4} = parse_value(Rest3),
    parse_pairs(Rest4, maps:put(Key, Value, Acc)).

skip_colon(<<":", Rest/binary>>) -> Rest;
skip_colon(<<" ", Rest/binary>>) -> skip_colon(Rest);
skip_colon(Rest) -> Rest.

%% AUDIT FIX: iolist accumulator instead of <<Acc/binary, C>> (O(N) vs O(N^2))
parse_string(<<"\"", Rest/binary>>, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Rest};
parse_string(<<"\\\"", Rest/binary>>, Acc) ->
    parse_string(Rest, [$\" | Acc]);
parse_string(<<C, Rest/binary>>, Acc) ->
    parse_string(Rest, [C | Acc]).

parse_value(<<" ", Rest/binary>>) ->
    parse_value(Rest);
parse_value(<<"\"", Rest/binary>>) ->
    {Str, Rest2} = parse_string(Rest, []),
    {Str, Rest2};
parse_value(Bin) ->
    parse_number(Bin, <<>>).

parse_number(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_number(Rest, <<Acc/binary, C>>);
parse_number(Rest, Acc) when byte_size(Acc) > 0 ->
    {binary_to_integer(Acc), Rest};
parse_number(Rest, <<>>) ->
    {null, Rest}.
