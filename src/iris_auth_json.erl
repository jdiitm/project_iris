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

-define(MAX_INPUT_SIZE, 8192).
-define(MAX_DEPTH, 32).  %% AUDIT: Prevent stack exhaustion from nested JSON

-spec decode(binary()) -> {ok, map()} | {error, invalid_json | input_too_large | max_depth_exceeded}.
decode(Bin) when byte_size(Bin) > ?MAX_INPUT_SIZE ->
    {error, input_too_large};
decode(Bin) ->
    try
        {Result, _Rest} = parse_object(Bin, 0),
        {ok, Result}
    catch
        throw:max_depth_exceeded -> {error, max_depth_exceeded};
        _:_ -> {error, invalid_json}
    end.

parse_object(<<"{", _Rest/binary>>, Depth) when Depth > ?MAX_DEPTH ->
    throw(max_depth_exceeded);
parse_object(<<"{", Rest/binary>>, Depth) ->
    parse_pairs(Rest, #{}, Depth).

parse_pairs(<<"}", Rest/binary>>, Acc, _Depth) ->
    {Acc, Rest};
parse_pairs(<<",", Rest/binary>>, Acc, Depth) ->
    parse_pairs(Rest, Acc, Depth);
parse_pairs(<<" ", Rest/binary>>, Acc, Depth) ->
    parse_pairs(Rest, Acc, Depth);
parse_pairs(<<"\"", Rest/binary>>, Acc, Depth) ->
    {Key, Rest2} = parse_string(Rest, []),
    Rest3 = skip_colon(Rest2),
    {Value, Rest4} = parse_value(Rest3, Depth),
    parse_pairs(Rest4, maps:put(Key, Value, Acc), Depth).

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

parse_value(<<" ", Rest/binary>>, Depth) ->
    parse_value(Rest, Depth);
parse_value(<<"{", _/binary>> = Bin, Depth) ->
    parse_object(Bin, Depth + 1);
parse_value(<<"\"", Rest/binary>>, _Depth) ->
    {Str, Rest2} = parse_string(Rest, []),
    {Str, Rest2};
parse_value(Bin, _Depth) ->
    parse_number(Bin, <<>>).

parse_number(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_number(Rest, <<Acc/binary, C>>);
parse_number(Rest, Acc) when byte_size(Acc) > 0 ->
    {binary_to_integer(Acc), Rest};
parse_number(Rest, <<>>) ->
    {null, Rest}.
