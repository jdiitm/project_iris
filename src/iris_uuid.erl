-module(iris_uuid).

%% =============================================================================
%% UUIDv7 Validation (RFC-001 v4.0 Section 1.2, RFC 9562)
%% =============================================================================
%% Server MUST validate idempotency_key format:
%% - 16 bytes (128 bits)
%% - Version nibble = 7 (bits 48-51)
%% - Variant bits = 10 (bits 64-65)
%%
%% Format (128 bits):
%%   Bits 0-47:   48-bit Unix timestamp (milliseconds)
%%   Bits 48-51:  Version = 0b0111 (7)
%%   Bits 52-63:  12-bit random
%%   Bits 64-65:  Variant = 0b10
%%   Bits 66-127: 62-bit random
%% =============================================================================

-export([is_valid_v7/1]).
-export([validate_idempotency_key/1]).

%% @doc Validate a 16-byte binary as UUIDv7 per RFC 9562.
%% Returns true if valid, false otherwise.
-spec is_valid_v7(binary()) -> boolean().
is_valid_v7(<<_Timestamp:48, Version:4, _Rand1:12, Variant:2, _Rand2:62>>) ->
    Version =:= 7 andalso Variant =:= 2;  %% variant 0b10 = integer 2
is_valid_v7(_) ->
    false.

%% @doc Validate an idempotency key for RFC compliance.
%% Accepts: 16-byte raw binary UUIDv7, or 32-char hex string, or 36-char hyphenated UUID string.
%% Returns ok | {error, invalid_idempotency_key}.
-spec validate_idempotency_key(binary()) -> ok | {error, invalid_idempotency_key}.
validate_idempotency_key(Key) when is_binary(Key), byte_size(Key) =:= 16 ->
    %% Raw 16-byte binary
    case is_valid_v7(Key) of
        true -> ok;
        false -> {error, invalid_idempotency_key}
    end;
validate_idempotency_key(Key) when is_binary(Key), byte_size(Key) =:= 36 ->
    %% Hyphenated UUID string: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
    case parse_uuid_string(Key) of
        {ok, Bytes} -> validate_idempotency_key(Bytes);
        error -> {error, invalid_idempotency_key}
    end;
validate_idempotency_key(Key) when is_binary(Key), byte_size(Key) =:= 32 ->
    %% Hex string without hyphens
    case hex_to_bytes(Key) of
        {ok, Bytes} -> validate_idempotency_key(Bytes);
        error -> {error, invalid_idempotency_key}
    end;
validate_idempotency_key(_) ->
    {error, invalid_idempotency_key}.

%% =============================================================================
%% Internal
%% =============================================================================

parse_uuid_string(Str) ->
    %% Remove hyphens and parse as hex
    Clean = binary:replace(Str, <<"-">>, <<>>, [global]),
    case byte_size(Clean) of
        32 -> hex_to_bytes(Clean);
        _ -> error
    end.

hex_to_bytes(Hex) when byte_size(Hex) =:= 32 ->
    try
        Bytes = << <<(hex_char_to_int(H) * 16 + hex_char_to_int(L))>> 
                   || <<H, L>> <= Hex >>,
        {ok, Bytes}
    catch
        _:_ -> error
    end;
hex_to_bytes(_) ->
    error.

hex_char_to_int(C) when C >= $0, C =< $9 -> C - $0;
hex_char_to_int(C) when C >= $a, C =< $f -> C - $a + 10;
hex_char_to_int(C) when C >= $A, C =< $F -> C - $A + 10;
hex_char_to_int(_) -> error(invalid_hex).
