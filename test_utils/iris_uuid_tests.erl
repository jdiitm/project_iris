-module(iris_uuid_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% UUIDv7 Validation Tests (RFC-001 v4.0 Section 1.2)
%% =============================================================================

%% Helper: Build a valid UUIDv7 binary (16 bytes).
%% Timestamp: 48 bits, Version: 4 bits = 7, Rand1: 12 bits,
%% Variant: 2 bits = 0b10, Rand2: 62 bits.
make_uuidv7() ->
    Ts = os:system_time(millisecond),
    Rand1 = rand:uniform(4096) - 1,   %% 12 bits
    Rand2 = rand:uniform(1 bsl 62) - 1,  %% 62 bits
    <<Ts:48, 7:4, Rand1:12, 2:2, Rand2:62>>.

%% Helper: Build a UUIDv4 binary (version=4, variant=0b10).
make_uuidv4() ->
    Rand1_high = rand:uniform(1 bsl 48) - 1,
    Rand1_low = rand:uniform(4096) - 1,
    Rand2 = rand:uniform(1 bsl 62) - 1,
    <<Rand1_high:48, 4:4, Rand1_low:12, 2:2, Rand2:62>>.

%% Helper: Convert 16-byte binary to 32-char hex string.
bytes_to_hex(Bytes) ->
    list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bytes]).

%% Helper: Convert 16-byte binary to hyphenated UUID string.
bytes_to_uuid_string(<<A:4/binary, B:2/binary, C:2/binary, D:2/binary, E:6/binary>>) ->
    <<(bytes_to_hex(A))/binary, "-",
      (bytes_to_hex(B))/binary, "-",
      (bytes_to_hex(C))/binary, "-",
      (bytes_to_hex(D))/binary, "-",
      (bytes_to_hex(E))/binary>>.

%% =============================================================================
%% is_valid_v7 tests
%% =============================================================================

is_valid_v7_test_() ->
    [
     {"Valid UUIDv7 binary accepted", fun() ->
         V7 = make_uuidv7(),
         ?assert(iris_uuid:is_valid_v7(V7))
     end},
     {"UUIDv4 binary rejected", fun() ->
         V4 = make_uuidv4(),
         ?assertNot(iris_uuid:is_valid_v7(V4))
     end},
     {"Too short binary rejected", fun() ->
         ?assertNot(iris_uuid:is_valid_v7(<<1,2,3,4,5,6,7,8>>))
     end},
     {"Too long binary rejected", fun() ->
         ?assertNot(iris_uuid:is_valid_v7(<<0:136>>))
     end},
     {"Empty binary rejected", fun() ->
         ?assertNot(iris_uuid:is_valid_v7(<<>>))
     end},
     {"All zeros rejected (version=0)", fun() ->
         ?assertNot(iris_uuid:is_valid_v7(<<0:128>>))
     end},
     {"All ones rejected (version=F, variant=11)", fun() ->
         ?assertNot(iris_uuid:is_valid_v7(<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:128>>))
     end}
    ].

%% =============================================================================
%% validate_idempotency_key tests
%% =============================================================================

validate_idempotency_key_test_() ->
    [
     {"Raw 16-byte UUIDv7 accepted", fun() ->
         V7 = make_uuidv7(),
         ?assertEqual(ok, iris_uuid:validate_idempotency_key(V7))
     end},
     {"32-char hex UUIDv7 accepted", fun() ->
         V7 = make_uuidv7(),
         Hex = bytes_to_hex(V7),
         ?assertEqual(ok, iris_uuid:validate_idempotency_key(Hex))
     end},
     {"36-char hyphenated UUIDv7 accepted", fun() ->
         V7 = make_uuidv7(),
         Str = bytes_to_uuid_string(V7),
         ?assertEqual(ok, iris_uuid:validate_idempotency_key(Str))
     end},
     {"UUIDv4 rejected", fun() ->
         V4 = make_uuidv4(),
         ?assertEqual({error, invalid_idempotency_key},
                      iris_uuid:validate_idempotency_key(V4))
     end},
     {"Arbitrary string rejected", fun() ->
         ?assertEqual({error, invalid_idempotency_key},
                      iris_uuid:validate_idempotency_key(<<"not-a-uuid">>))
     end},
     {"Empty binary rejected", fun() ->
         ?assertEqual({error, invalid_idempotency_key},
                      iris_uuid:validate_idempotency_key(<<>>))
     end},
     {"Non-binary rejected", fun() ->
         ?assertEqual({error, invalid_idempotency_key},
                      iris_uuid:validate_idempotency_key(12345))
     end}
    ].
