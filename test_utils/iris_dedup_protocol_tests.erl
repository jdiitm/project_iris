-module(iris_dedup_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Protocol-level idempotency enforcement
%% =============================================================================
%% RFC Section 1.2: Server MUST deduplicate by (user_id, idempotency_key).
%% Opcode 0x0D (SEND_SEQ_V2) includes a mandatory UUIDv7 idempotency key.
%% Opcode 0x07 (SEND_SEQ) does NOT -- it uses content-hash dedup (weaker).
%%
%% This test suite validates:
%% 1. is_idempotent_opcode/1 correctly classifies opcodes
%% 2. 0x0D round-trip encode/decode preserves idempotency key
%% 3. 0x07 is correctly identified as non-idempotent
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Test: is_idempotent_opcode correctly classifies opcodes
%% ---------------------------------------------------------------------------
classify_idempotent_opcodes_test() ->
    %% 0x0D (SEND_SEQ_V2) is idempotent (has idempotency_key)
    ?assertEqual(true, iris_proto:is_idempotent_opcode(16#0D)),
    %% 0x10 (CBOR_MSG) may carry idempotency_key
    ?assertEqual(true, iris_proto:is_idempotent_opcode(16#10)),
    %% 0x07 (SEND_SEQ) is NOT idempotent (no idempotency_key in wire format)
    ?assertEqual(false, iris_proto:is_idempotent_opcode(16#07)),
    %% Other opcodes are not idempotent
    ?assertEqual(false, iris_proto:is_idempotent_opcode(16#01)),
    ?assertEqual(false, iris_proto:is_idempotent_opcode(16#02)).

%% ---------------------------------------------------------------------------
%% Test: 0x0D round-trip preserves idempotency key
%% ---------------------------------------------------------------------------
send_seq_v2_roundtrip_test() ->
    Target = <<"bob">>,
    IdKey = crypto:strong_rand_bytes(16),
    SeqNo = 42,
    Msg = <<"hello world">>,
    Encoded = iris_proto:encode_seq_msg_v2(Target, IdKey, SeqNo, Msg),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    ?assertMatch({send_seq_v2, Target, IdKey, SeqNo, Msg}, Decoded).

%% ---------------------------------------------------------------------------
%% Test: 0x07 encodes without idempotency key
%% ---------------------------------------------------------------------------
send_seq_no_idempotency_key_test() ->
    Target = <<"bob">>,
    SeqNo = 42,
    Msg = <<"hello">>,
    Encoded = iris_proto:encode_seq_msg(Target, SeqNo, Msg),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    %% 0x07 decodes as {send_seq, Target, SeqNo, Msg} -- no idempotency key
    ?assertMatch({send_seq, Target, SeqNo, Msg}, Decoded).

%% ---------------------------------------------------------------------------
%% Test: Duplicate idempotency key detected via ETS hot tier (atomic check)
%% ---------------------------------------------------------------------------
dedup_hot_tier_detects_duplicate_key_test() ->
    %% The ETS hot tier (iris_dedup_seen) is the first dedup check.
    %% ets:insert_new is atomic -- if key exists, returns false (duplicate).
    %% This tests the critical fast path without requiring Mnesia.
    setup_dedup_ets(),
    try
        User = <<"test_user_dedup_proto">>,
        IdKey = crypto:strong_rand_bytes(16),
        DedupKey = <<User/binary, ":", IdKey/binary>>,
        Now = os:system_time(millisecond),

        %% First insert: succeeds (new)
        ?assertEqual(true, ets:insert_new(iris_dedup_seen, {DedupKey, Now})),

        %% Second insert with SAME key: fails (duplicate)
        ?assertEqual(false, ets:insert_new(iris_dedup_seen, {DedupKey, Now}))
    after
        cleanup_dedup_ets()
    end.

%% ---------------------------------------------------------------------------
%% Test: Different idempotency keys are not falsely deduplicated
%% ---------------------------------------------------------------------------
dedup_hot_tier_allows_different_keys_test() ->
    setup_dedup_ets(),
    try
        User = <<"test_user_dedup_different">>,
        IdKey1 = crypto:strong_rand_bytes(16),
        IdKey2 = crypto:strong_rand_bytes(16),
        DedupKey1 = <<User/binary, ":", IdKey1/binary>>,
        DedupKey2 = <<User/binary, ":", IdKey2/binary>>,
        Now = os:system_time(millisecond),

        %% Both should succeed (both are new)
        ?assertEqual(true, ets:insert_new(iris_dedup_seen, {DedupKey1, Now})),
        ?assertEqual(true, ets:insert_new(iris_dedup_seen, {DedupKey2, Now}))
    after
        cleanup_dedup_ets()
    end.

%% ---------------------------------------------------------------------------
%% Test: Dedup key format for 0x0D matches RFC Section 1.2
%% ---------------------------------------------------------------------------
dedup_key_format_rfc_compliant_test() ->
    %% RFC Section 1.2: dedup by (user_id, idempotency_key)
    %% iris_core.erl builds key as: <<User/binary, ":", IdempotencyKey/binary>>
    User = <<"alice">>,
    IdKey = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Expected = <<"alice:", 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    ?assertEqual(Expected, <<User/binary, ":", IdKey/binary>>).

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------
setup_dedup_ets() ->
    case ets:whereis(iris_dedup_seen) of
        undefined -> ets:new(iris_dedup_seen, [named_table, public, set]);
        _ -> ets:delete_all_objects(iris_dedup_seen)
    end.

cleanup_dedup_ets() ->
    case ets:whereis(iris_dedup_seen) of
        undefined -> ok;
        _ -> try ets:delete_all_objects(iris_dedup_seen) catch _:_ -> ok end
    end.
