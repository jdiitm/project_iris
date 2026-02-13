-module(iris_rfc_cbor_dedup_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% RFC-001 v4.0 Section 1.2: Idempotency Key-Based Deduplication Tests
%% =============================================================================
%% Requirement: "Server Atomically deduplicate by (user_id, idempotency_key)"
%%
%% The {idempotent_msg, IdempotencyKey, Payload} tuple must be recognized by
%% store_offline_durable_inner/2 and the dedup key must be derived from the
%% idempotency_key — NOT from the payload content hash.
%%
%% These tests are written BEFORE the fix and expected to FAIL.
%% =============================================================================

%% =============================================================================
%% Test Setup/Teardown
%% =============================================================================

setup() ->
    %% Unique Mnesia dir to avoid conflicts
    Dir = "/tmp/iris_test_mnesia_cbor_dedup_" ++ integer_to_list(erlang:system_time(microsecond)),
    application:set_env(mnesia, dir, Dir),
    mnesia:create_schema([node()]),
    mnesia:start(),

    %% Create tables required by store_offline_durable path
    mnesia:create_table(offline_msg, [
        {ram_copies, [node()]}, {attributes, [key, timestamp, msg]}, {type, bag}
    ]),
    mnesia:create_table(user_meta, [
        {ram_copies, [node()]}, {attributes, [user, bucket_count, last_modified]}
    ]),
    mnesia:create_table(dedup_log, [
        {ram_copies, [node()]}, {attributes, [msg_id, timestamp]}, {type, set}
    ]),
    mnesia:wait_for_tables([offline_msg, user_meta, dedup_log], 5000),

    %% Use sync_transaction path to avoid needing iris_durable_batcher
    application:set_env(iris_core, multimaster_durability, true),

    %% Start iris_dedup
    DedupRef = case whereis(iris_dedup) of
        undefined ->
            {ok, P} = iris_dedup:start_link(),
            {started, P};
        P ->
            {existing, P}
    end,

    %% Start iris_metrics (needed by dedup_hit counter)
    MetricsRef = case whereis(iris_metrics) of
        undefined ->
            case iris_metrics:start_link() of
                {ok, PM} -> {started, PM};
                {error, {already_started, PM}} -> {existing, PM}
            end;
        PM ->
            {existing, PM}
    end,

    {DedupRef, MetricsRef}.

cleanup({DedupRef, _MetricsRef}) ->
    case DedupRef of
        {started, Pid} -> catch gen_server:stop(Pid);
        _ -> ok
    end,
    application:unset_env(iris_core, multimaster_durability),
    mnesia:stop(),
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

rfc_cbor_dedup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"RFC 1.2: same idempotency_key + different payload = duplicate (dedup by key)",
         fun test_same_idem_key_different_payload_is_duplicate/0},
        {"RFC 1.2: different idempotency_key + same payload = both stored (not content hash)",
         fun test_different_idem_key_same_payload_both_stored/0},
        {"Backward compat: plain binary without idempotent_msg uses content hash",
         fun test_no_idem_key_falls_back_to_content_hash/0}
     ]}.

%% WS4: Cross-check that send_seq_v2 idempotency_key also gets deduped
rfc_send_seq_v2_dedup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"WS4: send_seq_v2 {idempotent_msg, Key, {SeqNo, Msg}} dedup by key",
         fun test_send_seq_v2_dedup_by_idempotency_key/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_same_idem_key_different_payload_is_duplicate() ->
    %% RFC 1.2: Dedup must be by (user_id, idempotency_key), NOT by content hash.
    %% Same key + different payload → second store must be detected as duplicate.
    %%
    %% Current code: store_offline_durable_inner/2 has no clause for
    %% {idempotent_msg, Key, Payload}. The whole tuple is hashed as content,
    %% so different payloads produce different hashes → both stored (BUG).
    User = <<"dedup_test_user_1">>,
    Key = <<"test-idem-key-001">>,

    Msg1 = {idempotent_msg, Key, <<"payload_v1">>},
    Msg2 = {idempotent_msg, Key, <<"payload_v2">>},

    %% First store
    R1 = iris_core:store_offline_durable(User, Msg1),
    ?assertEqual(ok, R1),

    %% Second store with SAME key but DIFFERENT payload
    R2 = iris_core:store_offline_durable(User, Msg2),
    ?assertEqual(ok, R2),

    %% Verify: the dedup key <<User/binary, ":", Key/binary>> should already
    %% have been marked by the first store call. If we check_and_mark it now,
    %% it should return 'duplicate' (proving the key was used for dedup).
    DedupKey = <<User/binary, ":", Key/binary>>,
    DedupResult = iris_dedup:check_and_mark(DedupKey),
    ?assertEqual(duplicate, DedupResult).

test_different_idem_key_same_payload_both_stored() ->
    %% RFC 1.2: Different idempotency keys with identical payload must NOT be
    %% deduped — they are distinct operations.
    User = <<"dedup_test_user_2">>,
    KeyA = <<"test-idem-key-aaa">>,
    KeyB = <<"test-idem-key-bbb">>,
    Payload = <<"identical_payload">>,

    Msg1 = {idempotent_msg, KeyA, Payload},
    Msg2 = {idempotent_msg, KeyB, Payload},

    %% Store both — both must succeed (different idempotency keys)
    R1 = iris_core:store_offline_durable(User, Msg1),
    ?assertEqual(ok, R1),
    R2 = iris_core:store_offline_durable(User, Msg2),
    ?assertEqual(ok, R2),

    %% Both keys should have been marked independently
    DedupKeyA = <<User/binary, ":", KeyA/binary>>,
    DedupKeyB = <<User/binary, ":", KeyB/binary>>,
    ?assertEqual(duplicate, iris_dedup:check_and_mark(DedupKeyA)),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(DedupKeyB)).

test_no_idem_key_falls_back_to_content_hash() ->
    %% Backward compatibility: plain binary (no {idempotent_msg, ...} wrapper)
    %% must still use existing content hash dedup.
    User = <<"dedup_test_user_3">>,
    Msg = <<"plain_message_body">>,

    %% First store
    R1 = iris_core:store_offline_durable(User, Msg),
    ?assertEqual(ok, R1),

    %% Second store of same content — deduped by content hash, returns ok
    R2 = iris_core:store_offline_durable(User, Msg),
    ?assertEqual(ok, R2).

test_send_seq_v2_dedup_by_idempotency_key() ->
    %% WS4 cross-check: The {idempotent_msg, Key, {SeqNo, Msg}} format
    %% (as produced by send_seq_v2 handler) must be deduped by key, not content.
    %% This verifies the WS2 implementation handles both CBOR and send_seq_v2 paths.
    User = <<"dedup_test_user_4">>,
    Key = <<"test-seq-v2-key-x">>,

    Msg1 = {idempotent_msg, Key, {1, <<"payload_a">>}},
    Msg2 = {idempotent_msg, Key, {2, <<"payload_b">>}},

    %% First store — new
    R1 = iris_core:store_offline_durable(User, Msg1),
    ?assertEqual(ok, R1),

    %% Second store with same key but different SeqNo/payload — must be duplicate
    R2 = iris_core:store_offline_durable(User, Msg2),
    ?assertEqual(ok, R2),

    %% Verify key was used for dedup
    DedupKey = <<User/binary, ":", Key/binary>>,
    ?assertEqual(duplicate, iris_dedup:check_and_mark(DedupKey)).
