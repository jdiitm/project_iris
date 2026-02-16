-module(iris_key_change_delivery_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Key change notification must reach online contacts
%% =============================================================================
%%       silently when iris_async_router pool isn't running.
%% =============================================================================

key_change_delivery_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [fun online_contact_receives_key_change_alert/0,
      fun offline_contact_gets_alert_stored_for_later_delivery/0]}.

setup() ->
    %% Mnesia for iris_keys bundle storage + offline delivery tables
    Dir = "/tmp/iris_test_mnesia_kcd_" ++ integer_to_list(erlang:system_time(microsecond)),
    application:set_env(mnesia, dir, Dir),
    mnesia:create_schema([node()]),
    mnesia:start(),
    %% Create tables needed for offline delivery path (store_offline_durable)
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
    %% Use sync_transaction path (no batcher dependency)
    application:set_env(iris_core, multimaster_durability, true),
    %% Start iris_keys gen_server
    {ok, KeysPid} = iris_keys:start_link(),
    %% Start iris_metrics (needed by store_offline_durable)
    MetricsPid = case whereis(iris_metrics) of
        undefined -> case iris_metrics:start_link() of {ok, P} -> P; {error, {already_started, P}} -> P end;
        P -> P
    end,
    %% Start iris_dedup (needed by store_offline_durable)
    DedupPid = case whereis(iris_dedup) of
        undefined -> case iris_dedup:start_link() of {ok, P2} -> P2; {error, {already_started, P2}} -> P2 end;
        P2 -> P2
    end,
    %% Set up ETS presence table for iris_core:lookup_user (ETS backend)
    application:set_env(iris_core, presence_backend, ets),
    catch ets:new(presence_local, [set, named_table, public, {read_concurrency, true}]),
    {KeysPid, MetricsPid, DedupPid}.

teardown({KeysPid, _MetricsPid, _DedupPid}) ->
    gen_server:stop(KeysPid),
    catch ets:delete(presence_local),
    application:unset_env(iris_core, multimaster_durability),
    mnesia:stop(),
    ok.

online_contact_receives_key_change_alert() ->
    %% Step 1: Alice uploads an initial key bundle
    {AliceIK1, AlicePriv1} = iris_x3dh:generate_identity_key(),
    SPK1 = crypto:strong_rand_bytes(32),
    Sig1 = iris_x3dh:sign_prekey(SPK1, AlicePriv1),
    Bundle1 = #{
        identity_key => AliceIK1,
        signed_prekey => SPK1,
        signed_prekey_signature => Sig1,
        one_time_prekeys => [crypto:strong_rand_bytes(32)]
    },
    ok = iris_keys:upload_bundle(<<"alice">>, Bundle1),

    %% Step 2: Record bob as alice's contact
    iris_keys:record_key_contact(<<"alice">>, <<"bob">>),

    %% Step 3: Register test process as bob in presence (ETS backend)
    Now = erlang:system_time(millisecond),
    Entry = {presence_entry, <<"bob">>, node(), self(), Now, Now},
    ets:insert(presence_local, {<<"bob">>, Entry}),

    %% Step 4: Alice uploads a NEW bundle with a DIFFERENT identity key
    {AliceIK2, AlicePriv2} = iris_x3dh:generate_identity_key(),
    SPK2 = crypto:strong_rand_bytes(32),
    Sig2 = iris_x3dh:sign_prekey(SPK2, AlicePriv2),
    Bundle2 = #{
        identity_key => AliceIK2,
        signed_prekey => SPK2,
        signed_prekey_signature => Sig2,
        one_time_prekeys => [crypto:strong_rand_bytes(32)]
    },
    ok = iris_keys:upload_bundle(<<"alice">>, Bundle2),

    %% Step 5: Bob should receive a key_change_alert (opcode 0x1A)
    receive
        {deliver_msg, <<16#1A, _/binary>>} -> ok
    after 2000 ->
        ?assert(false)  %% Timeout = no notification sent
    end.

%% =============================================================================
%% RFC 5.3.2 CRITICAL FIX: Offline contacts must receive key change alert
%% on reconnect via durable offline storage.
%% =============================================================================
offline_contact_gets_alert_stored_for_later_delivery() ->
    %% Step 1: Alice uploads an initial key bundle
    {AliceIK1, AlicePriv1} = iris_x3dh:generate_identity_key(),
    SPK1 = crypto:strong_rand_bytes(32),
    Sig1 = iris_x3dh:sign_prekey(SPK1, AlicePriv1),
    Bundle1 = #{
        identity_key => AliceIK1,
        signed_prekey => SPK1,
        signed_prekey_signature => Sig1,
        one_time_prekeys => [crypto:strong_rand_bytes(32)]
    },
    ok = iris_keys:upload_bundle(<<"alice_offline">>, Bundle1),

    %% Step 2: Record carol as alice's contact
    iris_keys:record_key_contact(<<"alice_offline">>, <<"carol">>),

    %% Step 3: Carol is NOT registered in presence (simulating offline)
    %% Do NOT insert carol into presence_local ETS.

    %% Step 4: Alice uploads a NEW bundle with a DIFFERENT identity key
    {AliceIK2, AlicePriv2} = iris_x3dh:generate_identity_key(),
    SPK2 = crypto:strong_rand_bytes(32),
    Sig2 = iris_x3dh:sign_prekey(SPK2, AlicePriv2),
    Bundle2 = #{
        identity_key => AliceIK2,
        signed_prekey => SPK2,
        signed_prekey_signature => Sig2,
        one_time_prekeys => [crypto:strong_rand_bytes(32)]
    },
    ok = iris_keys:upload_bundle(<<"alice_offline">>, Bundle2),

    %% Step 5: Verify the key_change_alert was stored in offline_msg for carol
    %% retrieve_offline reads from all buckets for the user
    StoredMsgs = iris_core:retrieve_offline(<<"carol">>),
    %% Find the 0x1A key_change_alert packet in the stored messages
    Alerts = [M || M <- StoredMsgs, is_binary(M), byte_size(M) >= 1,
                   binary:first(M) =:= 16#1A],
    ?assertNotEqual([], Alerts),
    %% Verify the alert references alice_offline
    [AlertPacket | _] = Alerts,
    Expected = iris_proto:encode_key_change_alert(<<"alice_offline">>),
    ?assertEqual(Expected, AlertPacket).
