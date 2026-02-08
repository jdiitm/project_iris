-module(iris_key_change_delivery_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Phase 6 TDD: Key change notification must reach online contacts
%% =============================================================================
%% RED:  detect_identity_key_change uses iris_router:route, which fails
%%       silently when iris_async_router pool isn't running.
%% GREEN: Change to direct pid delivery via iris_core:lookup_user.
%% =============================================================================

key_change_delivery_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [fun online_contact_receives_key_change_alert/0]}.

setup() ->
    %% Mnesia for iris_keys bundle storage
    Dir = "/tmp/iris_test_mnesia_kcd_" ++ integer_to_list(erlang:system_time(microsecond)),
    application:set_env(mnesia, dir, Dir),
    mnesia:create_schema([node()]),
    mnesia:start(),
    %% Start iris_keys gen_server
    {ok, KeysPid} = iris_keys:start_link(),
    %% Set up ETS presence table for iris_core:lookup_user (ETS backend)
    application:set_env(iris_core, presence_backend, ets),
    catch ets:new(presence_local, [set, named_table, public, {read_concurrency, true}]),
    KeysPid.

teardown(KeysPid) ->
    gen_server:stop(KeysPid),
    catch ets:delete(presence_local),
    mnesia:stop(),
    ok.

online_contact_receives_key_change_alert() ->
    %% Step 1: Alice uploads an initial key bundle
    AliceIK1 = crypto:strong_rand_bytes(32),
    Bundle1 = #{
        identity_key => AliceIK1,
        signed_prekey => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64),
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
    AliceIK2 = crypto:strong_rand_bytes(32),
    Bundle2 = #{
        identity_key => AliceIK2,
        signed_prekey => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64),
        one_time_prekeys => [crypto:strong_rand_bytes(32)]
    },
    ok = iris_keys:upload_bundle(<<"alice">>, Bundle2),

    %% Step 5: Bob should receive a key_change_alert (opcode 0x1A)
    receive
        {deliver_msg, <<16#1A, _/binary>>} -> ok
    after 2000 ->
        ?assert(false)  %% Timeout = no notification sent
    end.
