-module(iris_key_change_notify_full_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Key Change Notification Tests
%% =============================================================================
%% Written BEFORE implementation. These tests define the contract for:
%%   - iris_keys:record_key_contact/2 — track who fetched whose keys
%%   - iris_keys:get_key_contacts/1   — retrieve contacts for a user
%%   - iris_proto:encode_key_change_alert/1 — encode notification opcode 0x1A
%%   - iris_proto:decode key_change_alert   — decode notification opcode 0x1A
%% =============================================================================

setup() ->
    %% FIX: key contacts now use Mnesia, not ETS.
    %% Start Mnesia and create the key_contact table for unit tests.
    catch mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    catch mnesia:create_table(key_contact, [
        {attributes, [owner, fetcher]},
        {ram_copies, [node()]},
        {type, bag}
    ]),
    mnesia:wait_for_tables([key_contact], 5000),
    ok.

cleanup(_) ->
    catch mnesia:clear_table(key_contact),
    catch mnesia:stop(),
    ok.

%% Test: Recording a contact and retrieving it
fetch_records_contact_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        iris_keys:record_key_contact(<<"alice">>, <<"bob">>),
        Contacts = iris_keys:get_key_contacts(<<"alice">>),
        ?assert(lists:member(<<"bob">>, Contacts))
    end}.

%% Test: Multiple contacts are tracked
multiple_contacts_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        iris_keys:record_key_contact(<<"alice">>, <<"bob">>),
        iris_keys:record_key_contact(<<"alice">>, <<"carol">>),
        Contacts = iris_keys:get_key_contacts(<<"alice">>),
        ?assert(lists:member(<<"bob">>, Contacts)),
        ?assert(lists:member(<<"carol">>, Contacts)),
        ?assertEqual(2, length(Contacts))
    end}.

%% Test: No contacts for unknown user returns empty list
no_contacts_for_unknown_user_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Contacts = iris_keys:get_key_contacts(<<"unknown">>),
        ?assertEqual([], Contacts)
    end}.

%% Test: Duplicate contact is not recorded twice
no_duplicate_contacts_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        iris_keys:record_key_contact(<<"alice">>, <<"bob">>),
        iris_keys:record_key_contact(<<"alice">>, <<"bob">>),
        Contacts = iris_keys:get_key_contacts(<<"alice">>),
        ?assertEqual(1, length(Contacts))
    end}.

%% Test: Encode key_change_alert produces binary with opcode 0x1A
encode_key_change_alert_test() ->
    UserId = <<"alice">>,
    Encoded = iris_proto:encode_key_change_alert(UserId),
    <<Opcode:8, UserLen:16, User:UserLen/binary>> = Encoded,
    ?assertEqual(16#1A, Opcode),
    ?assertEqual(UserId, User).

%% Test: Decode key_change_alert roundtrip
decode_key_change_alert_roundtrip_test() ->
    UserId = <<"bob">>,
    Encoded = iris_proto:encode_key_change_alert(UserId),
    {Decoded, <<>>} = iris_proto:decode(Encoded),
    ?assertEqual({key_change_alert, UserId}, Decoded).

%% =============================================================================
%% Integration: iris_session fetch_prekeys must record key contact
%% =============================================================================

session_setup() ->
    %% Start Mnesia for iris_keys
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia),
    %% Start iris_keys gen_server (creates tables + ETS)
    {ok, KeysPid} = iris_keys:start_link(),
    %% Ensure contacts table is clean
    catch mnesia:clear_table(key_contact),
    %% Ensure rate limiter is running (needed by iris_session)
    RlPid = case whereis(iris_rate_limiter) of
        undefined ->
            case iris_rate_limiter:start_link() of
                {ok, P} -> P;
                {error, {already_started, P}} -> P;
                _ -> undefined
            end;
        P -> P
    end,
    %% Disable auth for this test
    application:set_env(iris_edge, auth_enabled, false),
    {KeysPid, RlPid}.

session_cleanup({KeysPid, _RlPid}) ->
    catch gen_server:stop(KeysPid, normal, 1000),
    catch mnesia:clear_table(key_contact),
    application:unset_env(iris_edge, auth_enabled),
    application:stop(mnesia),
    ok.

%% Test: When bob fetches alice's prekeys via iris_session, alice's key_contacts
%% must contain bob. This proves the session layer wires fetch_bundle/3 (not /1).
session_fetch_records_contact_test_() ->
    {setup, fun session_setup/0, fun session_cleanup/1, fun() ->
        %% Upload a key bundle for alice
        AliceBundle = #{
            identity_key => crypto:strong_rand_bytes(32),
            signed_prekey => crypto:strong_rand_bytes(32),
            signed_prekey_signature => crypto:strong_rand_bytes(64),
            one_time_prekeys => [crypto:strong_rand_bytes(32)]
        },
        ok = iris_keys:upload_bundle(<<"alice">>, AliceBundle),

        %% Bob fetches alice's prekeys through the session layer
        _Result = iris_session:handle_packet(
            {fetch_prekeys, <<"alice">>}, <<"bob">>, self(), iris_edge_conn),

        %% Assert: alice's key_contacts must now contain bob
        Contacts = iris_keys:get_key_contacts(<<"alice">>),
        ?assert(lists:member(<<"bob">>, Contacts))
    end}.
