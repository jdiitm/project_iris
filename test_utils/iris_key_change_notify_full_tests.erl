-module(iris_key_change_notify_full_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% TDD: Key Change Notification Tests (GAP-13, P2)
%% =============================================================================
%% Written BEFORE implementation. These tests define the contract for:
%%   - iris_keys:record_key_contact/2 — track who fetched whose keys
%%   - iris_keys:get_key_contacts/1   — retrieve contacts for a user
%%   - iris_proto:encode_key_change_alert/1 — encode notification opcode 0x1A
%%   - iris_proto:decode key_change_alert   — decode notification opcode 0x1A
%% =============================================================================

-define(CONTACTS_TABLE, iris_key_contacts).

setup() ->
    %% Create the ETS table that the implementation will use
    case ets:info(?CONTACTS_TABLE) of
        undefined -> ets:new(?CONTACTS_TABLE, [bag, named_table, public]);
        _ -> ets:delete_all_objects(?CONTACTS_TABLE)
    end,
    ok.

cleanup(_) ->
    catch ets:delete_all_objects(?CONTACTS_TABLE),
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
