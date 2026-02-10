-module(iris_group_sender_key_rotation_tests).
-include_lib("eunit/include/eunit.hrl").

%% GAP-1: Helper to generate test keys ≥80 bytes (simulates E2EE-encrypted blob)
test_encrypted_key(Seed) ->
    %% SHA-512 gives 64 bytes; concatenate two to get 128 bytes
    H1 = crypto:hash(sha512, Seed),
    H2 = crypto:hash(sha512, <<Seed/binary, "_extra">>),
    <<H1/binary, H2/binary>>.

%% =============================================================================
%% GAP-2: Sender Key Rotation on Member Removal (Amendment 6.3)
%%
%% RFC-001-AMENDMENT-001 Section 6.3 item 4:
%% "On member removal: All remaining members generate new Sender Keys"
%%
%% The current implementation only deletes the removed member's keys.
%% Remaining members keep their old Sender Keys, allowing the removed
%% member (who recorded the key state) to decrypt future messages.
%%
%% These tests verify that ALL sender keys in the group are invalidated
%% when any member is removed, forcing remaining members to rotate.
%% =============================================================================

setup() ->
    %% Start Mnesia
    catch mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Start iris_group (creates tables)
    catch gen_server:stop(iris_group),
    timer:sleep(50),
    {ok, Pid} = iris_group:start_link(),
    {started, Pid}.

cleanup({started, _Pid}) ->
    catch gen_server:stop(iris_group),
    catch mnesia:stop(),
    timer:sleep(50),
    ok.

iris_group_sender_key_rotation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"All sender keys deleted on member removal",
       fun test_all_sender_keys_deleted_on_removal/0},
      {"Removed member's keys are gone",
       fun test_removed_member_keys_deleted/0},
      {"Remaining members' keys are also gone (force rotation)",
       fun test_remaining_member_keys_deleted/0}
     ]}.

test_all_sender_keys_deleted_on_removal() ->
    %% Create a group with 3 members
    {ok, GroupId} = iris_group:create_group(<<"TestGroup">>, <<"alice">>),
    ok = iris_group:add_member(GroupId, <<"bob">>, <<"alice">>),
    ok = iris_group:add_member(GroupId, <<"charlie">>, <<"alice">>),
    
    %% Each member distributes their sender key (GAP-1: must be ≥80 bytes)
    iris_group:store_sender_key(GroupId, <<"alice">>, <<"key_a1">>, test_encrypted_key(<<"alice">>)),
    iris_group:store_sender_key(GroupId, <<"bob">>, <<"key_b1">>, test_encrypted_key(<<"bob">>)),
    iris_group:store_sender_key(GroupId, <<"charlie">>, <<"key_c1">>, test_encrypted_key(<<"charlie">>)),
    
    %% Verify all keys exist
    ?assertMatch({ok, _}, iris_group:get_sender_key(GroupId, <<"alice">>, <<"key_a1">>)),
    ?assertMatch({ok, _}, iris_group:get_sender_key(GroupId, <<"bob">>, <<"key_b1">>)),
    ?assertMatch({ok, _}, iris_group:get_sender_key(GroupId, <<"charlie">>, <<"key_c1">>)),
    
    %% Remove charlie
    ok = iris_group:remove_member(GroupId, <<"charlie">>, <<"alice">>),
    
    %% KEY ASSERTION: ALL sender keys must be deleted, not just charlie's.
    %% This forces alice and bob to generate new sender keys.
    ?assertEqual({error, not_found}, iris_group:get_sender_key(GroupId, <<"alice">>, <<"key_a1">>)),
    ?assertEqual({error, not_found}, iris_group:get_sender_key(GroupId, <<"bob">>, <<"key_b1">>)),
    ?assertEqual({error, not_found}, iris_group:get_sender_key(GroupId, <<"charlie">>, <<"key_c1">>)).

test_removed_member_keys_deleted() ->
    %% Create group and add sender key for the member we'll remove
    {ok, GroupId} = iris_group:create_group(<<"TestGroup2">>, <<"admin">>),
    ok = iris_group:add_member(GroupId, <<"victim">>, <<"admin">>),
    
    iris_group:store_sender_key(GroupId, <<"victim">>, <<"vk1">>, test_encrypted_key(<<"victim">>)),
    ?assertMatch({ok, _}, iris_group:get_sender_key(GroupId, <<"victim">>, <<"vk1">>)),
    
    %% Remove victim
    ok = iris_group:remove_member(GroupId, <<"victim">>, <<"admin">>),
    
    %% Victim's keys must be gone
    ?assertEqual({error, not_found}, iris_group:get_sender_key(GroupId, <<"victim">>, <<"vk1">>)).

test_remaining_member_keys_deleted() ->
    %% Create group with admin and member
    {ok, GroupId} = iris_group:create_group(<<"TestGroup3">>, <<"admin">>),
    ok = iris_group:add_member(GroupId, <<"member1">>, <<"admin">>),
    ok = iris_group:add_member(GroupId, <<"member2">>, <<"admin">>),
    
    %% Admin and member1 distribute sender keys (GAP-1: must be ≥80 bytes)
    iris_group:store_sender_key(GroupId, <<"admin">>, <<"ak1">>, test_encrypted_key(<<"admin">>)),
    iris_group:store_sender_key(GroupId, <<"member1">>, <<"mk1">>, test_encrypted_key(<<"member1">>)),
    
    %% Remove member2 (who doesn't even have sender keys yet)
    ok = iris_group:remove_member(GroupId, <<"member2">>, <<"admin">>),
    
    %% KEY ASSERTION: Even admin's and member1's keys must be deleted.
    %% member2 may have recorded these keys before being removed.
    ?assertEqual({error, not_found}, iris_group:get_sender_key(GroupId, <<"admin">>, <<"ak1">>)),
    ?assertEqual({error, not_found}, iris_group:get_sender_key(GroupId, <<"member1">>, <<"mk1">>)).
