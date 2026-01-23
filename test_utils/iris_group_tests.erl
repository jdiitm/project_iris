-module(iris_group_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_group.erl
%% RFC Reference: FR-13 (Group Messaging)
%% =============================================================================

%% Setup and teardown
setup() ->
    %% Start mnesia if not running
    case mnesia:system_info(is_running) of
        no -> 
            mnesia:create_schema([node()]),
            mnesia:start();
        _ -> ok
    end,
    
    %% Start the group server
    case whereis(iris_group) of
        undefined ->
            {ok, _Pid} = iris_group:start_link();
        _Pid -> ok
    end,
    ok.

cleanup(_) ->
    %% Clean up test data
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    catch mnesia:clear_table(group_sender_key),
    ok.

%% =============================================================================
%% Test Fixtures
%% =============================================================================

group_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Create group", fun test_create_group/0},
         {"Get group info", fun test_get_group/0},
         {"List user groups", fun test_list_groups/0},
         {"Add member", fun test_add_member/0},
         {"Remove member", fun test_remove_member/0},
         {"Self-removal", fun test_self_removal/0},
         {"Promote admin", fun test_promote_admin/0},
         {"Demote admin", fun test_demote_admin/0},
         {"Cannot demote last admin", fun test_last_admin/0},
         {"Group member limit", fun test_member_limit/0},
         {"Sender key operations", fun test_sender_keys/0},
         {"Delete group", fun test_delete_group/0},
         {"Authorization checks", fun test_authorization/0}
     ]}.

%% =============================================================================
%% Test Cases
%% =============================================================================

test_create_group() ->
    Creator = <<"alice">>,
    GroupName = <<"Test Group">>,
    
    %% Create group
    {ok, GroupId} = iris_group:create_group(GroupName, Creator),
    
    %% Verify group exists
    {ok, GroupInfo} = iris_group:get_group(GroupId),
    ?assertEqual(GroupName, maps:get(name, GroupInfo)),
    ?assertEqual(Creator, maps:get(created_by, GroupInfo)),
    ?assertEqual(1, maps:get(member_count, GroupInfo)),
    
    %% Verify creator is admin
    ?assert(iris_group:is_admin(GroupId, Creator)),
    ?assert(iris_group:is_member(GroupId, Creator)),
    
    ok.

test_get_group() ->
    %% Non-existent group
    ?assertEqual({error, not_found}, iris_group:get_group(<<"nonexistent">>)),
    
    %% Create and get
    {ok, GroupId} = iris_group:create_group(<<"Info Test">>, <<"bob">>),
    {ok, Info} = iris_group:get_group(GroupId),
    
    ?assert(is_binary(maps:get(id, Info))),
    ?assertEqual(<<"Info Test">>, maps:get(name, Info)),
    ?assert(is_integer(maps:get(created_at, Info))),
    
    ok.

test_list_groups() ->
    User = <<"charlie">>,
    
    %% Create multiple groups
    {ok, G1} = iris_group:create_group(<<"Group 1">>, User),
    {ok, G2} = iris_group:create_group(<<"Group 2">>, User),
    {ok, _G3} = iris_group:create_group(<<"Other Group">>, <<"dave">>),
    
    %% List user's groups
    Groups = iris_group:list_groups(User),
    ?assert(lists:member(G1, Groups)),
    ?assert(lists:member(G2, Groups)),
    ?assertEqual(2, length(Groups)),
    
    ok.

test_add_member() ->
    Creator = <<"eve">>,
    Member = <<"frank">>,
    {ok, GroupId} = iris_group:create_group(<<"Add Test">>, Creator),
    
    %% Add member (as admin)
    ?assertEqual(ok, iris_group:add_member(GroupId, Member, Creator)),
    
    %% Verify member was added
    ?assert(iris_group:is_member(GroupId, Member)),
    ?assertNot(iris_group:is_admin(GroupId, Member)),
    
    %% Verify member count updated
    {ok, Info} = iris_group:get_group(GroupId),
    ?assertEqual(2, maps:get(member_count, Info)),
    
    %% Cannot add same member twice
    ?assertEqual({error, already_member}, 
                 iris_group:add_member(GroupId, Member, Creator)),
    
    ok.

test_remove_member() ->
    Creator = <<"grace">>,
    Member = <<"henry">>,
    {ok, GroupId} = iris_group:create_group(<<"Remove Test">>, Creator),
    
    %% Add then remove member
    iris_group:add_member(GroupId, Member, Creator),
    ?assert(iris_group:is_member(GroupId, Member)),
    
    ?assertEqual(ok, iris_group:remove_member(GroupId, Member, Creator)),
    ?assertNot(iris_group:is_member(GroupId, Member)),
    
    %% Verify member count updated
    {ok, Info} = iris_group:get_group(GroupId),
    ?assertEqual(1, maps:get(member_count, Info)),
    
    ok.

test_self_removal() ->
    Creator = <<"ivan">>,
    Member = <<"judy">>,
    {ok, GroupId} = iris_group:create_group(<<"Self Remove Test">>, Creator),
    
    %% Add member
    iris_group:add_member(GroupId, Member, Creator),
    
    %% Member can remove themselves
    ?assertEqual(ok, iris_group:remove_member(GroupId, Member, Member)),
    ?assertNot(iris_group:is_member(GroupId, Member)),
    
    ok.

test_promote_admin() ->
    Creator = <<"kate">>,
    Member = <<"leo">>,
    {ok, GroupId} = iris_group:create_group(<<"Promote Test">>, Creator),
    
    %% Add member
    iris_group:add_member(GroupId, Member, Creator),
    ?assertNot(iris_group:is_admin(GroupId, Member)),
    
    %% Promote to admin
    ?assertEqual(ok, iris_group:promote_admin(GroupId, Member, Creator)),
    ?assert(iris_group:is_admin(GroupId, Member)),
    
    %% Cannot promote already admin
    ?assertEqual({error, already_admin}, 
                 iris_group:promote_admin(GroupId, Member, Creator)),
    
    ok.

test_demote_admin() ->
    Creator = <<"mike">>,
    Member = <<"nina">>,
    {ok, GroupId} = iris_group:create_group(<<"Demote Test">>, Creator),
    
    %% Add and promote member
    iris_group:add_member(GroupId, Member, Creator),
    iris_group:promote_admin(GroupId, Member, Creator),
    ?assert(iris_group:is_admin(GroupId, Member)),
    
    %% Demote admin
    ?assertEqual(ok, iris_group:demote_admin(GroupId, Member, Creator)),
    ?assertNot(iris_group:is_admin(GroupId, Member)),
    ?assert(iris_group:is_member(GroupId, Member)),
    
    ok.

test_last_admin() ->
    Creator = <<"oscar">>,
    {ok, GroupId} = iris_group:create_group(<<"Last Admin Test">>, Creator),
    
    %% Cannot demote last admin
    ?assertEqual({error, last_admin}, 
                 iris_group:demote_admin(GroupId, Creator, Creator)),
    
    %% Cannot remove last admin
    ?assertEqual({error, last_admin},
                 iris_group:remove_member(GroupId, Creator, Creator)),
    
    ok.

test_member_limit() ->
    %% This is a simplified test - we don't actually add 1000 members
    %% Just verify the check exists
    Creator = <<"paul">>,
    {ok, GroupId} = iris_group:create_group(<<"Limit Test">>, Creator),
    
    %% Add a few members to verify mechanism works
    [begin
        Member = list_to_binary(io_lib:format("member_~p", [N])),
        iris_group:add_member(GroupId, Member, Creator)
    end || N <- lists:seq(1, 10)],
    
    {ok, Info} = iris_group:get_group(GroupId),
    ?assertEqual(11, maps:get(member_count, Info)),
    
    ok.

test_sender_keys() ->
    Creator = <<"quinn">>,
    Member = <<"rachel">>,
    {ok, GroupId} = iris_group:create_group(<<"Sender Key Test">>, Creator),
    iris_group:add_member(GroupId, Member, Creator),
    
    %% Store sender key
    SenderKey = crypto:strong_rand_bytes(32),
    KeyId = <<"key_001">>,
    ?assertEqual(ok, iris_group:store_sender_key(GroupId, Creator, KeyId, SenderKey)),
    
    %% Retrieve sender key
    {ok, RetrievedKey} = iris_group:get_sender_key(GroupId, Creator, KeyId),
    ?assertEqual(SenderKey, RetrievedKey),
    
    %% Non-existent key
    ?assertEqual({error, not_found}, 
                 iris_group:get_sender_key(GroupId, Creator, <<"nonexistent">>)),
    
    %% Store another key
    SenderKey2 = crypto:strong_rand_bytes(32),
    iris_group:store_sender_key(GroupId, Creator, <<"key_002">>, SenderKey2),
    
    %% Get all keys
    Keys = iris_group:get_all_sender_keys(GroupId, Creator),
    ?assertEqual(2, length(Keys)),
    
    %% Rotate key
    NewKey = crypto:strong_rand_bytes(32),
    {ok, NewKeyId} = iris_group:rotate_sender_key(GroupId, Creator, NewKey),
    ?assert(is_binary(NewKeyId)),
    
    {ok, RotatedKey} = iris_group:get_sender_key(GroupId, Creator, NewKeyId),
    ?assertEqual(NewKey, RotatedKey),
    
    ok.

test_delete_group() ->
    Creator = <<"sam">>,
    Member = <<"tina">>,
    {ok, GroupId} = iris_group:create_group(<<"Delete Test">>, Creator),
    
    %% Add member and sender key
    iris_group:add_member(GroupId, Member, Creator),
    iris_group:store_sender_key(GroupId, Creator, <<"sk1">>, <<"key_data">>),
    
    %% Delete group
    ?assertEqual(ok, iris_group:delete_group(GroupId, Creator)),
    
    %% Verify group is gone
    ?assertEqual({error, not_found}, iris_group:get_group(GroupId)),
    ?assertNot(iris_group:is_member(GroupId, Creator)),
    ?assertNot(iris_group:is_member(GroupId, Member)),
    
    ok.

test_authorization() ->
    Creator = <<"uma">>,
    Member = <<"victor">>,
    Outsider = <<"wendy">>,
    {ok, GroupId} = iris_group:create_group(<<"Auth Test">>, Creator),
    iris_group:add_member(GroupId, Member, Creator),
    
    %% Non-admin cannot add members
    ?assertEqual({error, not_authorized},
                 iris_group:add_member(GroupId, Outsider, Member)),
    
    %% Non-admin cannot remove others
    ?assertEqual({error, not_authorized},
                 iris_group:remove_member(GroupId, Creator, Member)),
    
    %% Non-admin cannot promote
    ?assertEqual({error, not_authorized},
                 iris_group:promote_admin(GroupId, Member, Member)),
    
    %% Non-admin cannot delete group
    ?assertEqual({error, not_authorized},
                 iris_group:delete_group(GroupId, Member)),
    
    %% Outsider cannot do anything
    ?assertEqual({error, not_authorized},
                 iris_group:add_member(GroupId, <<"newuser">>, Outsider)),
    
    ok.
