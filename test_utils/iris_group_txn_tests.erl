-module(iris_group_txn_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Group Transaction Durability Tests
%% =============================================================================
%%
%% Tests verify that iris_group.erl operations use mnesia:transaction
%% instead of dirty_write. Specifically:
%% - store_sender_key uses transaction for sender key durability
%% - update_member_last_seen uses transaction for member state durability
%% - promote_admin uses transaction for role change durability
%% - demote_admin uses transaction for role change durability
%% - Source code contains mnesia:transaction, not dirty_write
%% =============================================================================

%% Helper to generate test keys ≥80 bytes (simulates E2EE-encrypted blob)
test_encrypted_key(Seed) ->
    H1 = crypto:hash(sha512, Seed),
    H2 = crypto:hash(sha512, <<Seed/binary, "_extra">>),
    <<H1/binary, H2/binary>>.

setup() ->
    case mnesia:system_info(is_running) of
        no ->
            mnesia:create_schema([node()]),
            mnesia:start();
        _ -> ok
    end,
    case whereis(iris_group) of
        undefined ->
            {ok, Pid} = iris_group:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, Pid}) ->
    gen_server:stop(Pid),
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    catch mnesia:clear_table(group_sender_key),
    ok;
cleanup({existing, _}) ->
    catch mnesia:clear_table(group),
    catch mnesia:clear_table(group_member),
    catch mnesia:clear_table(group_sender_key),
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_group_txn_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Source uses mnesia:transaction, not dirty_write",
       fun test_source_uses_transactions/0},
      {"store_sender_key writes transactionally",
       fun test_store_sender_key_transactional/0},
      {"update_member_last_seen writes transactionally",
       fun test_update_member_last_seen_transactional/0},
      {"promote_admin writes transactionally",
       fun test_promote_admin_transactional/0},
      {"demote_admin writes transactionally",
       fun test_demote_admin_transactional/0},
      {"sender key is retrievable after transactional store",
       fun test_sender_key_round_trip/0},
      {"role changes persist correctly",
       fun test_role_change_persistence/0}
     ]}.

%% =============================================================================
%% Source code analysis
%% =============================================================================

test_source_uses_transactions() ->
    {ok, Src} = file:read_file("src/iris_group.erl"),
    %% Must contain mnesia:transaction
    ?assert(binary:match(Src, <<"mnesia:transaction">>) =/= nomatch),
    %% Should NOT contain dirty_write for sender keys, member updates, or role changes
    Lines = binary:split(Src, <<"\n">>, [global]),
    DirtyWriteLines = [L || L <- Lines,
        binary:match(L, <<"dirty_write">>) =/= nomatch],
    %% No dirty_write should remain in the module
    ?assertEqual([], DirtyWriteLines).

%% =============================================================================
%% Transactional writes
%% =============================================================================

test_store_sender_key_transactional() ->
    Creator = <<"txn_creator_sk">>,
    {ok, GroupId} = iris_group:create_group(<<"TxnSKGroup">>, Creator),
    KeyId = <<"sk_key_1">>,
    SenderKey = test_encrypted_key(<<"txn_sender_key">>),
    ok = iris_group:store_sender_key(GroupId, Creator, KeyId, SenderKey),
    %% Verify key was stored
    {ok, Retrieved} = iris_group:get_sender_key(GroupId, Creator, KeyId),
    ?assertEqual(SenderKey, Retrieved).

test_update_member_last_seen_transactional() ->
    Creator = <<"txn_creator_ls">>,
    {ok, GroupId} = iris_group:create_group(<<"TxnLSGroup">>, Creator),
    %% Add a member to update
    Member = <<"txn_member_ls">>,
    ok = iris_group:add_member(GroupId, Member, Creator),
    %% Update last_seen
    ok = iris_group:update_member_last_seen(GroupId, Member),
    %% Verify member still exists (no crash from transactional write)
    ?assert(iris_group:is_member(GroupId, Member)).

test_promote_admin_transactional() ->
    Creator = <<"txn_creator_pa">>,
    {ok, GroupId} = iris_group:create_group(<<"TxnPAGroup">>, Creator),
    Member = <<"txn_member_pa">>,
    ok = iris_group:add_member(GroupId, Member, Creator),
    %% Member is not admin initially
    ?assertNot(iris_group:is_admin(GroupId, Member)),
    %% Promote
    ok = iris_group:promote_admin(GroupId, Member, Creator),
    ?assert(iris_group:is_admin(GroupId, Member)).

test_demote_admin_transactional() ->
    Creator = <<"txn_creator_da">>,
    {ok, GroupId} = iris_group:create_group(<<"TxnDAGroup">>, Creator),
    Member = <<"txn_member_da">>,
    ok = iris_group:add_member(GroupId, Member, Creator),
    ok = iris_group:promote_admin(GroupId, Member, Creator),
    ?assert(iris_group:is_admin(GroupId, Member)),
    %% Demote
    ok = iris_group:demote_admin(GroupId, Member, Creator),
    ?assertNot(iris_group:is_admin(GroupId, Member)).

test_sender_key_round_trip() ->
    Creator = <<"txn_creator_rt">>,
    {ok, GroupId} = iris_group:create_group(<<"TxnRTGroup">>, Creator),
    Key1 = test_encrypted_key(<<"round_trip_key_1">>),
    Key2 = test_encrypted_key(<<"round_trip_key_2">>),
    ok = iris_group:store_sender_key(GroupId, Creator, <<"k1">>, Key1),
    ok = iris_group:store_sender_key(GroupId, Creator, <<"k2">>, Key2),
    %% Both keys retrievable
    {ok, R1} = iris_group:get_sender_key(GroupId, Creator, <<"k1">>),
    {ok, R2} = iris_group:get_sender_key(GroupId, Creator, <<"k2">>),
    ?assertEqual(Key1, R1),
    ?assertEqual(Key2, R2),
    %% Get all sender keys
    AllKeys = iris_group:get_all_sender_keys(GroupId, Creator),
    ?assertEqual(2, length(AllKeys)).

test_role_change_persistence() ->
    Creator = <<"txn_creator_rp">>,
    {ok, GroupId} = iris_group:create_group(<<"TxnRPGroup">>, Creator),
    M1 = <<"txn_m1_rp">>,
    M2 = <<"txn_m2_rp">>,
    ok = iris_group:add_member(GroupId, M1, Creator),
    ok = iris_group:add_member(GroupId, M2, Creator),
    %% Promote both
    ok = iris_group:promote_admin(GroupId, M1, Creator),
    ok = iris_group:promote_admin(GroupId, M2, Creator),
    ?assert(iris_group:is_admin(GroupId, M1)),
    ?assert(iris_group:is_admin(GroupId, M2)),
    %% Demote M1
    ok = iris_group:demote_admin(GroupId, M1, Creator),
    ?assertNot(iris_group:is_admin(GroupId, M1)),
    ?assert(iris_group:is_admin(GroupId, M2)).
