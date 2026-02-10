-module(iris_group_size_limits_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% A2/NFR-27: Group Size Limits Tests
%%
%% iris_group.erl uses a local -define(MAX_GROUP_MEMBERS, 1000) for broadcast
%% groups instead of iris_limits:max_broadcast_group_members() which returns
%% 10,000. This test verifies the broadcast limit matches iris_limits.
%%
%% RED: A broadcast group with member_count=1000 should still accept new
%%      members (limit is 10000). Current code rejects at 1000.
%% GREEN: Replace ?MAX_GROUP_MEMBERS with iris_limits:max_broadcast_group_members().
%% =============================================================================

-record(group, {
    id          :: binary(),
    name        :: binary(),
    created_at  :: integer(),
    created_by  :: binary(),
    member_count :: integer()
}).

-record(group_member, {
    key         :: {binary(), binary()},
    role        :: admin | member,
    joined_at   :: integer(),
    added_by    :: binary(),
    last_seen   :: integer()
}).

iris_group_size_limits_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"broadcast group at 1000 members accepts one more (limit should be 10000)",
       fun test_broadcast_allows_member_1001/0},
      {"broadcast group full error reports iris_limits value",
       fun test_broadcast_full_reports_correct_limit/0},
      {"max_members/0 delegates to iris_limits (no local defines)",
       fun test_max_members_uses_iris_limits/0}
     ]}.

setup() ->
    %% Stop any existing iris_group
    catch gen_server:stop(iris_group),
    timer:sleep(50),

    %% Start Mnesia
    application:stop(mnesia),
    application:set_env(mnesia, dir, "/tmp/iris_a2_group_size_test_" ++
                        integer_to_list(erlang:system_time(millisecond))),
    mnesia:create_schema([node()]),
    application:start(mnesia),

    %% Start iris_group (creates tables)
    {ok, Pid} = iris_group:start_link(),
    mnesia:wait_for_tables([group, group_member, group_sender_key], 5000),
    {started, Pid}.

cleanup({started, Pid}) ->
    catch gen_server:stop(Pid),
    timer:sleep(50),
    catch mnesia:delete_table(group),
    catch mnesia:delete_table(group_member),
    catch mnesia:delete_table(group_sender_key),
    application:stop(mnesia),
    ok.

test_broadcast_allows_member_1001() ->
    %% Create a group and set member_count to 1000 via direct Mnesia write
    {ok, GroupId} = iris_group:create_group(<<"test_broadcast">>, <<"admin_user">>),

    %% The group starts with member_count=1 (creator). Set it to 1000.
    [Group] = mnesia:dirty_read(group, GroupId),
    FakedGroup = Group#group{member_count = 1000},
    mnesia:dirty_write(group, FakedGroup),

    %% Verify it's at 1000
    {ok, #{member_count := 1000}} = iris_group:get_group(GroupId),

    %% No sender keys => broadcast group => limit should be 10000
    ?assertNot(iris_group:has_sender_keys(GroupId)),

    %% Try to add member 1001 - should succeed if limit is 10000
    NewUser = <<"user_1001">>,
    Result = iris_group:add_member(GroupId, NewUser, <<"admin_user">>),
    ?assertEqual(ok, Result).

test_broadcast_full_reports_correct_limit() ->
    %% Create a group and set member_count to 10000 (at the real limit)
    {ok, GroupId} = iris_group:create_group(<<"test_full">>, <<"admin_user_2">>),

    [Group] = mnesia:dirty_read(group, GroupId),
    FakedGroup = Group#group{member_count = 10000},
    mnesia:dirty_write(group, FakedGroup),

    %% At limit 10000, adding should fail with group_full
    Result = iris_group:add_member(GroupId, <<"overflow_user">>, <<"admin_user_2">>),
    ?assertMatch({error, {group_full, #{limit := 10000, type := broadcast}}}, Result).

%% Verify iris_group:max_members/0 delegates to iris_limits, not a local define.
%% This prevents future auditors from flagging stale local macros.
test_max_members_uses_iris_limits() ->
    ?assertEqual(iris_limits:max_e2ee_group_members(), iris_group:max_members()).
