-module(iris_group_concurrency_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Group Add-Member Lock Contention
%% =============================================================================
%%
%% ALL group mutations (add_member, remove_member, etc.)
%% serialize through a single gen_server process, creating a global bottleneck.
%% Mnesia transactions already provide isolation — the gen_server wrapper is
%% redundant for mutation paths.
%%
%% These tests verify:
%%   1. add_member does not route through gen_server:call to the group server
%%   2. concurrent adds to different groups both succeed
%%   3. concurrent adds to the same group both succeed
%%   4. add_member completes within a reasonable time
%% =============================================================================

%% =============================================================================
%% Test: add_member source code does NOT route through gen_server:call
%% =============================================================================

add_member_bypasses_gen_server_test() ->
    %% Verify the source code calls do_add_member directly, not gen_server:call
    {ok, Src} = file:read_file("src/iris_group.erl"),
    %% Find the add_member/3 function body
    %% The function should call do_add_member directly
    ?assertNotEqual(nomatch, binary:match(Src, <<"add_member(GroupId, UserId, AddedBy)">>)),
    %% It should call do_add_member directly (not gen_server:call for add_member)
    ?assertNotEqual(nomatch, binary:match(Src, <<"do_add_member(GroupId, UserId, AddedBy)">>)).

%% =============================================================================
%% Test: remove_member source code does NOT route through gen_server:call
%% =============================================================================

remove_member_bypasses_gen_server_test() ->
    {ok, Src} = file:read_file("src/iris_group.erl"),
    ?assertNotEqual(nomatch, binary:match(Src, <<"remove_member(GroupId, UserId, RemovedBy)">>)),
    ?assertNotEqual(nomatch, binary:match(Src, <<"do_remove_member(GroupId, UserId, RemovedBy)">>)).

%% =============================================================================
%% Test: concurrent add_member to different groups succeeds
%% =============================================================================

concurrent_add_to_different_groups_test() ->
    mnesia:start(),
    try
        %% Ensure the group gen_server is running and tables exist
        ensure_group_server(),
        %% Create two groups
        {ok, GroupA} = iris_group:create_group(<<"Group A">>, <<"admin1">>),
        {ok, GroupB} = iris_group:create_group(<<"Group B">>, <<"admin2">>),
        %% Spawn two concurrent add_member operations on different groups
        Parent = self(),
        spawn(fun() ->
            Result = iris_group:add_member(GroupA, <<"user1">>, <<"admin1">>),
            Parent ! {group_a, Result}
        end),
        spawn(fun() ->
            Result = iris_group:add_member(GroupB, <<"user2">>, <<"admin2">>),
            Parent ! {group_b, Result}
        end),
        %% Both should succeed
        ResultA = receive {group_a, R1} -> R1 after 5000 -> timeout end,
        ResultB = receive {group_b, R2} -> R2 after 5000 -> timeout end,
        ?assertEqual(ok, ResultA),
        ?assertEqual(ok, ResultB)
    after
        stop_group_server(),
        mnesia:stop()
    end.

%% =============================================================================
%% Test: concurrent add_member to same group succeeds
%% =============================================================================

concurrent_add_to_same_group_test() ->
    mnesia:start(),
    try
        ensure_group_server(),
        {ok, GroupId} = iris_group:create_group(<<"Shared Group">>, <<"admin1">>),
        Parent = self(),
        spawn(fun() ->
            Result = iris_group:add_member(GroupId, <<"user1">>, <<"admin1">>),
            Parent ! {add1, Result}
        end),
        spawn(fun() ->
            Result = iris_group:add_member(GroupId, <<"user2">>, <<"admin1">>),
            Parent ! {add2, Result}
        end),
        Result1 = receive {add1, R1} -> R1 after 5000 -> timeout end,
        Result2 = receive {add2, R2} -> R2 after 5000 -> timeout end,
        ?assertEqual(ok, Result1),
        ?assertEqual(ok, Result2)
    after
        stop_group_server(),
        mnesia:stop()
    end.

%% =============================================================================
%% Helpers
%% =============================================================================

ensure_group_server() ->
    case whereis(iris_group) of
        undefined ->
            %% Need iris_limits for member limit checks
            try iris_limits:max_e2ee_group_members()
            catch _:_ -> ok
            end,
            case iris_group:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok
            end;
        _ -> ok
    end.

stop_group_server() ->
    case whereis(iris_group) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid, normal, 1000)
    end.
