-module(iris_group).
-behaviour(gen_server).

%% =============================================================================
%% Group Membership Management
%% =============================================================================
%% Purpose: Manage group creation, membership, and permissions.
%% RFC Reference: FR-13 (Group Messaging), RFC-001-AMENDMENT-001
%%
%% Design Principles:
%% 1. Groups are identified by unique binary IDs
%% 2. Creator becomes first admin with full permissions
%% 3. Admins can add/remove members and promote/demote admins
%% 4. Members can only send messages to the group
%% 5. Maximum 1000 members per group (per RFC)
%% 6. Group metadata stored in Mnesia for durability
%% =============================================================================

-export([start_link/0]).

%% API
-export([
    create_group/2,           %% (GroupName, CreatorId) -> {ok, GroupId} | {error, Reason}
    delete_group/2,           %% (GroupId, UserId) -> ok | {error, Reason}
    get_group/1,              %% (GroupId) -> {ok, GroupInfo} | {error, not_found}
    list_groups/1,            %% (UserId) -> [GroupId]
    
    %% Membership
    add_member/3,             %% (GroupId, UserId, AddedBy) -> ok | {error, Reason}
    remove_member/3,          %% (GroupId, UserId, RemovedBy) -> ok | {error, Reason}
    get_members/1,            %% (GroupId) -> {ok, [MemberInfo]} | {error, Reason}
    is_member/2,              %% (GroupId, UserId) -> boolean()
    
    %% Admin management
    promote_admin/3,          %% (GroupId, UserId, PromotedBy) -> ok | {error, Reason}
    demote_admin/3,           %% (GroupId, UserId, DemotedBy) -> ok | {error, Reason}
    is_admin/2,               %% (GroupId, UserId) -> boolean()
    
    %% Sender Keys (for E2EE groups)
    store_sender_key/4,       %% (GroupId, UserId, KeyId, SenderKey) -> ok
    get_sender_key/3,         %% (GroupId, UserId, KeyId) -> {ok, SenderKey} | {error, not_found}
    get_all_sender_keys/2,    %% (GroupId, UserId) -> [{KeyId, SenderKey}]
    rotate_sender_key/3       %% (GroupId, UserId, NewSenderKey) -> {ok, KeyId}
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(MAX_GROUP_MEMBERS, 1000).
-define(MAX_GROUP_NAME_LEN, 256).
-define(MAX_GROUPS_PER_USER, 100).

%% =============================================================================
%% Mnesia Table Definitions
%% =============================================================================

%% Group metadata
-record(group, {
    id          :: binary(),           %% Unique group ID
    name        :: binary(),           %% Human-readable name
    created_at  :: integer(),          %% Unix timestamp
    created_by  :: binary(),           %% Creator's user ID
    member_count = 0 :: integer()      %% Cached member count
}).

%% Group membership
-record(group_member, {
    key         :: {binary(), binary()}, %% {GroupId, UserId}
    role        :: admin | member,       %% Role in group
    joined_at   :: integer(),            %% Unix timestamp
    added_by    :: binary()              %% Who added this member
}).

%% Group sender keys (for E2EE)
-record(group_sender_key, {
    key         :: {binary(), binary(), binary()}, %% {GroupId, UserId, KeyId}
    sender_key  :: binary(),            %% The sender key
    created_at  :: integer(),           %% Unix timestamp
    chain_index :: integer()            %% Current chain index
}).

%% =============================================================================
%% API Functions
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create a new group. Creator becomes the first admin.
-spec create_group(binary(), binary()) -> {ok, binary()} | {error, term()}.
create_group(GroupName, CreatorId) when is_binary(GroupName), is_binary(CreatorId) ->
    case byte_size(GroupName) > ?MAX_GROUP_NAME_LEN of
        true -> {error, name_too_long};
        false -> gen_server:call(?SERVER, {create_group, GroupName, CreatorId})
    end.

%% @doc Delete a group. Only admins can delete.
-spec delete_group(binary(), binary()) -> ok | {error, term()}.
delete_group(GroupId, UserId) ->
    gen_server:call(?SERVER, {delete_group, GroupId, UserId}).

%% @doc Get group information.
-spec get_group(binary()) -> {ok, map()} | {error, not_found}.
get_group(GroupId) ->
    case mnesia:dirty_read(group, GroupId) of
        [#group{id = Id, name = Name, created_at = CreatedAt, 
                created_by = CreatedBy, member_count = Count}] ->
            {ok, #{
                id => Id,
                name => Name,
                created_at => CreatedAt,
                created_by => CreatedBy,
                member_count => Count
            }};
        [] ->
            {error, not_found}
    end.

%% @doc List all groups a user belongs to.
-spec list_groups(binary()) -> [binary()].
list_groups(UserId) ->
    try
        MatchSpec = [{
            #group_member{key = {'$1', UserId}, _ = '_'},
            [],
            ['$1']
        }],
        mnesia:dirty_select(group_member, MatchSpec)
    catch
        exit:{aborted, {no_exists, _}} -> [];
        error:badarg -> []
    end.

%% @doc Add a member to a group. Only admins can add members.
-spec add_member(binary(), binary(), binary()) -> ok | {error, term()}.
add_member(GroupId, UserId, AddedBy) ->
    gen_server:call(?SERVER, {add_member, GroupId, UserId, AddedBy}).

%% @doc Remove a member from a group. Admins can remove anyone; members can remove themselves.
-spec remove_member(binary(), binary(), binary()) -> ok | {error, term()}.
remove_member(GroupId, UserId, RemovedBy) ->
    gen_server:call(?SERVER, {remove_member, GroupId, UserId, RemovedBy}).

%% @doc Get all members of a group.
-spec get_members(binary()) -> {ok, [map()]} | {error, term()}.
get_members(GroupId) ->
    case mnesia:dirty_read(group, GroupId) of
        [] -> {error, not_found};
        [_] ->
            MatchSpec = [{
                #group_member{key = {GroupId, '$1'}, role = '$2', 
                             joined_at = '$3', added_by = '$4'},
                [],
                [#{user_id => '$1', role => '$2', joined_at => '$3', added_by => '$4'}]
            }],
            Members = mnesia:dirty_select(group_member, MatchSpec),
            {ok, Members}
    end.

%% @doc Check if a user is a member of a group.
-spec is_member(binary(), binary()) -> boolean().
is_member(GroupId, UserId) ->
    case mnesia:dirty_read(group_member, {GroupId, UserId}) of
        [_] -> true;
        [] -> false
    end.

%% @doc Promote a member to admin. Only admins can promote.
-spec promote_admin(binary(), binary(), binary()) -> ok | {error, term()}.
promote_admin(GroupId, UserId, PromotedBy) ->
    gen_server:call(?SERVER, {promote_admin, GroupId, UserId, PromotedBy}).

%% @doc Demote an admin to member. Only admins can demote; cannot demote last admin.
-spec demote_admin(binary(), binary(), binary()) -> ok | {error, term()}.
demote_admin(GroupId, UserId, DemotedBy) ->
    gen_server:call(?SERVER, {demote_admin, GroupId, UserId, DemotedBy}).

%% @doc Check if a user is an admin of a group.
-spec is_admin(binary(), binary()) -> boolean().
is_admin(GroupId, UserId) ->
    case mnesia:dirty_read(group_member, {GroupId, UserId}) of
        [#group_member{role = admin}] -> true;
        _ -> false
    end.

%% =============================================================================
%% Sender Key Functions (for E2EE Group Messaging)
%% =============================================================================

%% @doc Store a sender key for a user in a group.
-spec store_sender_key(binary(), binary(), binary(), binary()) -> ok.
store_sender_key(GroupId, UserId, KeyId, SenderKey) ->
    Record = #group_sender_key{
        key = {GroupId, UserId, KeyId},
        sender_key = SenderKey,
        created_at = erlang:system_time(second),
        chain_index = 0
    },
    mnesia:dirty_write(group_sender_key, Record),
    ok.

%% @doc Get a specific sender key.
-spec get_sender_key(binary(), binary(), binary()) -> {ok, binary()} | {error, not_found}.
get_sender_key(GroupId, UserId, KeyId) ->
    case mnesia:dirty_read(group_sender_key, {GroupId, UserId, KeyId}) of
        [#group_sender_key{sender_key = Key}] -> {ok, Key};
        [] -> {error, not_found}
    end.

%% @doc Get all sender keys for a user in a group.
-spec get_all_sender_keys(binary(), binary()) -> [{binary(), binary()}].
get_all_sender_keys(GroupId, UserId) ->
    MatchSpec = [{
        #group_sender_key{key = {GroupId, UserId, '$1'}, sender_key = '$2', _ = '_'},
        [],
        [{{'$1', '$2'}}]
    }],
    mnesia:dirty_select(group_sender_key, MatchSpec).

%% @doc Rotate sender key for a user. Generates a new KeyId.
-spec rotate_sender_key(binary(), binary(), binary()) -> {ok, binary()}.
rotate_sender_key(GroupId, UserId, NewSenderKey) ->
    KeyId = generate_key_id(),
    store_sender_key(GroupId, UserId, KeyId, NewSenderKey),
    {ok, KeyId}.

%% =============================================================================
%% gen_server Callbacks
%% =============================================================================

init([]) ->
    %% Initialize Mnesia tables if not exists
    init_tables(),
    {ok, #{}}.

handle_call({create_group, GroupName, CreatorId}, _From, State) ->
    Result = do_create_group(GroupName, CreatorId),
    {reply, Result, State};

handle_call({delete_group, GroupId, UserId}, _From, State) ->
    Result = do_delete_group(GroupId, UserId),
    {reply, Result, State};

handle_call({add_member, GroupId, UserId, AddedBy}, _From, State) ->
    Result = do_add_member(GroupId, UserId, AddedBy),
    {reply, Result, State};

handle_call({remove_member, GroupId, UserId, RemovedBy}, _From, State) ->
    Result = do_remove_member(GroupId, UserId, RemovedBy),
    {reply, Result, State};

handle_call({promote_admin, GroupId, UserId, PromotedBy}, _From, State) ->
    Result = do_promote_admin(GroupId, UserId, PromotedBy),
    {reply, Result, State};

handle_call({demote_admin, GroupId, UserId, DemotedBy}, _From, State) ->
    Result = do_demote_admin(GroupId, UserId, DemotedBy),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

init_tables() ->
    %% Create group table
    case mnesia:create_table(group, [
        {attributes, record_info(fields, group)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, group}} -> ok;
        {aborted, Reason1} -> 
            logger:warning("Failed to create group table: ~p", [Reason1])
    end,
    
    %% Create group_member table
    case mnesia:create_table(group_member, [
        {attributes, record_info(fields, group_member)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, group_member}} -> ok;
        {aborted, Reason2} ->
            logger:warning("Failed to create group_member table: ~p", [Reason2])
    end,
    
    %% Create group_sender_key table
    case mnesia:create_table(group_sender_key, [
        {attributes, record_info(fields, group_sender_key)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, group_sender_key}} -> ok;
        {aborted, Reason3} ->
            logger:warning("Failed to create group_sender_key table: ~p", [Reason3])
    end,
    
    %% Add secondary index on group_member for user lookups
    catch mnesia:add_table_index(group_member, role),
    
    %% Wait for all tables to be available
    Tables = [group, group_member, group_sender_key],
    case mnesia:wait_for_tables(Tables, 5000) of
        ok -> ok;
        {timeout, BadTables} ->
            logger:warning("Timeout waiting for tables: ~p", [BadTables]),
            %% Try to force load
            lists:foreach(fun(T) -> catch mnesia:force_load_table(T) end, BadTables),
            ok;
        {error, Reason} ->
            logger:error("Failed to wait for tables: ~p", [Reason]),
            ok
    end.

do_create_group(GroupName, CreatorId) ->
    %% Check user's group limit
    UserGroups = list_groups(CreatorId),
    case length(UserGroups) >= ?MAX_GROUPS_PER_USER of
        true -> {error, too_many_groups};
        false ->
            GroupId = generate_group_id(),
            Now = erlang:system_time(second),
            
            %% Create group and add creator as admin
            F = fun() ->
                %% Create group record
                Group = #group{
                    id = GroupId,
                    name = GroupName,
                    created_at = Now,
                    created_by = CreatorId,
                    member_count = 1
                },
                mnesia:write(group, Group, write),
                
                %% Add creator as admin
                Member = #group_member{
                    key = {GroupId, CreatorId},
                    role = admin,
                    joined_at = Now,
                    added_by = CreatorId
                },
                mnesia:write(group_member, Member, write),
                
                {ok, GroupId}
            end,
            
            case mnesia:transaction(F) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, Reason}
            end
    end.

do_delete_group(GroupId, UserId) ->
    %% Only admins can delete
    case is_admin(GroupId, UserId) of
        false -> {error, not_authorized};
        true ->
            F = fun() ->
                %% Delete all members
                MatchSpec = [{
                    #group_member{key = {GroupId, '_'}, _ = '_'},
                    [],
                    ['$_']
                }],
                Members = mnesia:select(group_member, MatchSpec, write),
                [mnesia:delete_object(group_member, M, write) || M <- Members],
                
                %% Delete all sender keys
                KeyMatchSpec = [{
                    #group_sender_key{key = {GroupId, '_', '_'}, _ = '_'},
                    [],
                    ['$_']
                }],
                Keys = mnesia:select(group_sender_key, KeyMatchSpec, write),
                [mnesia:delete_object(group_sender_key, K, write) || K <- Keys],
                
                %% Delete group
                mnesia:delete(group, GroupId, write),
                ok
            end,
            
            case mnesia:transaction(F) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> {error, Reason}
            end
    end.

do_add_member(GroupId, UserId, AddedBy) ->
    %% Check if AddedBy is an admin
    case is_admin(GroupId, AddedBy) of
        false -> {error, not_authorized};
        true ->
            %% Check if user is already a member
            case is_member(GroupId, UserId) of
                true -> {error, already_member};
                false ->
                    %% Check member limit
                    case get_group(GroupId) of
                        {error, not_found} -> {error, group_not_found};
                        {ok, #{member_count := Count}} when Count >= ?MAX_GROUP_MEMBERS ->
                            {error, group_full};
                        {ok, _} ->
                            Now = erlang:system_time(second),
                            F = fun() ->
                                %% Add member
                                Member = #group_member{
                                    key = {GroupId, UserId},
                                    role = member,
                                    joined_at = Now,
                                    added_by = AddedBy
                                },
                                mnesia:write(group_member, Member, write),
                                
                                %% Update member count
                                [Group] = mnesia:read(group, GroupId, write),
                                UpdatedGroup = Group#group{
                                    member_count = Group#group.member_count + 1
                                },
                                mnesia:write(group, UpdatedGroup, write),
                                ok
                            end,
                            
                            case mnesia:transaction(F) of
                                {atomic, ok} -> ok;
                                {aborted, Reason} -> {error, Reason}
                            end
                    end
            end
    end.

do_remove_member(GroupId, UserId, RemovedBy) ->
    %% Users can remove themselves; admins can remove anyone
    CanRemove = (UserId =:= RemovedBy) orelse is_admin(GroupId, RemovedBy),
    case CanRemove of
        false -> {error, not_authorized};
        true ->
            case is_member(GroupId, UserId) of
                false -> {error, not_member};
                true ->
                    %% Check if this is the last admin
                    case is_admin(GroupId, UserId) of
                        true ->
                            case count_admins(GroupId) of
                                1 -> {error, last_admin};
                                _ -> do_remove_member_impl(GroupId, UserId)
                            end;
                        false ->
                            do_remove_member_impl(GroupId, UserId)
                    end
            end
    end.

do_remove_member_impl(GroupId, UserId) ->
    F = fun() ->
        %% Remove member
        mnesia:delete(group_member, {GroupId, UserId}, write),
        
        %% Remove sender keys
        KeyMatchSpec = [{
            #group_sender_key{key = {GroupId, UserId, '_'}, _ = '_'},
            [],
            ['$_']
        }],
        Keys = mnesia:select(group_sender_key, KeyMatchSpec, write),
        [mnesia:delete_object(group_sender_key, K, write) || K <- Keys],
        
        %% Update member count
        [Group] = mnesia:read(group, GroupId, write),
        UpdatedGroup = Group#group{
            member_count = max(0, Group#group.member_count - 1)
        },
        mnesia:write(group, UpdatedGroup, write),
        ok
    end,
    
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

do_promote_admin(GroupId, UserId, PromotedBy) ->
    case is_admin(GroupId, PromotedBy) of
        false -> {error, not_authorized};
        true ->
            case mnesia:dirty_read(group_member, {GroupId, UserId}) of
                [] -> {error, not_member};
                [#group_member{role = admin}] -> {error, already_admin};
                [Member] ->
                    UpdatedMember = Member#group_member{role = admin},
                    mnesia:dirty_write(group_member, UpdatedMember),
                    ok
            end
    end.

do_demote_admin(GroupId, UserId, DemotedBy) ->
    case is_admin(GroupId, DemotedBy) of
        false -> {error, not_authorized};
        true ->
            case mnesia:dirty_read(group_member, {GroupId, UserId}) of
                [] -> {error, not_member};
                [#group_member{role = member}] -> {error, not_admin};
                [Member] ->
                    %% Check if this is the last admin
                    case count_admins(GroupId) of
                        1 -> {error, last_admin};
                        _ ->
                            UpdatedMember = Member#group_member{role = member},
                            mnesia:dirty_write(group_member, UpdatedMember),
                            ok
                    end
            end
    end.

count_admins(GroupId) ->
    MatchSpec = [{
        #group_member{key = {GroupId, '_'}, role = admin, _ = '_'},
        [],
        [true]
    }],
    length(mnesia:dirty_select(group_member, MatchSpec)).

generate_group_id() ->
    %% Generate a unique group ID (UUID-like)
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, C, D, E])).

generate_key_id() ->
    %% Generate a unique key ID
    <<N:64>> = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("~16.16.0b", [N])).
