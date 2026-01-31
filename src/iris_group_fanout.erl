-module(iris_group_fanout).

%% =============================================================================
%% Group Message Fan-out
%% =============================================================================
%% Purpose: Efficiently deliver messages to all group members.
%% RFC Reference: FR-13 (Group Messaging)
%%
%% Design:
%% 1. Sender sends message once to Core layer
%% 2. Core layer fans out to all online members
%% 3. Offline members receive via offline storage
%% 4. Batching and parallel delivery for efficiency
%%
%% Fan-out Strategies:
%% - Small groups (<50 members): Serial delivery
%% - Medium groups (50-200): Parallel batches
%% - Large groups (>200): Worker pool distribution
%% =============================================================================

-export([
    fanout_group_msg/3,       %% (GroupId, SenderId, Message) -> {ok, Stats} | {error, Reason}
    fanout_batch/3,           %% (GroupId, SenderId, Messages) -> {ok, Stats}
    get_delivery_stats/1      %% (GroupId) -> Stats
]).

-define(SMALL_GROUP_LIMIT, 50).
-define(MEDIUM_GROUP_LIMIT, 200).
-define(BATCH_SIZE, 25).
-define(PARALLEL_WORKERS, 4).

%% RFC-001 v3.0 Section 8: Fan-out rate limit (1000 Inboxes/sec/worker)
-define(FANOUT_RATE_LIMIT, 1000).
-define(FANOUT_INTERVAL_MS, 1).  %% Minimum ms between deliveries for rate limiting

-record(fanout_stats, {
    group_id       :: binary(),
    total_members  :: integer(),
    delivered      :: integer(),
    offline_stored :: integer(),
    failed         :: integer(),
    duration_ms    :: integer()
}).

%% =============================================================================
%% API Functions
%% =============================================================================

%% @doc Fan out a message to all group members.
-spec fanout_group_msg(binary(), binary(), binary()) -> 
    {ok, #fanout_stats{}} | {error, term()}.
fanout_group_msg(GroupId, SenderId, Message) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Get group members
    case iris_group:get_members(GroupId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Members} ->
            %% Filter out sender
            Recipients = [M || M <- Members, maps:get(user_id, M) =/= SenderId],
            TotalRecipients = length(Recipients),
            
            %% Choose fan-out strategy based on group size
            {Delivered, Offline, Failed} = 
                case TotalRecipients of
                    N when N =< ?SMALL_GROUP_LIMIT ->
                        fanout_serial(Recipients, SenderId, Message);
                    N when N =< ?MEDIUM_GROUP_LIMIT ->
                        fanout_parallel_batches(Recipients, SenderId, Message);
                    _ ->
                        fanout_worker_pool(Recipients, SenderId, Message)
                end,
            
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            
            Stats = #fanout_stats{
                group_id = GroupId,
                total_members = TotalRecipients + 1,  %% +1 for sender
                delivered = Delivered,
                offline_stored = Offline,
                failed = Failed,
                duration_ms = Duration
            },
            
            {ok, Stats}
    end.

%% @doc Fan out multiple messages to a group (batch delivery).
-spec fanout_batch(binary(), binary(), [binary()]) -> {ok, #fanout_stats{}}.
fanout_batch(GroupId, SenderId, Messages) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    case iris_group:get_members(GroupId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Members} ->
            Recipients = [M || M <- Members, maps:get(user_id, M) =/= SenderId],
            
            %% For batch, use parallel delivery with message batching
            Stats = lists:foldl(
                fun(Msg, {D, O, F}) ->
                    {D1, O1, F1} = fanout_serial(Recipients, SenderId, Msg),
                    {D + D1, O + O1, F + F1}
                end,
                {0, 0, 0},
                Messages
            ),
            
            {Delivered, Offline, Failed} = Stats,
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            
            {ok, #fanout_stats{
                group_id = GroupId,
                total_members = length(Recipients) + 1,
                delivered = Delivered,
                offline_stored = Offline,
                failed = Failed,
                duration_ms = Duration
            }}
    end.

%% @doc Get delivery statistics for a group.
-spec get_delivery_stats(binary()) -> map().
get_delivery_stats(GroupId) ->
    case iris_group:get_group(GroupId) of
        {ok, GroupInfo} ->
            #{
                group_id => GroupId,
                member_count => maps:get(member_count, GroupInfo),
                strategy => choose_strategy(maps:get(member_count, GroupInfo))
            };
        {error, _} ->
            #{group_id => GroupId, error => not_found}
    end.

%% =============================================================================
%% Fan-out Strategies
%% =============================================================================

%% @doc Serial delivery for small groups with rate limiting.
%% RFC-001 v3.0 Section 8: Max 1000 Inboxes/sec/worker
fanout_serial(Recipients, SenderId, Message) ->
    fanout_serial(Recipients, SenderId, Message, {0, 0, 0}, erlang:monotonic_time(millisecond), 0).

fanout_serial([], _SenderId, _Message, Acc, _LastTime, _Count) ->
    Acc;
fanout_serial([Member | Rest], SenderId, Message, {D, O, F}, LastTime, Count) ->
    %% Rate limiting: after FANOUT_RATE_LIMIT deliveries per second, throttle
    {NewLastTime, NewCount} = case Count >= ?FANOUT_RATE_LIMIT of
        true ->
            %% Check if a second has passed
            Now = erlang:monotonic_time(millisecond),
            Elapsed = Now - LastTime,
            case Elapsed < 1000 of
                true ->
                    %% Throttle - sleep for remaining time
                    timer:sleep(1000 - Elapsed),
                    {erlang:monotonic_time(millisecond), 0};
                false ->
                    %% Second passed, reset counter
                    {Now, 0}
            end;
        false ->
            {LastTime, Count}
    end,
    
    UserId = maps:get(user_id, Member),
    case deliver_to_user(UserId, SenderId, Message) of
        {delivered, _} -> 
            fanout_serial(Rest, SenderId, Message, {D + 1, O, F}, NewLastTime, NewCount + 1);
        {offline_stored, _} -> 
            fanout_serial(Rest, SenderId, Message, {D, O + 1, F}, NewLastTime, NewCount + 1);
        {error, _} -> 
            fanout_serial(Rest, SenderId, Message, {D, O, F + 1}, NewLastTime, NewCount + 1)
    end.

%% @doc Parallel batch delivery for medium groups.
fanout_parallel_batches(Recipients, SenderId, Message) ->
    %% Split into batches
    Batches = split_into_batches(Recipients, ?BATCH_SIZE),
    
    %% Process batches in parallel
    Parent = self(),
    Pids = [spawn_link(fun() ->
        Result = fanout_serial(Batch, SenderId, Message),
        Parent ! {batch_result, self(), Result}
    end) || Batch <- Batches],
    
    %% Collect results
    collect_batch_results(Pids, {0, 0, 0}).

%% @doc Worker pool delivery for large groups.
fanout_worker_pool(Recipients, SenderId, Message) ->
    %% Distribute work across multiple workers
    Batches = distribute_work(Recipients, ?PARALLEL_WORKERS),
    
    Parent = self(),
    Pids = [spawn_link(fun() ->
        Result = fanout_serial(Batch, SenderId, Message),
        Parent ! {worker_result, self(), Result}
    end) || Batch <- Batches],
    
    collect_batch_results(Pids, {0, 0, 0}).

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% @doc Deliver message to a single user.
deliver_to_user(UserId, SenderId, Message) ->
    %% Check if user is online
    case is_user_online(UserId) of
        true ->
            %% Send directly
            case send_to_user(UserId, SenderId, Message) of
                ok -> {delivered, UserId};
                {error, R} -> {error, R}
            end;
        false ->
            %% Store for offline delivery
            case store_offline_msg(UserId, SenderId, Message) of
                ok -> {offline_stored, UserId};
                {error, R} -> {error, R}
            end
    end.

%% @doc Check if user is online (has active session).
is_user_online(UserId) ->
    %% Try to find user in presence registry
    case catch iris_core:lookup_user(UserId) of
        {ok, _Pid} -> true;
        _ -> false
    end.

%% @doc Send message to online user.
send_to_user(UserId, SenderId, Message) ->
    case catch iris_core:lookup_user(UserId) of
        {ok, Pid} ->
            %% Send via session process
            try
                Pid ! {group_msg, SenderId, Message},
                ok
            catch
                _:_ -> {error, send_failed}
            end;
        _ ->
            {error, user_not_found}
    end.

%% @doc Store message for offline user.
store_offline_msg(UserId, SenderId, Message) ->
    %% Create offline message record
    OfflineMsg = #{
        from => SenderId,
        type => group_msg,
        content => Message,
        timestamp => erlang:system_time(second)
    },
    
    %% Store using offline storage module
    case catch iris_offline_storage:store_durable(UserId, OfflineMsg, 1) of
        ok -> ok;
        {error, R} -> {error, R};
        _ -> {error, storage_failed}
    end.

%% @doc Split list into batches of given size.
split_into_batches(List, BatchSize) ->
    split_into_batches(List, BatchSize, []).

split_into_batches([], _, Acc) ->
    lists:reverse(Acc);
split_into_batches(List, BatchSize, Acc) ->
    {Batch, Rest} = case length(List) > BatchSize of
        true -> lists:split(BatchSize, List);
        false -> {List, []}
    end,
    split_into_batches(Rest, BatchSize, [Batch | Acc]).

%% @doc Distribute work evenly across N workers.
distribute_work(Items, NumWorkers) ->
    ItemsPerWorker = (length(Items) + NumWorkers - 1) div NumWorkers,
    split_into_batches(Items, ItemsPerWorker).

%% @doc Collect results from parallel workers.
collect_batch_results([], Acc) ->
    Acc;
collect_batch_results(Pids, {D, O, F}) ->
    receive
        {batch_result, Pid, {D1, O1, F1}} ->
            collect_batch_results(lists:delete(Pid, Pids), {D + D1, O + O1, F + F1});
        {worker_result, Pid, {D1, O1, F1}} ->
            collect_batch_results(lists:delete(Pid, Pids), {D + D1, O + O1, F + F1})
    after 30000 ->
        %% Timeout - count remaining as failed
        Remaining = length(Pids),
        {D, O, F + Remaining}
    end.

%% @doc Choose fan-out strategy based on group size.
choose_strategy(MemberCount) when MemberCount =< ?SMALL_GROUP_LIMIT ->
    serial;
choose_strategy(MemberCount) when MemberCount =< ?MEDIUM_GROUP_LIMIT ->
    parallel_batch;
choose_strategy(_) ->
    worker_pool.
