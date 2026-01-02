-module(iris_offline_storage).
-export([store/3, retrieve/2]).

%% Mnesia table definition (created in iris_core:init_db/0):
%% {offline_msg, User, Timestamp, Msg}

store(User, Msg, Count) ->
    Timestamp = os:system_time(millisecond),
    %% Determine Partition
    BucketID = erlang:phash2(Msg, Count),
    Key = {User, BucketID},
    
    F = fun() ->
        mnesia:write({offline_msg, Key, Timestamp, Msg})
    end,
    mnesia:activity(transaction, F).

retrieve(User, Count) ->
    %% Read messages from all buckets
    F = fun() ->
        %% Iterate all buckets 0..Count-1
        Lists = lists:map(fun(ID) ->
            Key = {User, ID},
            Msgs = mnesia:read(offline_msg, Key, write),
            mnesia:delete({offline_msg, Key}),
            Msgs
        end, lists:seq(0, Count - 1)),
        lists:append(Lists)
    end,
    
    case mnesia:activity(transaction, F) of
        {atomic, Records} ->
            sort_and_extract(Records);
        Records when is_list(Records) ->
            sort_and_extract(Records);
        Error ->
            io:format("Error retrieving offline msgs: ~p~n", [Error]),
            []
    end.

sort_and_extract(Records) ->
    Sorted = lists:sort(fun({_, _, Ts1, _}, {_, _, Ts2, _}) -> Ts1 =< Ts2 end, Records),
    [Msg || {_, _, _, Msg} <- Sorted].
