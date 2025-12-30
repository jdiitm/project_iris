-module(iris_offline_storage).
-export([store/2, retrieve/1]).

%% Mnesia table definition (created in iris_core:init_db/0):
%% {offline_msg, User, Timestamp, Msg}

store(User, Msg) ->
    Timestamp = os:system_time(millisecond),
    F = fun() ->
        mnesia:write({offline_msg, User, Timestamp, Msg})
    end,
    mnesia:activity(transaction, F).

retrieve(User) ->
    %% Read all messages for user
    F = fun() ->
        %% Use match_object to get all records for this user based on primary key part?
        %% Mnesia bag tables key is the first element, but here we scan by User (field 1).
        %% Wait, {offline_msg, User, ...}. Yes, User is key.
        Msgs = mnesia:read(offline_msg, User),
        %% Delete them after reading
        lists:foreach(fun(Rec) -> mnesia:delete_object(Rec) end, Msgs),
        Msgs
    end,
    case mnesia:activity(transaction, F) of
        {atomic, Records} ->
            %% Records is a list of {offline_msg, User, Ts, Msg}
            %% Sort by timestamp
            Sorted = lists:sort(fun({_, _, Ts1, _}, {_, _, Ts2, _}) -> Ts1 =< Ts2 end, Records),
            %% Extract Msgs
            [Msg || {_, _, _, Msg} <- Sorted];
        Records when is_list(Records) ->
             %% Sometimes activity returns result directly if not explicitly {atomic, ...} depending on type?
             %% Transaction returns {atomic, Val} or {aborted, Reason}.
             %% Let's stick to matching result of activity/2.
             Sorted = lists:sort(fun({_, _, Ts1, _}, {_, _, Ts2, _}) -> Ts1 =< Ts2 end, Records),
             [Msg || {_, _, _, Msg} <- Sorted];
         Error ->
             io:format("Error retrieving offline msgs: ~p~n", [Error]),
             []
    end.
