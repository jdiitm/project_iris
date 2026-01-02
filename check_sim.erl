-module(check_sim).
-export([run/0]).

run() ->
    io:format("Checking Simulation State...~n"),
    %% Node started as distributed via -sname
    % erlang:set_cookie(node(), iris_secret), %% Already set by flag
    
    Target = 'iris_core@j',
    io:format("Pinging ~p: ~p~n", [Target, net_adm:ping(Target)]),
    
    User = <<"vip_global">>,
    Meta = rpc:call(Target, mnesia, dirty_read, [user_meta, User]),
    io:format("Meta: ~p~n", [Meta]),
    
    %% Count offline messages
    %% Scan bucket 0 to 50
    Count = 50,
    Total = lists:foldl(fun(B, Acc) ->
        Key = {User, B},
        Recs = rpc:call(Target, mnesia, dirty_read, [offline_msg, Key]),
        case Recs of
            L when is_list(L) -> 
                %% Each record has a payload. Payload might be Batch (List) or Binary.
                SubTotal = lists:foldl(fun({offline_msg, _, _, Payload}, A) ->
                    if is_list(Payload) -> A + length(Payload);
                       is_binary(Payload) -> A + 1;
                       true -> A
                    end
                end, 0, L),
                Acc + SubTotal;
            _ -> Acc
        end
    end, 0, lists:seq(0, Count)),
    
    io:format("Total Offline Msgs Found: ~p~n", [Total]),
    init:stop().
