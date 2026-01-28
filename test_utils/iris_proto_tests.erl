-module(iris_proto_tests).
-include_lib("eunit/include/eunit.hrl").

unpack_batch_limit_test() ->
    %% Create a batch with 5000 messages (limit is 1000)
    Msgs = [<<"msg">> || _ <- lists:seq(1, 5000)],
    BatchBlob = list_to_binary([<<3:16, M/binary>> || M <- Msgs]),
    
    %% Should fail
    ?assertEqual({error, batch_too_large}, iris_proto:unpack_batch(BatchBlob)),
    
    %% Small batch should pass
    SmallMsgs = [<<"msg">> || _ <- lists:seq(1, 10)],
    SmallBlob = list_to_binary([<<3:16, M/binary>> || M <- SmallMsgs]),
    ?assertEqual(10, length(iris_proto:unpack_batch(SmallBlob))).
