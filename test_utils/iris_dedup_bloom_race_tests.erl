-module(iris_dedup_bloom_race_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F2: Bloom Filter Race Condition Test (NFR-11 Audit Finding)
%%
%% The bloom filter's add_to_bloom/1 uses a non-atomic Read-Modify-Write
%% pattern: ets:lookup -> modify binary -> ets:insert. Two concurrent
%% callers can read the same bloom binary, each set their own bits, and
%% the second ets:insert overwrites the first's bit changes (TOCTOU race).
%%
%% This test spawns N concurrent writers to demonstrate bit loss.
%% =============================================================================

-define(BLOOM_TABLE, iris_dedup_bloom).
-define(BLOOM_SIZE, 10000000).

setup() ->
    %% Start the iris_dedup gen_server. This creates the bloom ETS table
    %% and provides the serialization point for bloom writes.
    %% We need mnesia schema for dedup_log (even if unused in this test).
    catch gen_server:stop(iris_dedup),
    catch ets:delete(?BLOOM_TABLE),
    catch ets:delete(iris_dedup_seen),
    timer:sleep(50),
    {ok, Pid} = iris_dedup:start_link(),
    {started, Pid}.

cleanup({started, Pid}) ->
    catch gen_server:stop(Pid),
    catch ets:delete(?BLOOM_TABLE),
    catch ets:delete(iris_dedup_seen),
    ok.

iris_dedup_bloom_race_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Concurrent bloom writes must not lose bits",
       {timeout, 30, fun test_concurrent_bloom_writes_no_bit_loss/0}}
     ]}.

test_concurrent_bloom_writes_no_bit_loss() ->
    %% Generate N distinct message IDs
    N = 200,
    MsgIds = [<<"bloom_race_test_msg_", (integer_to_binary(I))/binary>> 
              || I <- lists:seq(1, N)],
    
    %% Spawn N concurrent processes all calling add_to_bloom at once.
    %% With the gen_server fix, these casts serialize through the mailbox.
    %% Without the fix, the direct ets:lookup/insert race causes bit loss.
    Parent = self(),
    Pids = lists:map(fun(MsgId) ->
        spawn(fun() ->
            iris_dedup:add_to_bloom(MsgId),
            Parent ! {done, self()}
        end)
    end, MsgIds),
    
    %% Wait for all spawned processes to complete their cast
    lists:foreach(fun(Pid) ->
        receive {done, Pid} -> ok
        after 5000 -> error({timeout_waiting_for, Pid})
        end
    end, Pids),
    
    %% Wait for gen_server to process all casts
    %% (get_stats is a sync call that flushes the mailbox)
    _ = iris_dedup:get_stats(),
    
    %% Verify EVERY message ID is found in the bloom filter.
    %% With the TOCTOU race, some bits will be lost due to concurrent
    %% overwrites, causing false negatives.
    Results = lists:map(fun(MsgId) ->
        {MsgId, iris_dedup:check_bloom(MsgId)}
    end, MsgIds),
    
    Missing = [MsgId || {MsgId, false} <- Results],
    
    %% Assert zero bit loss
    ?assertEqual([], Missing).
