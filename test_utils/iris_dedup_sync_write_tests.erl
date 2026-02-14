-module(iris_dedup_sync_write_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% GAP-3: Deduplication Log Synchronous Write Tests (RFC Section 6.2)
%%
%% The current write_dedup_log/2 uses spawn(fun() -> dirty_write end) which
%% creates a crash window: check_and_mark returns "new" but the dedup_log
%% entry hasn't landed yet. If the node crashes in that window, the message
%% can be re-processed on restart.
%%
%% RED: After check_and_mark returns "new", dedup_log entry must exist
%%      IMMEDIATELY (no timer:sleep needed). Current async write fails this.
%% GREEN: Make write_dedup_log synchronous.
%% =============================================================================

-define(TABLE, iris_dedup_seen).
-define(BLOOM_TABLE, iris_dedup_bloom).

setup() ->
    %% Stop any existing iris_dedup and clean ETS
    catch gen_server:stop(iris_dedup),
    catch ets:delete(?TABLE),
    catch ets:delete(?BLOOM_TABLE),

    %% Start Mnesia with dedup_log table
    catch mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    case mnesia:create_table(dedup_log, [
        {attributes, [msg_id, timestamp]},
        {disc_only_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, dedup_log}} -> ok
    end,
    mnesia:wait_for_tables([dedup_log], 5000),

    %% Let iris_dedup:start_link create its own ETS tables
    {ok, Pid} = iris_dedup:start_link([]),
    {started, Pid}.

cleanup({started, _Pid}) ->
    catch gen_server:stop(iris_dedup),
    catch ets:delete(?TABLE),
    catch ets:delete(?BLOOM_TABLE),
    catch mnesia:delete_table(dedup_log),
    catch mnesia:stop();
cleanup({existing, _}) ->
    ok.

iris_dedup_sync_write_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"dedup_log entry exists immediately after check_and_mark returns new",
       fun test_dedup_log_written_synchronously/0},
      {"dedup_log survives full Mnesia stop/restart cycle",
       fun test_dedup_log_survives_full_mnesia_restart/0}
     ]}.

test_dedup_log_written_synchronously() ->
    MsgId = <<"sync_test_msg_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% Call check_and_mark -- should return "new"
    Result = iris_dedup:check_and_mark(MsgId),
    ?assertEqual(new, Result),

    %% IMMEDIATELY after check_and_mark returns, the dedup_log entry
    %% MUST exist in Mnesia. No timer:sleep, no yielding.
    %% If write_dedup_log is async (spawn), this will fail because the
    %% spawned process hasn't run yet.
    DedupEntry = mnesia:dirty_read(dedup_log, MsgId),
    ?assertMatch([{dedup_log, MsgId, _Timestamp}], DedupEntry).

test_dedup_log_survives_full_mnesia_restart() ->
    MsgId = <<"mnesia_restart_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% Write via check_and_mark -- must return "new"
    ?assertEqual(new, iris_dedup:check_and_mark(MsgId)),

    %% Verify the entry exists BEFORE restart
    ?assertMatch([{dedup_log, MsgId, _}], mnesia:dirty_read(dedup_log, MsgId)),

    %% Stop iris_dedup gen_server, then stop Mnesia entirely
    catch gen_server:stop(iris_dedup),
    catch ets:delete(?TABLE),
    catch ets:delete(?BLOOM_TABLE),
    mnesia:stop(),

    %% Restart Mnesia from cold state (simulates node restart)
    mnesia:start(),
    mnesia:wait_for_tables([dedup_log], 5000),

    %% Persistence check: the raw Mnesia record must survive
    DedupEntry = mnesia:dirty_read(dedup_log, MsgId),
    ?assertMatch([{dedup_log, MsgId, _}], DedupEntry),

    %% Functional check: restart iris_dedup and verify dedup works
    {ok, _Pid} = iris_dedup:start_link([]),
    ?assertEqual(duplicate, iris_dedup:check_and_mark(MsgId)).
