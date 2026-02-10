-module(iris_codel_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% CoDel (Controlled Delay) Active Queue Management Tests
%% =============================================================================
%% Tests the pure CoDel algorithm in iris_mailbox_guard:codel_check/3.
%%
%% CoDel tracks minimum sojourn time over a sliding interval. If the minimum
%% stays above a target for an entire interval, it begins dropping. Drops
%% stop immediately when the queue drains below target.
%%
%% RED:  codel_check/3 does not exist yet. All tests fail with undef.
%% GREEN: Implement codel_check/3 in iris_mailbox_guard.erl.
%% =============================================================================

codel_test_() ->
    [
     {"new codel state has correct defaults",
      fun test_codel_new_defaults/0},
     {"burst below interval does not trigger drops",
      fun test_codel_allows_burst/0},
     {"sustained delay above target triggers dropping",
      fun test_codel_drops_on_sustained_delay/0},
     {"dropping stops when queue drains below target",
      fun test_codel_stops_dropping_when_queue_drains/0},
     {"drop spacing decreases with 1/sqrt(count)",
      fun test_codel_drop_spacing/0}
    ].

test_codel_new_defaults() ->
    State = iris_mailbox_guard:codel_new(),
    ?assertMatch(#{target_ms := 5, interval_ms := 100, dropping := false}, State).

test_codel_allows_burst() ->
    S0 = iris_mailbox_guard:codel_new(),
    Now0 = 1000,
    %% Sojourn time above target (10ms > 5ms) but for less than one interval
    %% Check at T=1000: first time above, starts tracking
    {R1, S1} = iris_mailbox_guard:codel_check(10, Now0, S0),
    ?assertEqual(ok, R1),
    %% Check at T=1050 (50ms later, still within 100ms interval)
    {R2, _S2} = iris_mailbox_guard:codel_check(10, Now0 + 50, S1),
    ?assertEqual(ok, R2).

test_codel_drops_on_sustained_delay() ->
    S0 = iris_mailbox_guard:codel_new(),
    Now0 = 1000,
    %% First check: sojourn above target, starts tracking
    {ok, S1} = iris_mailbox_guard:codel_check(10, Now0, S0),
    %% Second check at T+110ms (past interval): should trigger drop
    {R2, _S2} = iris_mailbox_guard:codel_check(10, Now0 + 110, S1),
    ?assertEqual(drop, R2).

test_codel_stops_dropping_when_queue_drains() ->
    S0 = iris_mailbox_guard:codel_new(),
    Now0 = 1000,
    %% Enter dropping state
    {ok, S1} = iris_mailbox_guard:codel_check(10, Now0, S0),
    {drop, S2} = iris_mailbox_guard:codel_check(10, Now0 + 110, S1),
    %% Queue drains: sojourn drops below target
    {R3, S3} = iris_mailbox_guard:codel_check(2, Now0 + 120, S2),
    ?assertEqual(ok, R3),
    %% Verify dropping state is reset
    ?assertMatch(#{dropping := false}, S3).

test_codel_drop_spacing() ->
    S0 = iris_mailbox_guard:codel_new(),
    Now0 = 1000,
    %% Enter dropping state
    {ok, S1} = iris_mailbox_guard:codel_check(10, Now0, S0),
    {drop, S2} = iris_mailbox_guard:codel_check(10, Now0 + 110, S1),
    %% drop_count should be 1 after first drop
    ?assertMatch(#{drop_count := 1}, S2),
    %% Next check: still above target, still in dropping mode
    %% The drop_next time should be set for 1/sqrt(2) interval from now
    %% At T+120, we may or may not drop depending on drop_next timing
    %% But after enough time passes, we should get another drop with count=2
    {drop, S3} = iris_mailbox_guard:codel_check(10, Now0 + 250, S2),
    ?assertMatch(#{drop_count := 2}, S3).
