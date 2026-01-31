%%%-------------------------------------------------------------------
%%% @doc Tests for Operational Hard Limits (iris_limits) module.
%%% 
%%% RFC-001 v3.0 Section 8 Requirements:
%%% - E2EE Group Size: 256 members
%%% - Broadcast Group Size: 10,000 members
%%% - Payload Size: 64 KB
%%% - Rate Limit: 5 msg/sec sustained, 20 msg/sec burst
%%% - Fan-out Rate: 1,000 Inboxes/sec/worker
%%% - Inbox Size: 10,000 messages
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iris_limits_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Limit Value Tests
%% ============================================================================

limit_values_test_() ->
    [
        {"E2EE group limit is 256", fun() ->
            ?assertEqual(256, iris_limits:max_e2ee_group_members())
        end},
        
        {"Broadcast group limit is 10000", fun() ->
            ?assertEqual(10000, iris_limits:max_broadcast_group_members())
        end},
        
        {"Payload size limit is 64KB", fun() ->
            ?assertEqual(65536, iris_limits:max_payload_size())
        end},
        
        {"Sustained rate limit is 5/sec", fun() ->
            ?assertEqual(5, iris_limits:max_rate_sustained())
        end},
        
        {"Burst rate limit is 20/sec", fun() ->
            ?assertEqual(20, iris_limits:max_rate_burst())
        end},
        
        {"Burst window is 10 seconds", fun() ->
            ?assertEqual(10, iris_limits:max_rate_burst_window())
        end},
        
        {"Fan-out rate is 1000/sec", fun() ->
            ?assertEqual(1000, iris_limits:max_fanout_rate())
        end},
        
        {"Inbox size limit is 10000", fun() ->
            ?assertEqual(10000, iris_limits:max_inbox_size())
        end},
        
        {"Username max length is 256", fun() ->
            ?assertEqual(256, iris_limits:max_username_length())
        end},
        
        {"Group name max length is 256", fun() ->
            ?assertEqual(256, iris_limits:max_group_name_length())
        end},
        
        {"Max groups per user is 100", fun() ->
            ?assertEqual(100, iris_limits:max_groups_per_user())
        end},
        
        {"Dedup hot window is 5 minutes", fun() ->
            ?assertEqual(300000, iris_limits:dedup_window_hot_ms())
        end},
        
        {"Dedup warm window is 7 days", fun() ->
            ?assertEqual(7, iris_limits:dedup_window_days())
        end}
    ].

%% ============================================================================
%% Payload Validation Tests
%% ============================================================================

payload_validation_test_() ->
    [
        {"Empty payload is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_payload(<<>>))
        end},
        
        {"1 byte payload is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_payload(<<1>>))
        end},
        
        {"64KB payload is valid", fun() ->
            Payload = binary:copy(<<0>>, 65536),
            ?assertEqual(ok, iris_limits:validate_payload(Payload))
        end},
        
        {"64KB + 1 byte payload is invalid", fun() ->
            Payload = binary:copy(<<0>>, 65537),
            ?assertEqual({error, payload_too_large}, iris_limits:validate_payload(Payload))
        end},
        
        {"Size integer validation - at limit", fun() ->
            ?assertEqual(ok, iris_limits:validate_payload(65536))
        end},
        
        {"Size integer validation - over limit", fun() ->
            ?assertEqual({error, payload_too_large}, iris_limits:validate_payload(65537))
        end},
        
        {"1MB payload is invalid", fun() ->
            ?assertEqual({error, payload_too_large}, iris_limits:validate_payload(1048576))
        end}
    ].

%% ============================================================================
%% Username Validation Tests
%% ============================================================================

username_validation_test_() ->
    [
        {"Empty username is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_username(<<>>))
        end},
        
        {"Short username is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_username(<<"alice">>))
        end},
        
        {"256-char username is valid", fun() ->
            Username = binary:copy(<<"a">>, 256),
            ?assertEqual(ok, iris_limits:validate_username(Username))
        end},
        
        {"257-char username is invalid", fun() ->
            Username = binary:copy(<<"a">>, 257),
            ?assertEqual({error, username_too_long}, iris_limits:validate_username(Username))
        end},
        
        {"List username conversion", fun() ->
            ?assertEqual(ok, iris_limits:validate_username("alice"))
        end},
        
        {"Long list username is invalid", fun() ->
            Username = lists:duplicate(257, $a),
            ?assertEqual({error, username_too_long}, iris_limits:validate_username(Username))
        end}
    ].

%% ============================================================================
%% Group Size Validation Tests
%% ============================================================================

group_size_validation_test_() ->
    [
        {"E2EE group with 0 members is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_group_size(e2ee, 0))
        end},
        
        {"E2EE group with 255 members is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_group_size(e2ee, 255))
        end},
        
        {"E2EE group with 256 members is invalid", fun() ->
            ?assertEqual({error, group_full}, iris_limits:validate_group_size(e2ee, 256))
        end},
        
        {"E2EE group with 1000 members is invalid", fun() ->
            ?assertEqual({error, group_full}, iris_limits:validate_group_size(e2ee, 1000))
        end},
        
        {"Broadcast group with 0 members is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_group_size(broadcast, 0))
        end},
        
        {"Broadcast group with 9999 members is valid", fun() ->
            ?assertEqual(ok, iris_limits:validate_group_size(broadcast, 9999))
        end},
        
        {"Broadcast group with 10000 members is invalid", fun() ->
            ?assertEqual({error, group_full}, iris_limits:validate_group_size(broadcast, 10000))
        end}
    ].

%% ============================================================================
%% Rate Limiting Tests
%% ============================================================================

rate_validation_test_() ->
    [
        {"First request is allowed", fun() ->
            State = #{},
            Now = erlang:system_time(millisecond),
            {ok, _NewState} = iris_limits:validate_rate(State, Now)
        end},
        
        {"5 requests in 1 second are allowed", fun() ->
            Now = erlang:system_time(millisecond),
            State0 = #{},
            {ok, State1} = iris_limits:validate_rate(State0, Now),
            {ok, State2} = iris_limits:validate_rate(State1, Now),
            {ok, State3} = iris_limits:validate_rate(State2, Now),
            {ok, State4} = iris_limits:validate_rate(State3, Now),
            {ok, _State5} = iris_limits:validate_rate(State4, Now)
        end},
        
        {"6th request in same second is rejected", fun() ->
            Now = erlang:system_time(millisecond),
            State0 = #{},
            {ok, State1} = iris_limits:validate_rate(State0, Now),
            {ok, State2} = iris_limits:validate_rate(State1, Now),
            {ok, State3} = iris_limits:validate_rate(State2, Now),
            {ok, State4} = iris_limits:validate_rate(State3, Now),
            {ok, State5} = iris_limits:validate_rate(State4, Now),
            ?assertEqual({error, rate_limited}, iris_limits:validate_rate(State5, Now))
        end},
        
        {"Rate limit resets after 1 second", fun() ->
            Now = erlang:system_time(millisecond),
            State0 = #{},
            {ok, State1} = iris_limits:validate_rate(State0, Now),
            {ok, State2} = iris_limits:validate_rate(State1, Now),
            {ok, State3} = iris_limits:validate_rate(State2, Now),
            {ok, State4} = iris_limits:validate_rate(State3, Now),
            {ok, State5} = iris_limits:validate_rate(State4, Now),
            
            %% 6th in same window should fail
            {error, rate_limited} = iris_limits:validate_rate(State5, Now),
            
            %% After 1 second, should succeed
            Later = Now + 1001,
            {ok, _NewState} = iris_limits:validate_rate(State5, Later)
        end},
        
        {"Burst allows 20 requests in 10 seconds", fun() ->
            Now = erlang:system_time(millisecond),
            %% Simulate 20 requests spread across 10 seconds (all within burst window)
            State = lists:foldl(
                fun(Offset, S) ->
                    Time = Now + (Offset * 500),  %% 500ms apart
                    case iris_limits:validate_rate(S, Time) of
                        {ok, NewS} -> NewS;
                        {error, rate_limited} -> S  %% Shouldn't happen
                    end
                end,
                #{},
                lists:seq(0, 19)
            ),
            
            %% 21st request should be rate limited
            FinalTime = Now + 10000,
            ?assertEqual({error, rate_limited}, iris_limits:validate_rate(State, FinalTime))
        end},
        
        {"Burst window resets after 10 seconds", fun() ->
            Now = erlang:system_time(millisecond),
            
            %% Fill up burst quota
            State = lists:foldl(
                fun(Offset, S) ->
                    Time = Now + (Offset * 100),
                    case iris_limits:validate_rate(S, Time) of
                        {ok, NewS} -> NewS;
                        {error, _} -> S
                    end
                end,
                #{},
                lists:seq(0, 19)
            ),
            
            %% After 10+ seconds, burst should reset
            MuchLater = Now + 11000,
            {ok, _NewState} = iris_limits:validate_rate(State, MuchLater)
        end}
    ].

%% ============================================================================
%% Boundary Tests
%% ============================================================================

boundary_test_() ->
    [
        {"Payload at exact 64KB boundary", fun() ->
            Payload = binary:copy(<<$x>>, 65536),
            ?assertEqual(ok, iris_limits:validate_payload(Payload))
        end},
        
        {"Username at exact 256 byte boundary", fun() ->
            Username = binary:copy(<<$a>>, 256),
            ?assertEqual(ok, iris_limits:validate_username(Username))
        end},
        
        {"E2EE group at 255 members (one below limit)", fun() ->
            ?assertEqual(ok, iris_limits:validate_group_size(e2ee, 255))
        end},
        
        {"Broadcast group at 9999 members (one below limit)", fun() ->
            ?assertEqual(ok, iris_limits:validate_group_size(broadcast, 9999))
        end}
    ].

%% ============================================================================
%% Integration-style Tests
%% ============================================================================

integration_test_() ->
    [
        {"Simulate user sending messages", fun test_user_message_flow/0}
    ].

test_user_message_flow() ->
    %% Simulate a user sending messages with rate limiting
    Now = erlang:system_time(millisecond),
    
    %% User can send 5 messages quickly
    Results1 = simulate_messages(5, #{}, Now),
    ?assertEqual(5, length([ok || ok <- Results1])),
    
    %% 6th message should be rate limited
    {_, State6} = lists:foldl(
        fun(_, {T, S}) ->
            case iris_limits:validate_rate(S, T) of
                {ok, NewS} -> {T, NewS};
                {error, _} -> {T, S}
            end
        end,
        {Now, #{}},
        lists:seq(1, 6)
    ),
    ?assertEqual({error, rate_limited}, iris_limits:validate_rate(State6, Now)).

simulate_messages(Count, InitState, StartTime) ->
    {Results, _} = lists:foldl(
        fun(_, {Acc, State}) ->
            case iris_limits:validate_rate(State, StartTime) of
                {ok, NewState} -> {[ok | Acc], NewState};
                Error -> {[Error | Acc], State}
            end
        end,
        {[], InitState},
        lists:seq(1, Count)
    ),
    lists:reverse(Results).
