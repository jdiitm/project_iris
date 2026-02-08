-module(iris_rfc_v4_constants_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% RFC-001 v4.0 Constants Contract Tests
%% =============================================================================
%% Ensures all operational hard limits, TTLs, and table configurations match
%% the RFC specification. If any constant drifts, this test catches it.
%% =============================================================================

%% =============================================================================
%% Section 8: Operational Hard Limits
%% =============================================================================

section_8_limits_test_() ->
    [
     {"E2EE group size limit is 256 (Section 8)", fun() ->
          ?assertEqual(256, iris_limits:max_e2ee_group_members())
      end},
     {"Broadcast group size limit is 10000 (Section 8)", fun() ->
          ?assertEqual(10000, iris_limits:max_broadcast_group_members())
      end},
     {"Payload size limit is 64KB (Section 8)", fun() ->
          ?assertEqual(65536, iris_limits:max_payload_size())
      end},
     {"Sustained rate limit is 5/sec (Section 8)", fun() ->
          ?assertEqual(5, iris_limits:max_rate_sustained())
      end},
     {"Burst rate limit is 20/sec (Section 8)", fun() ->
          ?assertEqual(20, iris_limits:max_rate_burst())
      end},
     {"Fan-out rate is 1000/sec/worker (Section 8)", fun() ->
          ?assertEqual(1000, iris_limits:max_fanout_rate())
      end},
     {"Inbox size limit is 10000 (Section 8)", fun() ->
          ?assertEqual(10000, iris_limits:max_inbox_size())
      end}
    ].

%% =============================================================================
%% Section 7.2: Outbox Queue Constants
%% =============================================================================

outbox_constants_test_() ->
    [
     {"Outbox max queue size is 10000 (Section 7.2)", fun() ->
          ?assertEqual(10000, iris_region_bridge:get_max_queue_size())
      end}
    ].

%% =============================================================================
%% Section 3.4: Session Cache Constants
%% =============================================================================

session_cache_constants_test_() ->
    {setup,
     fun() ->
         iris_session_cache:start(),
         ok
     end,
     fun(_) ->
         iris_session_cache:stop(),
         ok
     end,
     [
      {"Session cache TTL is 300 seconds (Section 3.4)", fun() ->
           ?assertEqual(300, iris_session_cache:get_ttl())
       end},
      {"Session cache max is 100000 (Section 3.4)", fun() ->
           ?assertEqual(100000, iris_session_cache:get_max_sessions())
       end}
     ]}.
