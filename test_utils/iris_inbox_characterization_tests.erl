-module(iris_inbox_characterization_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% CHARACTERIZATION TEST: Inbox Size Limit (RFC-001 v4.0 Section 8)
%% =============================================================================
%% RFC says: "Inbox Size: 10,000 messages. Oldest messages archived to cold storage."
%%
%% This test documents the CURRENT (unpatched) behavior:
%%   iris_limits:max_inbox_size() returns 10000 but the value is never checked
%%   in iris_core:store_offline_durable/2.
%%
%% After the fix (GAP-6), this characterization test should be replaced by
%% iris_inbox_limit_tests.erl which asserts rejection at 10001.
%% =============================================================================

%% The inbox limit constant exists and returns the RFC value
inbox_limit_constant_test() ->
    ?assertEqual(10000, iris_limits:max_inbox_size()).

%% The inbox limit constant matches the RFC Section 8 hard constraint
inbox_limit_matches_rfc_section_8_test() ->
    Limit = iris_limits:max_inbox_size(),
    ?assert(Limit =:= 10000),
    %% Also verify the other Section 8 constants are consistent
    ?assertEqual(256, iris_limits:max_e2ee_group_members()),
    ?assertEqual(65536, iris_limits:max_payload_size()).
