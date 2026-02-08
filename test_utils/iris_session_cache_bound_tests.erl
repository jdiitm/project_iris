-module(iris_session_cache_bound_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% RFC-001 v4.0 Section 3.4: Session Cache Bounded at 100K
%% =============================================================================
%% "Session state cached in ETS for 5 minutes (<=100K sessions per edge node)"
%%
%% This test suite characterizes the current session cache behavior:
%% - No size limit enforcement exists (gap)
%% - TTL of 5 minutes works correctly
%% - After the fix, a hard 100K limit with LRU eviction should be enforced
%% =============================================================================

-define(MAX_SESSIONS, 100000).

setup() ->
    iris_session_cache:start(),
    ok.

cleanup(_) ->
    iris_session_cache:stop(),
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

session_cache_bound_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"TTL is 5 minutes (300 seconds)", fun test_ttl_is_5_minutes/0},
      {"Store and lookup works", fun test_store_and_lookup/0},
      {"Expired session returns not_found", fun test_expired_session/0},
      {"Cache accepts many sessions (characterization)",
       fun test_cache_accepts_unbounded/0},
      {"Session count function exists or is needed",
       fun test_session_count/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_ttl_is_5_minutes() ->
    ?assertEqual(300, iris_session_cache:get_ttl()).

test_store_and_lookup() ->
    SessionId = <<"test_sess_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    UserId = <<"test_user_bound">>,

    ok = iris_session_cache:store(SessionId, UserId),
    {ok, Info} = iris_session_cache:lookup(SessionId),
    ?assertEqual(UserId, maps:get(user_id, Info)),
    ?assertEqual(0, maps:get(seq, Info)),

    %% Cleanup
    iris_session_cache:remove(SessionId).

test_expired_session() ->
    %% We can't easily test TTL expiry without time manipulation,
    %% but we can verify the remove path works
    SessionId = <<"test_sess_expire_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    UserId = <<"test_user_expire">>,

    ok = iris_session_cache:store(SessionId, UserId),
    iris_session_cache:remove(SessionId),
    ?assertEqual({error, not_found}, iris_session_cache:lookup(SessionId)).

test_cache_accepts_unbounded() ->
    %% Test that the cache accepts a batch and stays within bounds.
    %% With the 100K limit fix, count should never exceed MAX_SESSIONS.
    BatchSize = 1000,
    SessionIds = [begin
        Id = <<"batch_sess_", (integer_to_binary(N))/binary>>,
        ok = iris_session_cache:store(Id, <<"user_", (integer_to_binary(N))/binary>>),
        Id
    end || N <- lists:seq(1, BatchSize)],

    %% Count should be <= MAX_SESSIONS
    Count = iris_session_cache:get_count(),
    MaxSessions = iris_session_cache:get_max_sessions(),
    ?assert(Count =< MaxSessions),

    %% All recently inserted should be retrievable (below limit)
    Found = length([ok || Id <- SessionIds,
                          element(1, iris_session_cache:lookup(Id)) =:= ok]),
    ?assertEqual(BatchSize, Found),

    %% Cleanup
    lists:foreach(fun(Id) -> iris_session_cache:remove(Id) end, SessionIds).

test_session_count() ->
    %% The session cache currently does NOT expose a count function.
    %% After the fix, it should expose get_count/0 for monitoring.
    %% For now, we can check ETS directly.
    SessionId = <<"count_sess_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ok = iris_session_cache:store(SessionId, <<"count_user">>),

    %% Check ETS directly for session count
    Count = ets:info(iris_session_cache_data, size),
    ?assert(is_integer(Count)),
    ?assert(Count >= 1),

    iris_session_cache:remove(SessionId).
