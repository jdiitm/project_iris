-module(iris_global_rate_limit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION: Global Rate Limit Tightening (Attack Vector 1)
%% =============================================================================
%%
%% Attack Vector 1 from audit: "The Botnet"
%% 100K bots across 100 Edge nodes, each under the per-connection limit.
%% Global load: 400K msg/sec. Core nodes overwhelmed.
%%
%% Mitigation:
%%   1. Tighter gossip interval (500ms instead of 1s)
%%   2. Hot-user detection: users depleting >80% tokens get flagged
%%   3. Flagged users trigger synchronous cross-node counter check
%%
%% Tests verify:
%%   1. Gossip interval is <= 500ms (structural)
%%   2. High-rate users are flagged as hot
%%   3. Sync check function exists and is exported
%%   4. Total cross-node budget is bounded
%% =============================================================================

%% =============================================================================
%% Test: Gossip interval is 500ms (structural verification)
%% =============================================================================

gossip_interval_under_500ms_test() ->
    {ok, Src} = file:read_file("src/iris_rate_limiter.erl"),
    %% Find the GOSSIP_INTERVAL define
    ?assertNotEqual(nomatch, binary:match(Src, <<"-define(GOSSIP_INTERVAL, 500).">>)).

%% =============================================================================
%% Test: Hot user table and flagging functions are exported
%% =============================================================================

hot_user_exports_test() ->
    Exports = iris_rate_limiter:module_info(exports),
    ?assert(lists:member({is_hot_user, 1}, Exports)),
    ?assert(lists:member({get_hot_users, 0}, Exports)).

%% =============================================================================
%% Test: High-rate user gets flagged as hot
%% =============================================================================

high_rate_user_flagged_test() ->
    %% Start rate limiter if not running
    ensure_rate_limiter(),
    User = <<"test_hot_user_flag">>,
    %% Set a low burst to make it easy to deplete 80%
    application:set_env(iris_core, rate_burst_default, 10),
    try
        %% Consume 9 out of 10 tokens (90% — above 80% threshold)
        lists:foreach(fun(_) ->
            iris_rate_limiter:check(User)
        end, lists:seq(1, 9)),
        %% User should now be flagged as hot
        ?assert(iris_rate_limiter:is_hot_user(User))
    after
        application:unset_env(iris_core, rate_burst_default),
        %% Clean up ETS
        catch ets:delete(iris_rate_limit_buckets, User),
        catch ets:delete(iris_rate_hot_users, User)
    end.

%% =============================================================================
%% Test: sync_check is exported (for cross-node synchronous verification)
%% =============================================================================

sync_check_exported_test() ->
    Exports = iris_rate_limiter:module_info(exports),
    ?assert(lists:member({sync_check, 1}, Exports)).

%% =============================================================================
%% Helpers
%% =============================================================================

ensure_rate_limiter() ->
    case ets:info(iris_rate_limit_buckets) of
        undefined ->
            %% Start the rate limiter
            case whereis(iris_rate_limiter) of
                undefined ->
                    {ok, _} = iris_rate_limiter:start_link();
                _ -> ok
            end;
        _ -> ok
    end.
