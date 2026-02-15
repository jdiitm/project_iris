-module(iris_revocation_error_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% H-6 Mitigation: revoke_refresh_family must not silently swallow errors.
%% =============================================================================
%% Currently, revoke_refresh_family catches all errors with `catch _:_ -> ok`.
%% This means a failed revocation (e.g., Mnesia down, table missing) returns
%% ok, giving the caller false confidence that the family was revoked.
%%
%% After the fix: on failure, it should return {error, Reason} and log.
%% =============================================================================

revoke_nonexistent_family_returns_ok_test() ->
    %% Revoking a family that doesn't exist should succeed (no-op)
    %% This verifies the happy path still works
    ensure_mnesia_ready(),
    Result = iris_auth:revoke_refresh_family(<<"nonexistent_family_id">>),
    %% After the fix: returns ok (unwrapped from {atomic, ok})
    ?assertEqual(ok, Result).

revoke_with_missing_table_returns_error_test() ->
    %% When the refresh_tokens table doesn't exist (e.g., Mnesia not started
    %% properly), the revocation should return an error, NOT silently succeed.
    %%
    %% We can simulate this by deleting the table temporarily
    %% or by calling with Mnesia stopped.
    
    %% Stop Mnesia to simulate an infrastructure failure
    mnesia:stop(),
    try
        Result = iris_auth:revoke_refresh_family(<<"test_family">>),
        %% After the fix: should return {error, _} instead of ok
        ?assertMatch({error, _}, Result)
    after
        %% Restart Mnesia so other tests aren't affected
        mnesia:start(),
        timer:sleep(100)
    end.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------
ensure_mnesia_ready() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        _ ->
            mnesia:start(),
            timer:sleep(100)
    end,
    %% Ensure refresh_tokens table exists
    case catch mnesia:table_info(refresh_tokens, type) of
        {'EXIT', _} ->
            mnesia:create_table(refresh_tokens, [
                {attributes, [id, user_id, family_id, used, created_at, expires_at]},
                {type, set}
            ]),
            mnesia:wait_for_tables([refresh_tokens], 5000);
        _ ->
            ok
    end.
