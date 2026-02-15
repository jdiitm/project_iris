-module(iris_rfc_keybundle_durability_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F3: Key Bundle Quorum Write Durability Tests
%%
%% store_key_bundle_durable/2 silently falls back to
%% mnesia:sync_transaction when quorum write returns {error, quorum_not_reached}.
%% This degrades from cluster-level to node-level durability for security-
%% critical key bundles. CP > AP: failure is preferred over weak durability.
%%
%% Tests verify:
%% 1. When quorum write fails, the error propagates -- no silent fallback.
%% =============================================================================

-record(key_bundle, {
    user_id,
    identity_key,
    signed_prekey,
    signed_prekey_signature,
    signed_prekey_timestamp,
    one_time_prekeys,
    created_at,
    updated_at
}).

setup() ->
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    case mnesia:create_table(e2ee_key_bundle, [
        {attributes, record_info(fields, key_bundle)},
        {record_name, key_bundle},
        {ram_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, e2ee_key_bundle}} -> ok
    end,
    mnesia:wait_for_tables([e2ee_key_bundle], 5000),

    %% Save original module beam for restoration
    OrigBeam = code:which(iris_quorum_write),

    %% Load a mock iris_quorum_write that always returns {error, quorum_not_reached}
    load_mock_quorum_write(),

    %% Register a dummy process so whereis(iris_quorum_write) returns non-undefined
    DummyPid = spawn(fun() -> receive stop -> ok end end),
    register(iris_quorum_write, DummyPid),

    {DummyPid, OrigBeam}.

cleanup({DummyPid, OrigBeam}) ->
    %% Unregister and stop dummy process
    catch unregister(iris_quorum_write),
    DummyPid ! stop,

    %% Restore original module
    restore_quorum_write(OrigBeam),

    catch mnesia:delete_table(e2ee_key_bundle),
    application:stop(mnesia).

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_rfc_keybundle_durability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Quorum failure propagates, no silent fallback",
       fun test_quorum_failure_rejects_not_fallback/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_quorum_failure_rejects_not_fallback() ->
    %% Arrange: create a valid key bundle record
    Now = os:system_time(second),
    Record = #key_bundle{
        user_id = <<"test_user_quorum">>,
        identity_key = crypto:strong_rand_bytes(32),
        signed_prekey = crypto:strong_rand_bytes(32),
        signed_prekey_signature = crypto:strong_rand_bytes(64),
        signed_prekey_timestamp = Now,
        one_time_prekeys = [],
        created_at = Now,
        updated_at = Now
    },

    %% Act: call store_key_bundle_durable when quorum write will fail
    Result = iris_keys:store_key_bundle_durable(<<"test_user_quorum">>, Record),

    %% Assert: error must propagate, NOT silently succeed via local fallback
    ?assertMatch({error, quorum_not_reached}, Result).

%% =============================================================================
%% Mock Helpers
%% =============================================================================

load_mock_quorum_write() ->
    %% Dynamically compile a mock iris_quorum_write module
    %% that returns {error, quorum_not_reached} for write_durable/3 and /4
    Forms = [
        {attribute, 1, module, iris_quorum_write},
        {attribute, 2, export, [{write_durable, 3}, {write_durable, 4}]},
        {function, 3, write_durable, 3,
            [{clause, 3,
                [{var, 3, '_'}, {var, 3, '_'}, {var, 3, '_'}],
                [],
                [{tuple, 4, [{atom, 4, error}, {atom, 4, quorum_not_reached}]}]
            }]},
        {function, 5, write_durable, 4,
            [{clause, 5,
                [{var, 5, '_'}, {var, 5, '_'}, {var, 5, '_'}, {var, 5, '_'}],
                [],
                [{tuple, 6, [{atom, 6, error}, {atom, 6, quorum_not_reached}]}]
            }]}
    ],
    {ok, iris_quorum_write, Bin} = compile:forms(Forms),
    code:purge(iris_quorum_write),
    {module, iris_quorum_write} = code:load_binary(iris_quorum_write, "mock", Bin),
    ok.

restore_quorum_write(OrigBeam) when is_list(OrigBeam) ->
    code:purge(iris_quorum_write),
    code:load_abs(filename:rootname(OrigBeam));
restore_quorum_write(_) ->
    %% Module was not loaded before, just purge mock
    code:purge(iris_quorum_write),
    ok.
