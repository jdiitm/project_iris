-module(iris_key_change_notify_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% RFC-001-AMENDMENT-001 Section 5.3.2: Key Change Notification
%% =============================================================================
%% "When a user's Identity Key changes, the server MUST notify all active
%% sessions that have communicated with that user."
%%
%% STATUS: PENDING_DESIGN
%%
%% Current implementation (GAP-13):
%%   - Identity key change DETECTION is implemented in iris_keys.erl
%%   - A metric (iris_identity_key_changes) is incremented on change
%%   - A warning log is emitted
%%   - Notification to peers is NOT yet implemented (requires mini-RFC)
%%
%% Required for full compliance:
%%   1. Track which users have fetched each other's key bundles (session pairs)
%%   2. On IK change, look up all users who fetched the changed user's bundle
%%   3. Send a server->client notification event to each affected user
%%   4. Define a new protocol message type for key change events
%% =============================================================================

%% The detection API exists
key_change_detection_api_exists_test() ->
    Exports = iris_keys:module_info(exports),
    ?assert(lists:member({upload_bundle, 2}, Exports)).

%% The identity key change metric is initialized
key_change_metric_initialized_test() ->
    case whereis(iris_metrics) of
        undefined ->
            {ok, _} = iris_metrics:start_link();
        _ -> ok
    end,
    Metrics = iris_metrics:get_metrics(),
    ?assert(maps:is_key(iris_identity_key_changes, Metrics)).

%% PENDING_DESIGN: This test should verify notification delivery
%% Uncomment when notification logic is implemented:
%%
%% key_change_notifies_active_sessions_test() ->
%%     %% Setup: Alice uploads initial bundle
%%     iris_keys:upload_bundle(<<"alice">>, make_bundle(<<"ik1">>)),
%%     %% Bob fetches Alice's bundle (creates a "session pair")
%%     iris_keys:fetch_bundle(<<"alice">>),
%%     %% Alice uploads new bundle with different IK
%%     iris_keys:upload_bundle(<<"alice">>, make_bundle(<<"ik2">>)),
%%     %% Bob should receive a key change notification
%%     %% ?assert(received_key_change_notification(<<"bob">>, <<"alice">>)).
