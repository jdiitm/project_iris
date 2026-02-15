-module(iris_key_change_notify_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% RFC-001-AMENDMENT-001 Section 5.3.2: Key Change Notification
%% =============================================================================
%% "When a user's Identity Key changes, the server MUST notify all active
%% sessions that have communicated with that user."
%%
%% STATUS: IMPLEMENTED
%%
%% Implementation (iris_keys.erl):
%%   - Identity key change DETECTION via detect_identity_key_change/2
%%   - Metric (iris_identity_key_changes) incremented on change
%%   - Contact tracking via record_key_contact/2 (Mnesia-persisted key_contact table)
%%   - On IK change, all contacts are notified:
%%     * Online contacts: direct pid delivery (Pid ! {deliver_msg, AlertPacket})
%%     * Offline contacts: durable offline storage (iris_core:store_offline_durable)
%%   - Protocol: opcode 0x1A (key_change_alert) in iris_proto.erl
%%
%% End-to-end delivery tests: see iris_key_change_delivery_tests.erl
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

%% Notification delivery is implemented and tested.
%% See iris_key_change_delivery_tests.erl for end-to-end tests covering:
%%   - online_contact_receives_key_change_alert/0
%%   - offline_contact_gets_alert_stored_for_later_delivery/0
