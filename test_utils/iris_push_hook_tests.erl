-module(iris_push_hook_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: Push Notification Hook Interface Tests
%% =============================================================================
%%
%% Tests verify that iris_offline_storage has a configurable push notification
%% hook point so that future APNS/FCM integrations can be plugged in.
%% =============================================================================

iris_push_hook_test_() ->
    [
     {"AUDIT M12: push_notify hook exists in source",
      fun test_push_hook_in_source/0},
     {"AUDIT M12: default push hook is no-op",
      fun test_push_hook_default_is_noop/0}
    ].

test_push_hook_in_source() ->
    {ok, Src} = file:read_file("src/iris_offline_storage.erl"),
    ?assert(binary:match(Src, <<"push_notify">>) =/= nomatch).

test_push_hook_default_is_noop() ->
    %% With no push_hook configured, notify_push should be a no-op
    application:unset_env(iris_core, push_hook),
    %% Call the exported function -- should return ok without error
    Result = iris_offline_storage:notify_push(<<"user1">>, <<"test msg">>),
    ?assertEqual(ok, Result).
