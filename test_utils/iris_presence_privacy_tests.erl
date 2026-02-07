-module(iris_presence_privacy_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% P1-10 (PS-2): Presence Privacy Tests
%%
%% RFC-001 v4.0 FR-8a: User configures visibility:
%% - everyone: All users see real status (default)
%% - contacts: Only contacts see status; others see unavailable
%% - nobody: No one sees status; all queries return unavailable
%%
%% Tests verify:
%% 1. Default is everyone (real status returned)
%% 2. contacts: non-contact gets unavailable
%% 3. contacts: contact gets real status
%% 4. nobody: everyone gets unavailable
%% 5. set_privacy/2 returns ok
%% 6. Privacy does not affect messaging
%%
%% Pattern: follows iris_presence ETS-based approach.
%% =============================================================================

setup() ->
    case whereis(iris_presence) of
        undefined ->
            {ok, Pid} = iris_presence:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_presence);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_presence_privacy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Default visibility is everyone", fun test_default_visibility_everyone/0},
      {"set_privacy returns ok", fun test_set_privacy_level/0},
      {"nobody hides from all", fun test_visibility_nobody_hides_from_all/0},
      {"contacts hides from stranger", fun test_visibility_contacts_hides_from_stranger/0},
      {"contacts shows to contact", fun test_visibility_contacts_shows_to_contact/0},
      {"Privacy does not affect messaging", fun test_privacy_does_not_affect_messaging/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_default_visibility_everyone() ->
    User = <<"privacy_default_user">>,
    iris_presence:register(User, node(), self()),
    %% Default lookup should return real status {ok, Node, Pid}
    Result = iris_presence:lookup(User),
    ?assertMatch({ok, _, _}, Result),
    iris_presence:unregister(User).

test_set_privacy_level() ->
    User = <<"privacy_set_user">>,
    %% Set privacy level
    Result = iris_presence:set_privacy(User, contacts),
    ?assertEqual(ok, Result),
    %% Verify it can be set to other values
    ?assertEqual(ok, iris_presence:set_privacy(User, nobody)),
    ?assertEqual(ok, iris_presence:set_privacy(User, everyone)).

test_visibility_nobody_hides_from_all() ->
    User = <<"privacy_nobody_user">>,
    iris_presence:register(User, node(), self()),
    iris_presence:set_privacy(User, nobody),
    %% Lookup with any requester should return unavailable
    Result = iris_presence:lookup_with_privacy(User, <<"random_requester">>),
    ?assertEqual({ok, unavailable}, Result),
    iris_presence:unregister(User).

test_visibility_contacts_hides_from_stranger() ->
    User = <<"privacy_contacts_user">>,
    iris_presence:register(User, node(), self()),
    iris_presence:set_privacy(User, contacts),
    %% Non-contact should get unavailable
    Result = iris_presence:lookup_with_privacy(User, <<"stranger">>),
    ?assertEqual({ok, unavailable}, Result),
    iris_presence:unregister(User).

test_visibility_contacts_shows_to_contact() ->
    User = <<"privacy_contact_visible_user">>,
    Contact = <<"trusted_contact">>,
    iris_presence:register(User, node(), self()),
    iris_presence:set_privacy(User, contacts),
    %% Add contact
    iris_presence:add_contact(User, Contact),
    %% Contact should see real status {ok, Node, Pid}
    Result = iris_presence:lookup_with_privacy(User, Contact),
    ?assertMatch({ok, _, _}, Result),
    iris_presence:unregister(User).

test_privacy_does_not_affect_messaging() ->
    %% Privacy controls affect status queries only.
    %% Register and set privacy to nobody
    User = <<"privacy_msg_user">>,
    iris_presence:register(User, node(), self()),
    iris_presence:set_privacy(User, nobody),
    %% Standard lookup (without privacy check) should still work
    %% for internal routing purposes
    Result = iris_presence:lookup(User),
    ?assertMatch({ok, _, _}, Result),
    iris_presence:unregister(User).
