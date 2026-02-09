-module(iris_key_contacts_persistence_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% GAP-3: Key Contact Tracking Persistence Tests (Amendment 5.3.2)
%%
%% The key contacts table tracks which users have fetched each other's key
%% bundles. When a user's Identity Key changes, the server MUST notify all
%% contacts. If the contact graph is lost on restart (RAM-only ETS), Bob
%% will never learn Alice changed her key -- creating MITM vulnerability.
%%
%% These tests verify:
%% 1. Key contacts survive a gen_server restart
%% 2. Key contacts are readable after restart (Mnesia persistence)
%% =============================================================================

setup() ->
    %% Start Mnesia with a schema
    catch mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Stop any running iris_keys
    catch gen_server:stop(iris_keys),
    timer:sleep(50),
    
    %% Start iris_keys (creates tables)
    {ok, Pid} = iris_keys:start_link(),
    {started, Pid}.

cleanup({started, _Pid}) ->
    catch gen_server:stop(iris_keys),
    catch mnesia:stop(),
    timer:sleep(50),
    ok.

iris_key_contacts_persistence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Key contacts survive gen_server restart",
       {timeout, 15, fun test_contacts_survive_restart/0}},
      {"Key contacts are not empty after restart",
       {timeout, 15, fun test_contacts_not_empty_after_restart/0}}
     ]}.

test_contacts_survive_restart() ->
    %% Record some contacts
    iris_keys:record_key_contact(<<"alice">>, <<"bob">>),
    iris_keys:record_key_contact(<<"alice">>, <<"charlie">>),
    iris_keys:record_key_contact(<<"dave">>, <<"bob">>),
    
    %% Verify contacts exist before restart
    AliceContacts1 = iris_keys:get_key_contacts(<<"alice">>),
    ?assertEqual(2, length(AliceContacts1)),
    ?assert(lists:member(<<"bob">>, AliceContacts1)),
    ?assert(lists:member(<<"charlie">>, AliceContacts1)),
    
    %% Restart iris_keys gen_server (simulates node restart)
    gen_server:stop(iris_keys),
    timer:sleep(100),
    {ok, _NewPid} = iris_keys:start_link(),
    timer:sleep(100),
    
    %% KEY ASSERTION: Contacts must survive restart
    AliceContacts2 = iris_keys:get_key_contacts(<<"alice">>),
    ?assertEqual(2, length(AliceContacts2)),
    ?assert(lists:member(<<"bob">>, AliceContacts2)),
    ?assert(lists:member(<<"charlie">>, AliceContacts2)),
    
    DaveContacts = iris_keys:get_key_contacts(<<"dave">>),
    ?assertEqual(1, length(DaveContacts)),
    ?assert(lists:member(<<"bob">>, DaveContacts)).

test_contacts_not_empty_after_restart() ->
    %% Record a contact
    iris_keys:record_key_contact(<<"eve">>, <<"frank">>),
    
    %% Restart
    gen_server:stop(iris_keys),
    timer:sleep(100),
    {ok, _} = iris_keys:start_link(),
    timer:sleep(100),
    
    %% Must not be empty (this is the core GAP-3 failure: ETS is wiped)
    Contacts = iris_keys:get_key_contacts(<<"eve">>),
    ?assertNotEqual([], Contacts).
