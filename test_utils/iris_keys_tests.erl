-module(iris_keys_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% E2EE Key Bundle Storage Unit Tests (RFC-001-AMENDMENT-001)
%% Per TEST_CONTRACT.md: Tests must be deterministic
%% =============================================================================

%% Test fixture - start Mnesia and key server
setup() ->
    %% Create temp Mnesia directory
    Dir = "/tmp/iris_keys_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = filelib:ensure_dir(Dir ++ "/"),
    application:set_env(mnesia, dir, Dir),
    
    %% Start Mnesia
    mnesia:create_schema([node()]),
    application:start(mnesia),
    
    %% Start key server
    {ok, Pid} = iris_keys:start_link(),
    {Pid, Dir}.

cleanup({Pid, Dir}) ->
    %% Stop key server
    gen_server:stop(Pid),
    
    %% Stop Mnesia
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    
    %% Clean up temp directory
    os:cmd("rm -rf " ++ Dir).

%% Generate test keys (32 bytes each for Curve25519)
generate_test_keys() ->
    #{
        identity_key => crypto:strong_rand_bytes(32),
        signed_prekey => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64),  %% Ed25519 signature
        one_time_prekeys => [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 10)]
    }.

%% =============================================================================
%% Test: Upload and Fetch Bundle
%% =============================================================================

upload_fetch_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"upload valid bundle", fun upload_valid_bundle/0},
          {"fetch existing bundle", fun fetch_existing_bundle/0},
          {"fetch non-existing bundle", fun fetch_non_existing_bundle/0}
         ]
     end}.

upload_valid_bundle() ->
    UserId = <<"test_user_1">>,
    Bundle = generate_test_keys(),
    
    ?assertEqual(ok, iris_keys:upload_bundle(UserId, Bundle)),
    
    %% Verify it was stored
    {ok, Count} = iris_keys:get_prekey_count(UserId),
    ?assertEqual(10, Count).

fetch_existing_bundle() ->
    UserId = <<"test_user_2">>,
    Bundle = generate_test_keys(),
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    %% Fetch without consuming OPK
    {ok, FetchedBundle} = iris_keys:fetch_bundle(UserId, false),
    
    ?assertEqual(maps:get(identity_key, Bundle), maps:get(identity_key, FetchedBundle)),
    ?assertEqual(maps:get(signed_prekey, Bundle), maps:get(signed_prekey, FetchedBundle)),
    ?assertEqual(10, maps:get(prekeys_remaining, FetchedBundle)).

fetch_non_existing_bundle() ->
    ?assertEqual({error, not_found}, iris_keys:fetch_bundle(<<"non_existing_user">>)).

%% =============================================================================
%% Test: One-Time Prekey Consumption
%% =============================================================================

opk_consumption_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"consume opk on fetch", fun consume_opk_on_fetch/0},
          {"pop one time prekey", fun pop_one_time_prekey/0},
          {"exhaust prekeys", fun exhaust_prekeys/0}
         ]
     end}.

consume_opk_on_fetch() ->
    UserId = <<"opk_test_user_1">>,
    Bundle = generate_test_keys(),
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    %% Fetch with OPK consumption (default)
    {ok, Fetched1} = iris_keys:fetch_bundle(UserId, true),
    
    ?assertNotEqual(undefined, maps:get(one_time_prekey, Fetched1)),
    ?assertEqual(9, maps:get(prekeys_remaining, Fetched1)),
    
    %% Fetch again - should get different OPK
    {ok, Fetched2} = iris_keys:fetch_bundle(UserId, true),
    ?assertEqual(8, maps:get(prekeys_remaining, Fetched2)).

pop_one_time_prekey() ->
    UserId = <<"opk_test_user_2">>,
    Bundle = generate_test_keys(),
    OriginalOPKs = maps:get(one_time_prekeys, Bundle),
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    %% Pop first prekey
    {ok, PoppedKey, _Index} = iris_keys:pop_one_time_prekey(UserId),
    
    %% Should be the first one in the list
    ?assertEqual(hd(OriginalOPKs), PoppedKey),
    
    %% Count should decrease
    {ok, Count} = iris_keys:get_prekey_count(UserId),
    ?assertEqual(9, Count).

exhaust_prekeys() ->
    UserId = <<"opk_test_user_3">>,
    Bundle = #{
        identity_key => crypto:strong_rand_bytes(32),
        signed_prekey => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64),
        one_time_prekeys => [crypto:strong_rand_bytes(32)]  %% Only 1 prekey
    },
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    %% Pop the only prekey
    {ok, _, _} = iris_keys:pop_one_time_prekey(UserId),
    
    %% No more prekeys
    ?assertEqual({error, no_prekeys}, iris_keys:pop_one_time_prekey(UserId)).

%% =============================================================================
%% Test: Prekey Refill
%% =============================================================================

prekey_refill_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"refill prekeys", fun refill_prekeys/0},
          {"refill invalid prekeys", fun refill_invalid_prekeys/0}
         ]
     end}.

refill_prekeys() ->
    UserId = <<"refill_user">>,
    Bundle = #{
        identity_key => crypto:strong_rand_bytes(32),
        signed_prekey => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64),
        one_time_prekeys => [crypto:strong_rand_bytes(32)]  %% Start with 1
    },
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    %% Refill with 5 more prekeys
    NewPrekeys = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 5)],
    {ok, NewCount} = iris_keys:refill_one_time_prekeys(UserId, NewPrekeys),
    
    ?assertEqual(6, NewCount),
    
    %% Verify count
    {ok, Count} = iris_keys:get_prekey_count(UserId),
    ?assertEqual(6, Count).

refill_invalid_prekeys() ->
    UserId = <<"refill_invalid_user">>,
    Bundle = generate_test_keys(),
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    %% Try to refill with wrong-sized keys
    InvalidPrekeys = [crypto:strong_rand_bytes(16)],  %% 16 bytes instead of 32
    
    ?assertEqual({error, invalid_prekey_size}, 
                 iris_keys:refill_one_time_prekeys(UserId, InvalidPrekeys)).

%% =============================================================================
%% Test: Identity and Signed Prekey Access
%% =============================================================================

key_access_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"get identity key", fun get_identity_key/0},
          {"get signed prekey", fun get_signed_prekey/0}
         ]
     end}.

get_identity_key() ->
    UserId = <<"identity_key_user">>,
    Bundle = generate_test_keys(),
    ExpectedIK = maps:get(identity_key, Bundle),
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    {ok, IK} = iris_keys:get_identity_key(UserId),
    ?assertEqual(ExpectedIK, IK).

get_signed_prekey() ->
    UserId = <<"signed_prekey_user">>,
    Bundle = generate_test_keys(),
    ExpectedSPK = maps:get(signed_prekey, Bundle),
    ExpectedSig = maps:get(signed_prekey_signature, Bundle),
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    {ok, SPK, Sig} = iris_keys:get_signed_prekey(UserId),
    ?assertEqual(ExpectedSPK, SPK),
    ?assertEqual(ExpectedSig, Sig).

%% =============================================================================
%% Test: Bundle Validation
%% =============================================================================

validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"reject missing fields", fun reject_missing_fields/0},
          {"reject wrong key sizes", fun reject_wrong_key_sizes/0}
         ]
     end}.

reject_missing_fields() ->
    UserId = <<"validation_user_1">>,
    
    %% Missing signed_prekey
    InvalidBundle = #{
        identity_key => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64)
    },
    
    ?assertEqual({error, missing_required_fields}, 
                 iris_keys:upload_bundle(UserId, InvalidBundle)).

reject_wrong_key_sizes() ->
    UserId = <<"validation_user_2">>,
    
    %% Wrong identity key size (16 instead of 32)
    InvalidBundle = #{
        identity_key => crypto:strong_rand_bytes(16),
        signed_prekey => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64)
    },
    
    ?assertEqual({error, invalid_key_sizes}, 
                 iris_keys:upload_bundle(UserId, InvalidBundle)).

%% =============================================================================
%% Test: User Management
%% =============================================================================

user_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"list users", fun list_users/0},
          {"delete user keys", fun delete_user_keys/0}
         ]
     end}.

list_users() ->
    %% Upload bundles for multiple users
    lists:foreach(fun(N) ->
        UserId = <<"user_", (integer_to_binary(N))/binary>>,
        Bundle = generate_test_keys(),
        ok = iris_keys:upload_bundle(UserId, Bundle)
    end, lists:seq(1, 3)),
    
    Users = iris_keys:list_users(),
    ?assert(length(Users) >= 3).

delete_user_keys() ->
    UserId = <<"delete_test_user">>,
    Bundle = generate_test_keys(),
    
    ok = iris_keys:upload_bundle(UserId, Bundle),
    
    %% Verify exists
    {ok, _} = iris_keys:get_identity_key(UserId),
    
    %% Delete
    ok = iris_keys:delete_user_keys(UserId),
    
    %% Verify deleted
    ?assertEqual({error, not_found}, iris_keys:get_identity_key(UserId)).
