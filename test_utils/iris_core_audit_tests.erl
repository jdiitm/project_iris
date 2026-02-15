-module(iris_core_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Audit Mitigation Tests for iris_core.erl
%% =============================================================================
%%
%% Tests cover:
%% - 3.2/6.1: mTLS enforcement must be checked at startup
%% - 4.3: Production mode must reject default cookie 'iris_secret'
%% - 5.4: Bare spawns in init/1 must be replaced with supervised workers
%% - 4.2: CP consistency mode must be fatal in production
%% - 6.5: Dedup keys must use strong hash (not phash2)
%% - 6.7: nuke_and_recreate must be blocked in production mode
%%
%% Note: erlang:set_cookie/2 fails on non-distributed nodes (nonode@nohost),
%% so we test via validate_production_cookie/1 which accepts the cookie atom.
%% Note: init:get_argument cannot be changed at runtime, so we test
%% check_mtls_enforcement/1 which accepts ssl_configured boolean.
%% =============================================================================

%% =============================================================================
%% 4.3: Cookie Enforcement
%% =============================================================================

cookie_enforcement_test_() ->
    [
     {"production mode rejects default cookie iris_secret", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          application:set_env(iris_core, deployment_mode, production),
          try
              ?assertEqual({error, default_cookie_in_production},
                           iris_core:validate_production_cookie(iris_secret))
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end
          end
      end},

     {"development mode allows default cookie", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          application:set_env(iris_core, deployment_mode, development),
          try
              ?assertEqual(ok, iris_core:validate_production_cookie(iris_secret))
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end
          end
      end},

     {"production mode allows non-default cookie", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          application:set_env(iris_core, deployment_mode, production),
          try
              ?assertEqual(ok,
                           iris_core:validate_production_cookie(secure_prod_cookie))
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end
          end
      end}
    ].

%% =============================================================================
%% 5.4: Supervised Cluster Join (no bare spawns in init/1)
%% =============================================================================

cluster_join_worker_test_() ->
    [
     {"iris_cluster_join_worker module exists and exports start_link", fun() ->
          Exports = iris_cluster_join_worker:module_info(exports),
          ?assert(lists:member({start_link, 1}, Exports))
      end},

     {"iris_cluster_join_worker is a gen_server", fun() ->
          Exports = iris_cluster_join_worker:module_info(exports),
          ?assert(lists:member({init, 1}, Exports)),
          ?assert(lists:member({handle_info, 2}, Exports))
      end},

     {"cluster join worker accepts cluster_join task", fun() ->
          %% Should not crash when started with cluster_join task
          %% (will just find no peers in unit test and log standalone mode)
          {ok, Pid} = iris_cluster_join_worker:start_link(cluster_join),
          ?assert(is_pid(Pid)),
          gen_server:stop(Pid)
      end},

     {"cluster join worker accepts region_wiring task", fun() ->
          {ok, Pid} = iris_cluster_join_worker:start_link(region_wiring),
          ?assert(is_pid(Pid)),
          gen_server:stop(Pid)
      end}
    ].

%% =============================================================================
%% 4.2: CP Consistency Mode Hard-Fail
%% =============================================================================

cp_mode_test_() ->
    [
     {"production mode rejects CP consistency mode", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          OldCons = application:get_env(iris_core, consistency_mode, undefined),
          application:set_env(iris_core, deployment_mode, production),
          application:set_env(iris_core, consistency_mode, cp),
          try
              ?assertEqual({error, cp_not_implemented},
                           iris_core:validate_consistency_mode())
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end,
              case OldCons of
                  undefined -> application:unset_env(iris_core, consistency_mode);
                  _ -> application:set_env(iris_core, consistency_mode, OldCons)
              end
          end
      end},

     {"development mode also rejects CP (CRIT-01: no silent fallback)", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          OldCons = application:get_env(iris_core, consistency_mode, undefined),
          application:set_env(iris_core, deployment_mode, development),
          application:set_env(iris_core, consistency_mode, cp),
          try
              ?assertEqual({error, cp_not_implemented},
                           iris_core:validate_consistency_mode())
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end,
              case OldCons of
                  undefined -> application:unset_env(iris_core, consistency_mode);
                  _ -> application:set_env(iris_core, consistency_mode, OldCons)
              end
          end
      end},

     {"hardened_ap mode always succeeds", fun() ->
          OldCons = application:get_env(iris_core, consistency_mode, undefined),
          application:set_env(iris_core, consistency_mode, hardened_ap),
          try
              ?assertEqual(ok, iris_core:validate_consistency_mode())
          after
              case OldCons of
                  undefined -> application:unset_env(iris_core, consistency_mode);
                  _ -> application:set_env(iris_core, consistency_mode, OldCons)
              end
          end
      end}
    ].

%% =============================================================================
%% 3.2/6.1: mTLS Startup Enforcement
%% =============================================================================

mtls_enforcement_test_() ->
    [
     {"enforce_mtls=true without ssl_dist rejects startup", fun() ->
          OldEnforce = application:get_env(iris_core, enforce_mtls, undefined),
          application:set_env(iris_core, enforce_mtls, true),
          try
              ?assertExit(mtls_not_configured,
                          iris_core:check_mtls_enforcement(false))
          after
              case OldEnforce of
                  undefined -> application:unset_env(iris_core, enforce_mtls);
                  _ -> application:set_env(iris_core, enforce_mtls, OldEnforce)
              end
          end
      end},

     {"enforce_mtls=true with ssl_dist allows startup", fun() ->
          OldEnforce = application:get_env(iris_core, enforce_mtls, undefined),
          application:set_env(iris_core, enforce_mtls, true),
          try
              ?assertEqual(ok, iris_core:check_mtls_enforcement(true))
          after
              case OldEnforce of
                  undefined -> application:unset_env(iris_core, enforce_mtls);
                  _ -> application:set_env(iris_core, enforce_mtls, OldEnforce)
              end
          end
      end},

     {"enforce_mtls=false logs warning but succeeds", fun() ->
          OldEnforce = application:get_env(iris_core, enforce_mtls, undefined),
          application:set_env(iris_core, enforce_mtls, false),
          try
              ?assertEqual(ok, iris_core:check_mtls_enforcement(false))
          after
              case OldEnforce of
                  undefined -> application:unset_env(iris_core, enforce_mtls);
                  _ -> application:set_env(iris_core, enforce_mtls, OldEnforce)
              end
          end
      end}
    ].

%% =============================================================================
%% 6.5: Dedup Key Uses Strong Hash (not phash2)
%% =============================================================================

dedup_key_test_() ->
    [
     {"dedup key with SeqNo uses 16-char hex suffix (64-bit hash)", fun() ->
          Key = iris_core:make_dedup_key(<<"alice">>, {1, <<"hello world">>}),
          %% Key format: alice:1:<16-char-hex>
          ?assert(is_binary(Key)),
          Parts = binary:split(Key, <<":">>, [global]),
          ?assertEqual(3, length(Parts)),
          [_User, _SeqStr, HashPart] = Parts,
          %% 8 bytes of SHA-256 encoded as hex = 16 chars
          ?assertEqual(16, byte_size(HashPart))
      end},

     {"dedup key without SeqNo uses 16-char hex suffix", fun() ->
          Key = iris_core:make_dedup_key(<<"bob">>, <<"raw message">>),
          %% Key format: bob:hash:<16-char-hex>
          Parts = binary:split(Key, <<":">>, [global]),
          ?assertEqual(3, length(Parts)),
          [_User, <<"hash">>, HashPart] = Parts,
          ?assertEqual(16, byte_size(HashPart))
      end},

     {"dedup key is deterministic", fun() ->
          K1 = iris_core:make_dedup_key(<<"alice">>, {1, <<"hello">>}),
          K2 = iris_core:make_dedup_key(<<"alice">>, {1, <<"hello">>}),
          ?assertEqual(K1, K2)
      end},

     {"different messages produce different dedup keys", fun() ->
          K1 = iris_core:make_dedup_key(<<"alice">>, {1, <<"hello">>}),
          K2 = iris_core:make_dedup_key(<<"alice">>, {1, <<"world">>}),
          ?assertNotEqual(K1, K2)
      end}
    ].

%% =============================================================================
%% 6.7: nuke_and_recreate Blocked in Production
%% =============================================================================

nuke_guard_test_() ->
    [
     {"production mode blocks nuke even with allow_table_nuke=true", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          OldNuke = application:get_env(iris_core, allow_table_nuke, undefined),
          application:set_env(iris_core, deployment_mode, production),
          application:set_env(iris_core, allow_table_nuke, true),
          try
              ?assertExit({nuke_blocked_in_production, test_table},
                          iris_core:nuke_and_recreate_table(test_table))
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end,
              case OldNuke of
                  undefined -> application:unset_env(iris_core, allow_table_nuke);
                  _ -> application:set_env(iris_core, allow_table_nuke, OldNuke)
              end
          end
      end},

     {"development mode allows nuke with allow_table_nuke=true", fun() ->
          OldMode = application:get_env(iris_core, deployment_mode, undefined),
          application:set_env(iris_core, deployment_mode, development),
          %% We don't actually nuke -- just verify the function doesn't exit
          %% with nuke_blocked_in_production. It will fail for other reasons
          %% (no Mnesia) but that's acceptable.
          try
              iris_core:nuke_and_recreate_table(test_table)
          catch
              exit:{nuke_blocked_in_production, _} ->
                  ?assert(false);  %% Should NOT happen in development mode
              _:_ ->
                  ok  %% Any other error is fine (no Mnesia, etc.)
          after
              case OldMode of
                  undefined -> application:unset_env(iris_core, deployment_mode);
                  _ -> application:set_env(iris_core, deployment_mode, OldMode)
              end
          end
      end}
    ].

%% =============================================================================
%% B-6 AUDIT: e2ee_key_bundle reconciliation must keep newer record
%% =============================================================================
%% RFC 7.1.1: "Key bundles: Union (all bundles are valid)"
%% Since the table is type=set (one record per user_id), union semantics
%% means "keep the record with the most recent updated_at."
%% The bug: should_overwrite defaulted to false (always keep local),
%% which could overwrite a newer key bundle with an older one during healing.

key_bundle_reconcile_test_() ->
    [
     {"B-6: Remote key bundle with newer updated_at overwrites local", fun() ->
          %% Record shape: {key_bundle, UserId, IdentityKey, SignedPreKey,
          %%   SignedPreKeySig, SignedPreKeyTimestamp, OneTimePrekeys, CreatedAt, UpdatedAt}
          LocalRec = {key_bundle, <<"alice">>, <<"ik_local">>, <<"spk_local">>,
                      <<"spk_sig">>, 1000, [<<"opk1">>], 1000, 1000},
          RemoteRec = {key_bundle, <<"alice">>, <<"ik_remote">>, <<"spk_remote">>,
                       <<"spk_sig2">>, 2000, [<<"opk2">>], 1000, 2000},
          %% Remote has updated_at=2000, local has updated_at=1000
          %% should_overwrite must return true (take remote's newer bundle)
          ?assertEqual(true,
                       iris_core:should_overwrite(e2ee_key_bundle, RemoteRec, LocalRec))
      end},

     {"B-6: Remote key bundle with older updated_at does not overwrite local", fun() ->
          LocalRec = {key_bundle, <<"bob">>, <<"ik_local">>, <<"spk_local">>,
                      <<"spk_sig">>, 2000, [<<"opk1">>], 1000, 2000},
          RemoteRec = {key_bundle, <<"bob">>, <<"ik_remote">>, <<"spk_remote">>,
                       <<"spk_sig2">>, 1000, [<<"opk2">>], 1000, 1000},
          %% Remote has updated_at=1000, local has updated_at=2000
          %% should_overwrite must return false (keep local's newer bundle)
          ?assertEqual(false,
                       iris_core:should_overwrite(e2ee_key_bundle, RemoteRec, LocalRec))
      end},

     {"B-6: Equal updated_at keeps local (deterministic tiebreak)", fun() ->
          LocalRec = {key_bundle, <<"carol">>, <<"ik_local">>, <<"spk_local">>,
                      <<"spk_sig">>, 1500, [<<"opk1">>], 1000, 1500},
          RemoteRec = {key_bundle, <<"carol">>, <<"ik_remote">>, <<"spk_remote">>,
                       <<"spk_sig2">>, 1500, [<<"opk2">>], 1000, 1500},
          %% Same timestamp: keep local (conservative)
          ?assertEqual(false,
                       iris_core:should_overwrite(e2ee_key_bundle, RemoteRec, LocalRec))
      end}
    ].
