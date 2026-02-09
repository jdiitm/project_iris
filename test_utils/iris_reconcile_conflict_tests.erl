-module(iris_reconcile_conflict_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% F1: Split-Brain Conflict Resolution Tests (RFC 7.1.1)
%%
%% merge_table_batch/3 previously performed a blind union merge, overwriting
%% newer local records with stale remote copies. The fix adds should_overwrite/3
%% for timestamp-aware Last-Writer-Wins conflict resolution.
%% =============================================================================

-record(group_member, {
    key         :: {binary(), binary()},
    role        :: admin | member,
    joined_at   :: integer(),
    added_by    :: binary(),
    last_seen   :: integer()
}).

iris_reconcile_conflict_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"should_overwrite is exported",
       fun test_should_overwrite_exported/0},
      {"rejects stale remote group_member (local newer)",
       fun test_rejects_stale_remote/0},
      {"accepts newer remote group_member (local older)",
       fun test_accepts_newer_remote/0},
      {"preserves local presence (ephemeral, local authoritative)",
       fun test_presence_keeps_local/0}
     ]}.

setup() ->
    application:stop(mnesia),
    application:set_env(mnesia, dir, "/tmp/iris_f1_conflict_test_" ++
                        integer_to_list(erlang:system_time(millisecond))),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(group_member, [
        {attributes, record_info(fields, group_member)},
        {type, set},
        {ram_copies, [node()]}
    ]),
    mnesia:wait_for_tables([group_member], 5000),
    ok.

cleanup(_) ->
    mnesia:delete_table(group_member),
    application:stop(mnesia),
    ok.

test_should_overwrite_exported() ->
    Exports = iris_core:module_info(exports),
    ?assert(lists:member({should_overwrite, 3}, Exports)).

test_rejects_stale_remote() ->
    Key = {<<"grp">>, <<"usr">>},
    Local = #group_member{key = Key, role = admin, joined_at = 100,
                          added_by = <<"c">>, last_seen = 2000},
    Remote = #group_member{key = Key, role = member, joined_at = 100,
                           added_by = <<"c">>, last_seen = 1000},
    %% Remote is older → should NOT overwrite
    ?assertEqual(false, iris_core:should_overwrite(group_member, Remote, Local)).

test_accepts_newer_remote() ->
    Key = {<<"grp">>, <<"usr">>},
    Local = #group_member{key = Key, role = member, joined_at = 100,
                          added_by = <<"c">>, last_seen = 1000},
    Remote = #group_member{key = Key, role = admin, joined_at = 100,
                           added_by = <<"c">>, last_seen = 2000},
    %% Remote is newer → should overwrite
    ?assertEqual(true, iris_core:should_overwrite(group_member, Remote, Local)).

test_presence_keeps_local() ->
    %% Presence is ram_copies / ephemeral: local is authoritative
    Local = {presence, <<"user_a">>, online, node()},
    Remote = {presence, <<"user_a">>, offline, 'other@node'},
    ?assertEqual(false, iris_core:should_overwrite(presence, Remote, Local)).
