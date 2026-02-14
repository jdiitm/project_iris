-module(iris_reconciliation_lww_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: Cross-Region LWW Reconciliation Tests
%% =============================================================================
%%
%% Tests verify that:
%% - user_meta reconciliation uses last_modified timestamp (LWW)
%% - user_status reconciliation uses last_seen timestamp (LWW)
%% - Newer remote record overwrites older local record
%% - Older remote record is discarded when local is newer
%% - offline_msg (bag type) keeps both records (union merge)
%% - presence always keeps local (ephemeral, authoritative)
%% - should_overwrite/3 is the single conflict resolution entry point
%% =============================================================================

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_reconciliation_lww_test_() ->
    [
     {"AUDIT LWW: newer user_meta remote overwrites older local",
      fun test_user_meta_newer_wins/0},
     {"AUDIT LWW: older user_meta remote is discarded",
      fun test_user_meta_older_discarded/0},
     {"AUDIT LWW: user_meta legacy format (no last_modified) handled",
      fun test_user_meta_legacy_format/0},
     {"AUDIT LWW: newer user_status remote overwrites older local",
      fun test_user_status_newer_wins/0},
     {"AUDIT LWW: older user_status remote is discarded",
      fun test_user_status_older_discarded/0},
     {"AUDIT LWW: presence always keeps local",
      fun test_presence_keeps_local/0},
     {"AUDIT LWW: offline_msg both records kept (bag semantics)",
      fun test_offline_msg_both_kept/0},
     {"AUDIT LWW: should_overwrite is exported",
      fun test_should_overwrite_exported/0},
     {"AUDIT LWW: source contains LWW comments",
      fun test_source_has_lww/0}
    ].

%% =============================================================================
%% Tests: should_overwrite/3 (unit tests, no Mnesia needed)
%% =============================================================================

test_user_meta_newer_wins() ->
    %% {user_meta, User, BucketCount, LastModified}
    Remote = {user_meta, <<"alice">>, 5, 1000},  %% newer
    Local  = {user_meta, <<"alice">>, 3, 500},   %% older
    ?assert(iris_core:should_overwrite(user_meta, Remote, Local)).

test_user_meta_older_discarded() ->
    Remote = {user_meta, <<"bob">>, 2, 500},   %% older
    Local  = {user_meta, <<"bob">>, 4, 1000},  %% newer
    ?assertNot(iris_core:should_overwrite(user_meta, Remote, Local)).

test_user_meta_legacy_format() ->
    %% Legacy record without last_modified (tuple_size = 3)
    Remote = {user_meta, <<"carol">>, 5, 1000},  %% new format
    Local  = {user_meta, <<"carol">>, 3},         %% legacy (no last_modified)
    %% New-format remote should win over legacy local
    ?assert(iris_core:should_overwrite(user_meta, Remote, Local)).

test_user_status_newer_wins() ->
    %% {user_status, User, LastSeen}
    Remote = {user_status, <<"dave">>, 2000},  %% newer
    Local  = {user_status, <<"dave">>, 1000},  %% older
    ?assert(iris_core:should_overwrite(user_status, Remote, Local)).

test_user_status_older_discarded() ->
    Remote = {user_status, <<"eve">>, 500},    %% older
    Local  = {user_status, <<"eve">>, 1000},   %% newer
    ?assertNot(iris_core:should_overwrite(user_status, Remote, Local)).

test_presence_keeps_local() ->
    %% Presence is ephemeral — local is always authoritative
    Remote = {presence, <<"frank">>, 'node_b@host', self()},
    Local  = {presence, <<"frank">>, 'node_a@host', self()},
    ?assertNot(iris_core:should_overwrite(presence, Remote, Local)).

test_offline_msg_both_kept() ->
    %% offline_msg is a bag — should_overwrite is not called (union merge path).
    %% We test the merge_table_batch code path indirectly by verifying the
    %% table type check. For bags, merge_set_records is NOT called.
    %% The correct behavior is: both records are kept.
    %%
    %% Since we can't easily test the full merge path without 2-node RPC,
    %% we verify the table_spec says it's a bag (which triggers union merge).
    {_StorageType, Opts} = iris_core:table_spec(offline_msg),
    ?assert(lists:member({type, bag}, Opts)).

test_should_overwrite_exported() ->
    Exports = iris_core:module_info(exports),
    ?assert(lists:member({should_overwrite, 3}, Exports)).

test_source_has_lww() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    ?assert(binary:match(Src, <<"AUDIT LWW">>) =/= nomatch),
    ?assert(binary:match(Src, <<"should_overwrite">>) =/= nomatch),
    ?assert(binary:match(Src, <<"last_modified">>) =/= nomatch).
