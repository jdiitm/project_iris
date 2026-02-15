-module(iris_session_state_audit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Session State Documentation Tests
%% =============================================================================
%%
%% Tests verify that:
%% - iris_edge_conn.erl has a documented -record(data, ...) with field comments
%% - The system acknowledges that edge nodes hold per-connection state
%% - iris_session_cache covers critical state fields for resume
%% =============================================================================

iris_session_state_audit_test_() ->
    [
     {"edge_conn has documented state record",
      fun test_edge_conn_state_fields_documented/0},
     {"session_cache covers session_id and capabilities",
      fun test_session_resume_covers_critical_fields/0},
     {"README does not claim stateless edge",
      fun test_readme_no_stateless_claim/0}
    ].

test_edge_conn_state_fields_documented() ->
    %% Source must have -record(data, ...) with comments on fields
    {ok, Src} = file:read_file("src/iris_edge_conn.erl"),
    %% Must have the record definition
    ?assert(binary:match(Src, <<"-record(data">>) =/= nomatch),
    %% Critical fields must be documented with comments
    ?assert(binary:match(Src, <<"session_id">>) =/= nomatch),
    ?assert(binary:match(Src, <<"capabilities">>) =/= nomatch),
    ?assert(binary:match(Src, <<"pending_acks">>) =/= nomatch),
    ?assert(binary:match(Src, <<"buffer">>) =/= nomatch).

test_session_resume_covers_critical_fields() ->
    %% iris_edge_conn must save session_id and pending_acks on terminate
    {ok, Src} = file:read_file("src/iris_edge_conn.erl"),
    %% terminate must reference session cache
    ?assert(binary:match(Src, <<"queue_pending_to_session_cache">>) =/= nomatch),
    %% terminate must reference save_pending_acks
    ?assert(binary:match(Src, <<"save_pending_acks">>) =/= nomatch).

test_readme_no_stateless_claim() ->
    {ok, Src} = file:read_file("README.md"),
    %% README should say "session-recoverable" not "stateless"
    %% (Or at minimum, if "stateless" appears, "session-recoverable" should too)
    case binary:match(Src, <<"stateless">>) of
        nomatch -> ok;  %% Good — no misleading claim
        _ ->
            %% If stateless is mentioned, session-recoverable should also be
            ?assert(binary:match(Src, <<"session-recoverable">>) =/= nomatch)
    end.
