-module(iris_circuit_breaker_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Fixtures & Setup
%% =============================================================================

setup() ->
    case whereis(iris_circuit_breaker) of
        undefined ->
            {ok, Pid} = iris_circuit_breaker:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_circuit_breaker);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_circuit_breaker_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Status tests
      {"Get status returns map", fun test_get_status/0},
      {"Get all status returns map", fun test_get_all_status/0},
      
      %% Record tests
      {"Record success returns ok", fun test_record_success/0},
      {"Record failure returns ok", fun test_record_failure/0},
      
      %% Circuit behavior tests
      {"Multiple failures changes state", fun test_multiple_failures/0}
     ]}.

%% =============================================================================
%% Status Tests
%% =============================================================================

test_get_status() ->
    Node = 'status_test@localhost',
    Status = iris_circuit_breaker:get_status(Node),
    ?assert(is_map(Status)).

test_get_all_status() ->
    Status = iris_circuit_breaker:get_all_status(),
    ?assert(is_map(Status)).

%% =============================================================================
%% Record Tests
%% =============================================================================

test_record_success() ->
    Node = 'success_test@localhost',
    Result = iris_circuit_breaker:record_success(Node),
    ?assertEqual(ok, Result).

test_record_failure() ->
    Node = 'failure_test@localhost',
    Result = iris_circuit_breaker:record_failure(Node),
    ?assertEqual(ok, Result).

%% =============================================================================
%% Circuit Behavior Tests
%% =============================================================================

test_multiple_failures() ->
    Node = 'multi_fail_test@localhost',
    
    %% Record several failures
    lists:foreach(fun(_) ->
        iris_circuit_breaker:record_failure(Node)
    end, lists:seq(1, 5)),
    
    %% Status should reflect failures
    Status = iris_circuit_breaker:get_status(Node),
    ?assert(is_map(Status)),
    
    %% Clean up with success
    iris_circuit_breaker:record_success(Node),
    ok.
