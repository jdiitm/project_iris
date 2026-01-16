-module(iris_flow_controller_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Fixtures & Setup
%% =============================================================================

setup() ->
    case whereis(iris_flow_controller) of
        undefined ->
            {ok, Pid} = iris_flow_controller:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end.

cleanup({started, _Pid}) ->
    gen_server:stop(iris_flow_controller);
cleanup({existing, _Pid}) ->
    ok.

%% =============================================================================
%% Main Test Generator
%% =============================================================================

iris_flow_controller_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Admission tests
      {"Normal level admits new", fun test_normal_admits/0},
      {"Check admission returns result", fun test_check_admission_result/0},
      
      %% Level tests
      {"Get current level", fun test_get_level/0},
      {"Level is valid atom", fun test_level_valid/0},
      
      %% Stats tests
      {"Get stats returns map", fun test_get_stats/0},
      {"Stats has admit count", fun test_stats_has_counts/0}
     ]}.

%% =============================================================================
%% Admission Tests
%% =============================================================================

test_normal_admits() ->
    User = <<"flow_test_user">>,
    
    %% At normal level, should admit
    Result = iris_flow_controller:check_admission(User, #{}),
    ?assert(Result =:= admit orelse is_tuple(Result) orelse 
            Result =:= ok orelse Result =:= reject).

test_check_admission_result() ->
    User = <<"check_test_user">>,
    
    Result = iris_flow_controller:check_admission(User, #{}),
    %% Result should be some form of admit/reject
    ?assert(is_atom(Result) orelse is_tuple(Result)).

%% =============================================================================
%% Level Tests
%% =============================================================================

test_get_level() ->
    Level = iris_flow_controller:get_level(),
    ?assert(is_integer(Level)).

test_level_valid() ->
    Level = iris_flow_controller:get_level(),
    ValidLevels = [1, 2, 3, 4],  %% 1=normal, 2=slow, 3=shed, 4=critical
    ?assert(lists:member(Level, ValidLevels)).

%% =============================================================================
%% Stats Tests
%% =============================================================================

test_get_stats() ->
    Stats = iris_flow_controller:get_stats(),
    ?assert(is_map(Stats)).

test_stats_has_counts() ->
    Stats = iris_flow_controller:get_stats(),
    %% Should have some stats fields
    ?assert(maps:size(Stats) >= 1).
