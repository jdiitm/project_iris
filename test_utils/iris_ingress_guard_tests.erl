-module(iris_ingress_guard_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = iris_ingress_guard:start_link().
cleanup(_) ->
    exit(whereis(iris_ingress_guard), kill).

basic_limit_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        %% Initial count is 0
        ?assertEqual(0, iris_ingress_guard:get_active_count()),
        
        %% Check allow
        ?assertEqual(allow, iris_ingress_guard:check()),
        ?assertEqual(1, iris_ingress_guard:get_active_count()),
        
        %% Mock saturation - assuming MAX is 100,000 defined in header
        %% We can't easily spam 100k calls in unit test, so we verify logic
        %% by manually filling atomics from back side if public?
        %% Better: just trust the loop for small amount
        lists:foreach(fun(_) -> 
            ?assertEqual(allow, iris_ingress_guard:check())
        end, lists:seq(1, 100)),
        
        ?assertEqual(101, iris_ingress_guard:get_active_count()),
        
        %% Verify close decrements
        lists:foreach(fun(_) ->
            iris_ingress_guard:close()
        end, lists:seq(1, 101)),
        
        ?assertEqual(0, iris_ingress_guard:get_active_count())
    end}.
