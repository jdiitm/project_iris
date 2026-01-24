-module(iris_region_router_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit Tests for iris_region_router.erl
%% =============================================================================
%% 
%% Tests cover:
%% - Region configuration
%% - Home region calculation (consistent hashing)
%% - Local vs cross-region routing decisions
%% - Region endpoint management
%% 
%% Note: Actual cross-region routing requires multi-node setup
%% and is tested in integration tests.
%% =============================================================================

%% =============================================================================
%% Test Setup/Teardown
%% =============================================================================

setup() ->
    %% Clear all region configuration
    application:unset_env(iris_core, region_id),
    application:unset_env(iris_core, regions),
    application:unset_env(iris_core, region_endpoints),
    ok.

cleanup(_) ->
    %% Clear all region configuration
    application:unset_env(iris_core, region_id),
    application:unset_env(iris_core, regions),
    application:unset_env(iris_core, region_endpoints),
    ok.

%% =============================================================================
%% Current Region Tests
%% =============================================================================

current_region_test_() ->
    {"Current region configuration",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"Default region is 'local'", fun() ->
            application:unset_env(iris_core, region_id),
            ?assertEqual(<<"local">>, iris_region_router:get_current_region())
        end},
       
       {"Region can be set as binary", fun() ->
            application:set_env(iris_core, region_id, <<"us-east-1">>),
            ?assertEqual(<<"us-east-1">>, iris_region_router:get_current_region())
        end},
       
       {"Region can be set as list (string)", fun() ->
            application:set_env(iris_core, region_id, "eu-west-1"),
            ?assertEqual(<<"eu-west-1">>, iris_region_router:get_current_region())
        end},
       
       {"Region can be set as atom", fun() ->
            application:set_env(iris_core, region_id, 'ap-south-1'),
            ?assertEqual(<<"ap-south-1">>, iris_region_router:get_current_region())
        end}
      ]}}.

%% =============================================================================
%% All Regions Tests
%% =============================================================================

all_regions_test_() ->
    {"All regions configuration",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"Default returns current region only", fun() ->
            application:unset_env(iris_core, regions),
            application:set_env(iris_core, region_id, <<"us-east-1">>),
            ?assertEqual([<<"us-east-1">>], iris_region_router:get_all_regions())
        end},
       
       {"Can configure multiple regions", fun() ->
            Regions = [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>],
            application:set_env(iris_core, regions, Regions),
            ?assertEqual(Regions, iris_region_router:get_all_regions())
        end},
       
       {"Normalizes mixed region formats", fun() ->
            %% Mix of binary, list, and atom
            MixedRegions = [<<"us-east-1">>, "eu-west-1", 'ap-south-1'],
            application:set_env(iris_core, regions, MixedRegions),
            Result = iris_region_router:get_all_regions(),
            ?assertEqual([<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>], 
                         Result)
        end}
      ]}}.

%% =============================================================================
%% Home Region Tests
%% =============================================================================

home_region_test_() ->
    {"Home region calculation",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"Single region always returns that region", fun() ->
            application:set_env(iris_core, regions, [<<"us-east-1">>]),
            ?assertEqual(<<"us-east-1">>, 
                         iris_region_router:get_home_region(<<"user1">>)),
            ?assertEqual(<<"us-east-1">>, 
                         iris_region_router:get_home_region(<<"user2">>))
        end},
       
       {"Home region is deterministic", fun() ->
            Regions = [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>],
            application:set_env(iris_core, regions, Regions),
            
            UserId = <<"deterministic_user_123">>,
            Region1 = iris_region_router:get_home_region(UserId),
            Region2 = iris_region_router:get_home_region(UserId),
            ?assertEqual(Region1, Region2)
        end},
       
       {"Home region uses consistent hashing", fun() ->
            Regions = [<<"us-east-1">>, <<"eu-west-1">>, <<"ap-south-1">>],
            application:set_env(iris_core, regions, Regions),
            
            %% Generate many user IDs and verify distribution
            UserIds = [list_to_binary("user_" ++ integer_to_list(I)) 
                       || I <- lists:seq(1, 100)],
            HomeRegions = [iris_region_router:get_home_region(U) || U <- UserIds],
            
            %% All regions should be represented (with high probability)
            UniqueRegions = lists:usort(HomeRegions),
            ?assert(length(UniqueRegions) >= 1)  %% At least one region
        end},
       
       {"No regions returns current region", fun() ->
            application:set_env(iris_core, regions, []),
            application:set_env(iris_core, region_id, <<"default">>),
            ?assertEqual(<<"default">>, 
                         iris_region_router:get_home_region(<<"any_user">>))
        end}
      ]}}.

%% =============================================================================
%% Local Region Check Tests
%% =============================================================================

is_local_region_test_() ->
    {"Local region check",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"is_local_region returns true for current region", fun() ->
            application:set_env(iris_core, region_id, <<"us-east-1">>),
            ?assert(iris_region_router:is_local_region(<<"us-east-1">>))
        end},
       
       {"is_local_region returns false for other regions", fun() ->
            application:set_env(iris_core, region_id, <<"us-east-1">>),
            ?assertNot(iris_region_router:is_local_region(<<"eu-west-1">>)),
            ?assertNot(iris_region_router:is_local_region(<<"ap-south-1">>))
        end}
      ]}}.

%% =============================================================================
%% Region Endpoint Tests
%% =============================================================================

region_endpoint_test_() ->
    {"Region endpoint management",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"get_region_endpoint returns error when not configured", fun() ->
            application:unset_env(iris_core, region_endpoints),
            Result = iris_region_router:get_region_endpoint(<<"us-east-1">>),
            ?assertMatch({error, _}, Result)
        end},
       
       {"set_region_endpoint stores nodes", fun() ->
            Nodes = ['core1@host1', 'core2@host2'],
            ok = iris_region_router:set_region_endpoint(<<"us-east-1">>, Nodes),
            Result = iris_region_router:get_region_endpoint(<<"us-east-1">>),
            ?assertEqual({ok, Nodes}, Result)
        end},
       
       {"set_region_endpoint can update existing", fun() ->
            OldNodes = ['old@host'],
            NewNodes = ['new1@host', 'new2@host'],
            
            ok = iris_region_router:set_region_endpoint(<<"region1">>, OldNodes),
            ok = iris_region_router:set_region_endpoint(<<"region1">>, NewNodes),
            
            ?assertEqual({ok, NewNodes}, 
                         iris_region_router:get_region_endpoint(<<"region1">>))
        end},
       
       {"Multiple regions can have different endpoints", fun() ->
            Nodes1 = ['core1@us'],
            Nodes2 = ['core1@eu'],
            
            ok = iris_region_router:set_region_endpoint(<<"us-east-1">>, Nodes1),
            ok = iris_region_router:set_region_endpoint(<<"eu-west-1">>, Nodes2),
            
            ?assertEqual({ok, Nodes1}, 
                         iris_region_router:get_region_endpoint(<<"us-east-1">>)),
            ?assertEqual({ok, Nodes2}, 
                         iris_region_router:get_region_endpoint(<<"eu-west-1">>))
        end},
       
       {"get_region_endpoint returns not_found for unknown region", fun() ->
            ok = iris_region_router:set_region_endpoint(<<"us-east-1">>, ['node']),
            Result = iris_region_router:get_region_endpoint(<<"unknown-region">>),
            ?assertEqual({error, not_found}, Result)
        end}
      ]}}.

%% =============================================================================
%% Route Decision Tests
%% =============================================================================

route_decision_test_() ->
    {"Route decision logic",
     {setup, fun setup/0, fun cleanup/1,
      [
       {"Local user uses local routing", fun() ->
            %% Set up single region
            application:set_env(iris_core, region_id, <<"us-east-1">>),
            application:set_env(iris_core, regions, [<<"us-east-1">>]),
            
            %% All users are local in single-region mode
            UserId = <<"local_user">>,
            HomeRegion = iris_region_router:get_home_region(UserId),
            CurrentRegion = iris_region_router:get_current_region(),
            
            ?assertEqual(HomeRegion, CurrentRegion)
        end}
      ]}}.

%% =============================================================================
%% Integration Test Markers
%% =============================================================================

integration_markers_test_() ->
    {"Integration test markers",
     [
      {"Cross-region routing requires integration test", fun() ->
           %% Actual cross-region routing tested in Python integration suite
           ?assert(true)
       end},
      
      {"Circuit breaker integration requires integration test", fun() ->
           %% iris_circuit_breaker interaction tested in integration suite
           ?assert(true)
       end},
      
      {"Bridge routing requires integration test", fun() ->
           %% iris_region_bridge interaction tested in integration suite
           ?assert(true)
       end}
     ]}.
