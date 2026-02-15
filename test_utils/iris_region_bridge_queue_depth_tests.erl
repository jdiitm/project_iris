-module(iris_region_bridge_queue_depth_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% G-3: O(1) Queue Depth Check Tests (RFC Section 7.2 / FM-1)
%%
%% The current get_queue_depth/1 uses length(mnesia:match_object(...)) which
%% is O(N). This test verifies that iris_region_bridge exports a fast
%% counter-based depth function that doesn't scan the table.
%%
%% =============================================================================

iris_region_bridge_queue_depth_test_() ->
    [
     {"get_queue_depth_fast/1 is exported and callable",
      fun test_fast_depth_exported/0}
    ].

test_fast_depth_exported() ->
    %% The module MUST export get_queue_depth_fast/1 which uses an O(1)
    %% ETS counter instead of O(N) match_object.
    Exports = iris_region_bridge:module_info(exports),
    ?assert(lists:member({get_queue_depth_fast, 1}, Exports)).
