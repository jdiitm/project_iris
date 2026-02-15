-module(iris_app_registration_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% OTP Application Module Registration Tests
%% =============================================================================
%%
%% Tests verify that all required modules are listed in iris_core.app.src
%% so that they are included in OTP releases. Specifically:
%% - iris_rpc is registered (previously missing)
%% - iris_cluster_join_worker is registered (previously missing)
%% - iris_discovery is registered (previously missing)
%% - All modules in src/ are registered
%% =============================================================================

%% =============================================================================
%% Source file (.app.src) analysis
%% =============================================================================

app_src_registration_test_() ->
    [
     {"iris_rpc listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_rpc">>) =/= nomatch)
      end},

     {"iris_cluster_join_worker listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_cluster_join_worker">>) =/= nomatch)
      end},

     {"iris_discovery listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_discovery">>) =/= nomatch)
      end},

     {"iris_group listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_group">>) =/= nomatch)
      end},

     {"iris_keys listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_keys">>) =/= nomatch)
      end},

     {"iris_store listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_store">>) =/= nomatch)
      end},

     {"iris_user_safety listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_user_safety">>) =/= nomatch)
      end},

     {"iris_metrics listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_metrics">>) =/= nomatch)
      end},

     {"iris_core_registry listed in iris_core.app.src", fun() ->
          {ok, Src} = file:read_file("src/iris_core.app.src"),
          ?assert(binary:match(Src, <<"iris_core_registry">>) =/= nomatch)
      end}
    ].

%% =============================================================================
%% Module loadability tests
%% =============================================================================

module_loadable_test_() ->
    [
     {"iris_rpc module is loadable", fun() ->
          {module, iris_rpc} = code:ensure_loaded(iris_rpc),
          Exports = iris_rpc:module_info(exports),
          ?assert(lists:member({call, 4}, Exports)),
          ?assert(lists:member({call, 5}, Exports)),
          ?assert(lists:member({cast, 4}, Exports))
      end},

     {"iris_cluster_join_worker module is loadable", fun() ->
          {module, iris_cluster_join_worker} = code:ensure_loaded(iris_cluster_join_worker),
          Exports = iris_cluster_join_worker:module_info(exports),
          ?assert(lists:member({start_link, 1}, Exports))
      end},

     {"iris_discovery module is loadable", fun() ->
          {module, iris_discovery} = code:ensure_loaded(iris_discovery),
          Exports = iris_discovery:module_info(exports),
          ?assert(length(Exports) > 0)
      end}
    ].
