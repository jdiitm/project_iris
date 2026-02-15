-module(iris_spk_rotation_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% iris_keys must handle check_spk_rotation and detect expired SPKs
%% =============================================================================
%%        and increments a metric when one is found.
%% =============================================================================

%% Structural test: iris_keys source must contain a dedicated handler for
%% check_spk_rotation (not just the catch-all _Info clause).
spk_rotation_handler_exists_in_source_test() ->
    {ok, Source} = file:read_file("src/iris_keys.erl"),
    ?assertNotEqual(nomatch, binary:match(Source, <<"check_spk_rotation">>)).

%% Behavioral test: after uploading a bundle with an expired SPK and sending
%% check_spk_rotation, a metric counter should be incremented.
spk_rotation_detects_expired_spk_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [fun expired_spk_increments_metric/0]}.

setup() ->
    %% Set up Mnesia for iris_keys
    Dir = "/tmp/iris_test_mnesia_spk_" ++ integer_to_list(erlang:system_time(microsecond)),
    application:set_env(mnesia, dir, Dir),
    mnesia:create_schema([node()]),
    mnesia:start(),
    %% Start iris_keys gen_server
    {ok, Pid} = iris_keys:start_link(),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid),
    mnesia:stop(),
    ok.

expired_spk_increments_metric() ->
    %% Upload a bundle with SPK timestamp 8 days ago (expired > 7 days)
    OldTs = os:system_time(second) - (8 * 86400),
    Bundle = #{
        identity_key => crypto:strong_rand_bytes(32),
        signed_prekey => crypto:strong_rand_bytes(32),
        signed_prekey_signature => crypto:strong_rand_bytes(64),
        signed_prekey_timestamp => OldTs,
        one_time_prekeys => [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 10)]
    },
    ok = iris_keys:upload_bundle(<<"spk_test_user">>, Bundle),

    %% Get metric before
    MetricsBefore = iris_keys:get_opk_metrics(),
    RotBefore = maps:get(spk_rotation_needed, MetricsBefore, 0),

    %% Send check_spk_rotation directly to the gen_server
    iris_keys ! check_spk_rotation,
    %% Give the gen_server time to process
    timer:sleep(100),

    %% Verify metric was incremented
    MetricsAfter = iris_keys:get_opk_metrics(),
    RotAfter = maps:get(spk_rotation_needed, MetricsAfter, 0),
    ?assert(RotAfter > RotBefore).
