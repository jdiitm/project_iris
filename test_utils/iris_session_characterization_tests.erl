-module(iris_session_characterization_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT MITIGATION P2-2: Characterization Tests for iris_session
%% Pins the public API contract to enable safe future refactoring.
%% =============================================================================

%% validate_e2ee_header/1: Valid header returns ok
%% Keys are <<"ik">> and <<"ek">>, each must be >= 32 bytes binary
validate_e2ee_header_valid_test() ->
    IK = crypto:strong_rand_bytes(32),
    EK = crypto:strong_rand_bytes(32),
    Header = #{<<"ik">> => IK, <<"ek">> => EK},
    Result = iris_session:validate_e2ee_header(Header),
    ?assertEqual(ok, Result).

%% validate_e2ee_header/1: Missing fields returns error
validate_e2ee_header_missing_fields_test() ->
    Header = #{<<"ik">> => crypto:strong_rand_bytes(32)},
    Result = iris_session:validate_e2ee_header(Header),
    ?assertMatch({error, {missing_e2ee_fields, _}}, Result).

%% validate_e2ee_header/1: Non-map returns error
validate_e2ee_header_non_map_test() ->
    Result = iris_session:validate_e2ee_header(not_a_map),
    ?assertMatch({error, invalid_header_type}, Result).

%% validate_e2ee_header/1: Short keys rejected
validate_e2ee_header_short_keys_test() ->
    Header = #{<<"ik">> => <<"short">>, <<"ek">> => <<"short">>},
    Result = iris_session:validate_e2ee_header(Header),
    ?assertMatch({error, {e2ee_key_too_short, _, _}}, Result).

%% calculate_remaining/2: calculate_remaining(Depth, NextCursor) = max(0, Depth - NextCursor)
calculate_remaining_test() ->
    ?assertEqual(70, iris_session:calculate_remaining(100, 30)).

%% calculate_remaining/2: Never returns negative
calculate_remaining_floor_test() ->
    ?assertEqual(0, iris_session:calculate_remaining(10, 150)).

%% calculate_remaining/2: Zero case
calculate_remaining_zero_test() ->
    ?assertEqual(0, iris_session:calculate_remaining(50, 50)).

%% check_block_status/2: Unblocked users return ok
check_block_status_unblocked_test_() ->
    {setup,
     fun() ->
         case mnesia:system_info(is_running) of
             yes -> ok;
             _ -> mnesia:start()
         end,
         ok
     end,
     fun(_) -> ok end,
     fun(_) ->
         ?_assertEqual(ok, iris_session:check_block_status(<<"unknown_sender">>, <<"unknown_recipient">>))
     end}.

%% Module exports exist (canary tests for future refactoring)
exports_exist_test() ->
    code:ensure_loaded(iris_session),
    ?assert(erlang:function_exported(iris_session, handle_packet, 4)),
    ?assert(erlang:function_exported(iris_session, terminate, 1)),
    ?assert(erlang:function_exported(iris_session, validate_e2ee_header, 1)),
    ?assert(erlang:function_exported(iris_session, group_fanout_recipients, 3)),
    ?assert(erlang:function_exported(iris_session, estimate_remaining_messages, 3)),
    ?assert(erlang:function_exported(iris_session, calculate_remaining, 2)),
    ?assert(erlang:function_exported(iris_session, check_block_status, 2)).
