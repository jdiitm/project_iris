-module(iris_rate_limit_granularity_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Rate Limiting Granularity
%% Verifies that per-message-type rate limiting prevents cheap packet floods
%% (typing, handshake, presence) from starving real message delivery.
%% =============================================================================

setup() ->
    %% Start rate limiter with default config
    case whereis(iris_rate_limiter) of
        undefined ->
            {ok, Pid} = iris_rate_limiter:start_link([]),
            Pid;
        Pid ->
            Pid
    end.

cleanup(_Pid) ->
    case whereis(iris_rate_limiter) of
        undefined -> ok;
        P ->
            gen_server:stop(P),
            ok
    end.

typing_has_separate_limit_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         User = <<"rate_test_typing_user">>,
         %% Exhaust typing limit (default burst=50)
         lists:foreach(fun(_) ->
             iris_rate_limiter:check_typed(User, typing)
         end, lists:seq(1, 60)),
         %% Typing should now be denied
         ?_assertMatch({deny, _}, iris_rate_limiter:check_typed(User, typing))
     end}.

message_still_allowed_after_typing_exhaust_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         User = <<"rate_test_msg_after_typing">>,
         %% Exhaust typing limit
         lists:foreach(fun(_) ->
             iris_rate_limiter:check_typed(User, typing)
         end, lists:seq(1, 60)),
         %% Messages should still be allowed (separate bucket)
         ?_assertEqual(allow, iris_rate_limiter:check_typed(User, message))
     end}.

message_limit_independent_from_typing_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         User = <<"rate_test_independent">>,
         %% Exhaust message limit (default burst=20)
         lists:foreach(fun(_) ->
             iris_rate_limiter:check_typed(User, message)
         end, lists:seq(1, 25)),
         %% Typing should still be allowed
         ?_assertEqual(allow, iris_rate_limiter:check_typed(User, typing))
     end}.

check_typed_exported_test() ->
    code:ensure_loaded(iris_rate_limiter),
    ?assert(erlang:function_exported(iris_rate_limiter, check_typed, 2)).
