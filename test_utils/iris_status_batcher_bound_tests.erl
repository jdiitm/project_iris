-module(iris_status_batcher_bound_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% H-5 Mitigation: Status batcher buffer must be bounded.
%% =============================================================================

buffer_drops_when_oversized_test() ->
    {ok, Pid} = iris_status_batcher:start_link(97),
    unlink(Pid),
    
    %% Suspend the process to prevent the flush timer from firing
    sys:suspend(Pid),
    
    %% Inject an artificially large buffer via sys:replace_state
    LargeBuf = maps:from_list(
        [{list_to_binary("inject_" ++ integer_to_list(N)), N}
         || N <- lists:seq(1, 10001)]
    ),
    sys:replace_state(Pid, fun(State) ->
        %% #state{id, buffer, count, timer_ref}
        %% Cancel the timer to prevent flush during test
        TRef = element(5, State),
        erlang:cancel_timer(TRef),
        S1 = setelement(3, State, LargeBuf),
        setelement(4, S1, 10001)
    end),
    
    %% Resume to process the next cast
    sys:resume(Pid),
    
    %% Now send one more item -- should be dropped after fix
    gen_server:cast(Pid, {update, <<"overflow_user">>, offline}),
    timer:sleep(50),
    
    %% After the fix: count should not increase past MAX_BUFFER_SIZE
    State = sys:get_state(Pid),
    Count = element(4, State),
    ?assert(Count =< 10001),
    
    exit(Pid, kill),
    timer:sleep(10).

buffer_accepts_normal_load_test() ->
    {ok, Pid} = iris_status_batcher:start_link(96),
    unlink(Pid),
    
    %% Suspend to prevent timer-based flush during test
    sys:suspend(Pid),
    sys:replace_state(Pid, fun(State) ->
        TRef = element(5, State),
        erlang:cancel_timer(TRef),
        State
    end),
    sys:resume(Pid),
    
    %% Normal load: 100 items (well under BATCH_SIZE and MAX_BUFFER_SIZE)
    lists:foreach(fun(N) ->
        User = list_to_binary("user_" ++ integer_to_list(N)),
        gen_server:cast(Pid, {update, User, offline})
    end, lists:seq(1, 100)),
    
    timer:sleep(50),
    
    State = sys:get_state(Pid),
    Count = element(4, State),
    ?assertEqual(100, Count),
    
    exit(Pid, kill),
    timer:sleep(10).
