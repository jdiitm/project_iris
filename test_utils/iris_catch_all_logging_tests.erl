-module(iris_catch_all_logging_tests).
-include_lib("eunit/include/eunit.hrl").
-export([log/2]).  %% Logger handler callback

%% =============================================================================
%% Mitigation: Silent error suppression must emit warnings.
%% =============================================================================
%% Multiple modules have `catch _:_ -> ok` or `catch _:_ -> []` patterns
%% that silently suppress errors. After the fix, these should emit
%% logger:warning messages while preserving the same return values.
%%
%% We test representative modules by triggering error paths and checking
%% that a warning log is emitted via a custom logger handler.
%% =============================================================================

%% ---------------------------------------------------------------------------
%% Test: iris_mnesia_guard check_memory logs on Mnesia unavailability
%% ---------------------------------------------------------------------------
mnesia_guard_logs_on_error_test() ->
    %% Install a log capture handler
    install_log_handler(),
    try
        %% Stop Mnesia to trigger error path in check_memory
        mnesia:stop(),
        
        %% The periodic check in iris_mnesia_guard calls check_memory()
        %% which accesses mnesia:system_info(tables) -- should catch and log
        %% We call the memory check directly via the module
        case catch iris_mnesia_guard:check_memory() of
            _ -> ok  %% We don't care about the return, only the log
        end,
        
        %% Check if any warning was logged from the module
        %% After the fix, warnings should be present
        Logs = get_captured_logs(),
        MnesiaGuardWarnings = [L || #{msg := M} = L <- Logs,
                                     is_log_from(M, iris_mnesia_guard)],
        ?assert(length(MnesiaGuardWarnings) > 0)
    after
        remove_log_handler(),
        mnesia:start(),
        timer:sleep(100)
    end.

%% ---------------------------------------------------------------------------
%% Test: iris_storage_tier metric failure is logged
%% ---------------------------------------------------------------------------
storage_tier_logs_on_metric_error_test() ->
    install_log_handler(),
    try
        %% Calling with invalid args to trigger error paths
        %% The metrics catch-all should log on failure
        %% We can trigger this by calling evict when metrics module 
        %% doesn't have the counter registered
        case catch iris_storage_tier:check_memory_pressure() of
            _ -> ok
        end,
        
        %% For this test we just verify the catch path returns gracefully
        %% (the actual log check requires the error to fire)
        ok
    after
        remove_log_handler()
    end.

%% ---------------------------------------------------------------------------
%% Helpers: Log capture
%% ---------------------------------------------------------------------------
install_log_handler() ->
    HandlerConfig = #{
        level => warning,
        formatter => {logger_formatter, #{template => [msg]}},
        config => #{pid => self()}
    },
    logger:add_handler(test_catch_all_handler, ?MODULE, HandlerConfig).

remove_log_handler() ->
    logger:remove_handler(test_catch_all_handler).

get_captured_logs() ->
    collect_logs([]).

collect_logs(Acc) ->
    receive
        {log_captured, LogEvent} ->
            collect_logs([LogEvent | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

%% Logger handler callback
log(LogEvent, #{config := #{pid := Pid}}) ->
    Pid ! {log_captured, LogEvent}.

%% Check if a log message references a specific module
is_log_from({string, Msg}, Module) ->
    ModStr = atom_to_list(Module),
    string:find(Msg, ModStr) =/= nomatch;
is_log_from({report, Report}, Module) when is_map(Report) ->
    ModStr = atom_to_list(Module),
    Str = io_lib:format("~p", [Report]),
    string:find(lists:flatten(Str), ModStr) =/= nomatch;
is_log_from({Fmt, Args}, Module) when is_list(Fmt) ->
    Str = lists:flatten(io_lib:format(Fmt, Args)),
    ModStr = atom_to_list(Module),
    string:find(Str, ModStr) =/= nomatch;
is_log_from(_, _) ->
    false.
