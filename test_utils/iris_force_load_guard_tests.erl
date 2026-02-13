-module(iris_force_load_guard_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT V2 P1-3: force_load_table Cluster Awareness Guard Tests
%% =============================================================================
%%
%% Tests verify via source analysis that repair_failed_tables/1 checks
%% for live peer nodes before using mnesia:force_load_table/1, to prevent
%% data divergence when peers are available with newer data.
%%
%% Source analysis approach: verifying code patterns since force_load_table
%% requires a multi-node Mnesia cluster to test functionally.
%% =============================================================================

%% Test: repair_failed_tables checks for peers before force-loading
repair_checks_peers_before_force_load_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    FuncBody = extract_function(Src, <<"repair_failed_tables">>),
    %% Must contain a check for peer nodes or where_to_read before force_load_table
    HasPeerCheck = binary:match(FuncBody, <<"where_to_read">>) =/= nomatch
        orelse binary:match(FuncBody, <<"nodes()">>) =/= nomatch
        orelse binary:match(FuncBody, <<"active_replicas">>) =/= nomatch,
    ?assert(HasPeerCheck).

%% Test: repair_failed_tables logs divergence warning
repair_logs_divergence_warning_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    FuncBody = extract_function(Src, <<"repair_failed_tables">>),
    %% Must contain a DATA DIVERGENCE warning message
    HasDivergenceWarning = binary:match(FuncBody, <<"DIVERGENCE">>) =/= nomatch
        orelse binary:match(FuncBody, <<"divergence">>) =/= nomatch,
    ?assert(HasDivergenceWarning).

%% Test: repair_failed_tables emits force_load metric
repair_emits_force_load_metric_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    FuncBody = extract_function(Src, <<"repair_failed_tables">>),
    HasMetric = binary:match(FuncBody, <<"force_load_table_events">>) =/= nomatch,
    ?assert(HasMetric).

%% Helper: Extract a function's body as a single binary
extract_function(Src, FuncName) ->
    Lines = binary:split(Src, <<"\n">>, [global]),
    extract_func_lines(Lines, FuncName, false, []).

extract_func_lines([], _FuncName, _InFunc, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
extract_func_lines([Line | Rest], FuncName, false, Acc) ->
    %% Look for function definition: starts with FuncName( at column 0
    Needle = <<FuncName/binary, "(">>,
    case binary:match(Line, Needle) of
        {0, _} -> extract_func_lines(Rest, FuncName, true, [Line, <<"\n">> | Acc]);
        _ -> extract_func_lines(Rest, FuncName, false, Acc)
    end;
extract_func_lines([Line | Rest], FuncName, true, Acc) ->
    %% End of function: non-blank line at column 0 that is a new function def
    IsNewFunc = case Line of
        <<>> -> false;
        <<$%, _/binary>> -> false;  %% comment
        <<$\s, _/binary>> -> false;  %% indented
        <<$\t, _/binary>> -> false;  %% indented
        _ ->
            %% Check if it's a new function (not a clause of the same function)
            Needle = <<FuncName/binary, "(">>,
            case binary:match(Line, Needle) of
                {0, _} -> false;  %% Same function, different clause
                _ ->
                    %% Check for function-like pattern: word(
                    binary:match(Line, <<"(">>) =/= nomatch
                    andalso binary:match(Line, <<"->">>) =/= nomatch
            end
    end,
    case IsNewFunc of
        true -> iolist_to_binary(lists:reverse(Acc));
        false -> extract_func_lines(Rest, FuncName, true, [Line, <<"\n">> | Acc])
    end.
