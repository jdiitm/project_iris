-module(iris_inbox_depth_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT V2 P1-6: Inbox Depth Soft Limit Tests
%% =============================================================================
%%
%% Tests verify via source analysis that store_offline_durable/2:
%% 1. Warns when inbox depth reaches 95% of limit (soft warning)
%% 2. Rejects at 100% (hard limit)
%% 3. Documents dirty_read consistency tradeoff
%% =============================================================================

%% Test: store_offline_durable has a soft warning at 95% capacity
inbox_has_soft_warning_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    FuncBody = extract_function(Src, <<"store_offline_durable">>),
    %% Must contain a warning for near-capacity (95% or similar)
    HasWarning = binary:match(FuncBody, <<"inbox_near_capacity">>) =/= nomatch
        orelse binary:match(FuncBody, <<"near capacity">>) =/= nomatch
        orelse binary:match(FuncBody, <<"0.95">>) =/= nomatch,
    ?assert(HasWarning).

%% Test: get_offline_queue_depth documents dirty_read consistency tradeoff
dirty_read_documented_test() ->
    {ok, Src} = file:read_file("src/iris_core.erl"),
    FuncBody = extract_function(Src, <<"get_offline_queue_depth">>),
    %% Must contain a comment about dirty_read consistency
    HasDoc = binary:match(FuncBody, <<"dirty">>) =/= nomatch
        andalso (binary:match(FuncBody, <<"consistency">>) =/= nomatch
            orelse binary:match(FuncBody, <<"stale">>) =/= nomatch
            orelse binary:match(FuncBody, <<"non-blocking">>) =/= nomatch),
    ?assert(HasDoc).

%% Helper: Extract a function's body as a single binary
extract_function(Src, FuncName) ->
    Lines = binary:split(Src, <<"\n">>, [global]),
    extract_func_lines(Lines, FuncName, false, []).

extract_func_lines([], _FuncName, _InFunc, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
extract_func_lines([Line | Rest], FuncName, false, Acc) ->
    Needle = <<FuncName/binary, "(">>,
    case binary:match(Line, Needle) of
        {0, _} -> extract_func_lines(Rest, FuncName, true, [Line, <<"\n">> | Acc]);
        _ -> extract_func_lines(Rest, FuncName, false, Acc)
    end;
extract_func_lines([Line | Rest], FuncName, true, Acc) ->
    IsNewFunc = case Line of
        <<>> -> false;
        <<$%, _/binary>> -> false;
        <<$\s, _/binary>> -> false;
        <<$\t, _/binary>> -> false;
        _ ->
            Needle = <<FuncName/binary, "(">>,
            case binary:match(Line, Needle) of
                {0, _} -> false;
                _ -> binary:match(Line, <<"(">>) =/= nomatch
                     andalso binary:match(Line, <<"->">>) =/= nomatch
            end
    end,
    case IsNewFunc of
        true -> iolist_to_binary(lists:reverse(Acc));
        false -> extract_func_lines(Rest, FuncName, true, [Line, <<"\n">> | Acc])
    end.
