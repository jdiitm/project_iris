-module(iris_json_formatter).

%% =============================================================================
%% JSON Structured Logger Formatter
%% =============================================================================
%% Implements the OTP logger formatter behaviour for JSON output.
%% Compatible with ELK, Loki, CloudWatch, and Datadog log ingestion.
%%
%% Usage in config:
%%   {kernel, [
%%       {logger, [
%%           {handler, default, logger_std_h, #{
%%               formatter => {iris_json_formatter, #{}}
%%           }}
%%       ]}
%%   ]}
%% =============================================================================

-export([format/2]).
-export([check_config/1]).

-define(MAX_MSG_SIZE, 32768).  %% AUDIT M9: 32KB max message size

%% @doc Format a log event as a single-line JSON string.
-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    Timestamp = format_timestamp(Meta),
    MsgStr = format_msg(Msg),
    Node = atom_to_list(node()),
    Pid = format_pid(maps:get(pid, Meta, undefined)),
    Module = atom_to_list(maps:get(mf, Meta, maps:get(module, Meta, unknown))),
    
    %% Build JSON manually (no dependency on jsx/jiffy)
    Json = [
        <<"{">>,
        <<"\"ts\":\"">>, Timestamp, <<"\",">>,
        <<"\"level\":\"">>, atom_to_list(Level), <<"\",">>,
        <<"\"node\":\"">>, Node, <<"\",">>,
        <<"\"pid\":\"">>, Pid, <<"\",">>,
        <<"\"module\":\"">>, Module, <<"\",">>,
        <<"\"msg\":\"">>, json_escape(MsgStr), <<"\"">>,
        <<"}\n">>
    ],
    Json.

%% @doc Validate formatter config.
check_config(_Config) -> ok.

%% =============================================================================
%% Internal
%% =============================================================================

format_timestamp(#{time := Time}) ->
    calendar:system_time_to_rfc3339(Time, [{unit, microsecond}]);
format_timestamp(_) ->
    calendar:system_time_to_rfc3339(
        erlang:system_time(microsecond), [{unit, microsecond}]).

format_msg({string, Msg}) ->
    truncate_msg(unicode:characters_to_list(Msg));
format_msg({report, Report}) when is_map(Report) ->
    truncate_msg(io_lib:format("~0p", [Report]));
format_msg({report, Report}) ->
    truncate_msg(io_lib:format("~0p", [Report]));
format_msg({Format, Args}) ->
    truncate_msg(io_lib:format(Format, Args)).

%% AUDIT M9: Truncate oversized messages to prevent memory bloat
truncate_msg(Msg) ->
    Bin = iolist_to_binary([Msg]),
    case byte_size(Bin) > ?MAX_MSG_SIZE of
        true ->
            Truncated = binary:part(Bin, 0, ?MAX_MSG_SIZE),
            binary_to_list(<<Truncated/binary, "...[truncated]">>);
        false ->
            binary_to_list(Bin)
    end.

format_pid(undefined) -> "unknown";
format_pid(Pid) -> pid_to_list(Pid).

%% Escape special JSON characters
json_escape(Str) ->
    List = unicode:characters_to_list(iolist_to_binary([Str])),
    lists:flatmap(fun json_escape_char/1, List).

json_escape_char($") -> "\\\"";
json_escape_char($\\) -> "\\\\";
json_escape_char($\n) -> "\\n";
json_escape_char($\r) -> "\\r";
json_escape_char($\t) -> "\\t";
json_escape_char(C) when C < 32 ->
    io_lib:format("\\u~4.16.0b", [C]);
json_escape_char(C) -> [C].
