-module(iris_json_formatter_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% AUDIT: JSON Formatter Message Size Cap Tests
%% =============================================================================
%%
%% Tests verify that iris_json_formatter:format/2 truncates oversized log
%% messages to prevent memory bloat from a single large log event.
%% =============================================================================

iris_json_formatter_test_() ->
    [
     {"AUDIT M9: oversized message is truncated",
      fun test_oversized_message_truncated/0},
     {"AUDIT M9: normal message is preserved",
      fun test_normal_message_preserved/0},
     {"AUDIT M9: source has MAX_MSG_SIZE",
      fun test_source_has_max_msg_size/0}
    ].

test_oversized_message_truncated() ->
    %% 1MB log message -- output must be capped
    BigMsg = binary:copy(<<"x">>, 1024 * 1024),
    Event = #{level => info,
              msg => {string, BigMsg},
              meta => #{time => erlang:system_time(microsecond)}},
    Output = iolist_to_binary(iris_json_formatter:format(Event, #{})),
    %% Output must be significantly smaller than 1MB
    %% (32KB cap + JSON overhead should be < 40KB)
    ?assert(byte_size(Output) < 40000),
    %% Must contain truncation marker
    ?assert(binary:match(Output, <<"[truncated]">>) =/= nomatch).

test_normal_message_preserved() ->
    %% Small message -- should be preserved intact
    Event = #{level => warning,
              msg => {string, <<"hello world">>},
              meta => #{time => erlang:system_time(microsecond)}},
    Output = iolist_to_binary(iris_json_formatter:format(Event, #{})),
    ?assert(binary:match(Output, <<"hello world">>) =/= nomatch),
    %% No truncation marker
    ?assertEqual(nomatch, binary:match(Output, <<"[truncated]">>)).

test_source_has_max_msg_size() ->
    {ok, Src} = file:read_file("src/iris_json_formatter.erl"),
    ?assert(binary:match(Src, <<"MAX_MSG_SIZE">>) =/= nomatch).
