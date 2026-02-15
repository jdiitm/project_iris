-module(iris_ws_frame_limit_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% WebSocket frame size limit
%% =============================================================================
%% RFC Section 8: Payload Size = 64 KB max.
%% decode_frame/1 MUST reject frames claiming length > MAX_FRAME_SIZE.
%% parse_http_upgrade/1 MUST reject headers > MAX_HTTP_HEADER_SIZE.
%% =============================================================================

%% Helper: Build a masked WebSocket binary frame with given payload length.
%% Format: FIN=1, RSV=0, Op=2 (binary), MASK=1, then length encoding + mask key.
build_ws_frame(PayloadLen) ->
    MaskKey = <<16#DEADBEEF:32>>,
    Payload = binary:copy(<<0>>, min(PayloadLen, 256)),  %% Truncated for test
    MaskedPayload = Payload,  %% Don't actually mask for decode_frame test
    if
        PayloadLen < 126 ->
            <<1:1, 0:3, 2:4, 1:1, PayloadLen:7, MaskKey/binary, MaskedPayload/binary>>;
        PayloadLen < 65536 ->
            <<1:1, 0:3, 2:4, 1:1, 126:7, PayloadLen:16, MaskKey/binary, MaskedPayload/binary>>;
        true ->
            <<1:1, 0:3, 2:4, 1:1, 127:7, PayloadLen:64, MaskKey/binary, MaskedPayload/binary>>
    end.

%% ---------------------------------------------------------------------------
%% Test: Frame within limit is accepted
%% ---------------------------------------------------------------------------
frame_within_limit_accepted_test() ->
    %% A 100-byte frame should be accepted (returns 'more' since payload is truncated)
    Frame = build_ws_frame(100),
    Result = iris_ws_lite:decode_frame(Frame),
    %% Should either decode successfully or return 'more' (waiting for full payload)
    ?assert(Result =:= more orelse element(1, Result) =:= ok).

%% ---------------------------------------------------------------------------
%% Test: Frame at exactly 64KB is accepted
%% ---------------------------------------------------------------------------
frame_at_limit_accepted_test() ->
    Frame = build_ws_frame(65536),
    Result = iris_ws_lite:decode_frame(Frame),
    %% At limit: should be accepted (returns 'more' since payload truncated)
    ?assert(Result =:= more orelse element(1, Result) =:= ok).

%% ---------------------------------------------------------------------------
%% Test: Frame exceeding 64KB MUST be rejected
%% ---------------------------------------------------------------------------
frame_exceeding_limit_rejected_test() ->
    %% 128KB frame -- must be rejected
    Frame = build_ws_frame(131072),
    Result = iris_ws_lite:decode_frame(Frame),
    ?assertMatch({error, frame_too_large}, Result).

%% ---------------------------------------------------------------------------
%% Test: Frame claiming 1TB MUST be rejected (DoS vector)
%% ---------------------------------------------------------------------------
frame_gigantic_rejected_test() ->
    %% 1TB frame header -- the exact OOM DoS vector
    Frame = build_ws_frame(1099511627776),
    Result = iris_ws_lite:decode_frame(Frame),
    ?assertMatch({error, frame_too_large}, Result).

%% ---------------------------------------------------------------------------
%% Test: HTTP header exceeding 8KB MUST be rejected
%% ---------------------------------------------------------------------------
http_header_too_large_rejected_test() ->
    %% Build a header > 8KB without \r\n\r\n terminator
    LargeHeader = binary:copy(<<"X">>, 9000),
    Result = iris_ws_lite:parse_http_upgrade(LargeHeader),
    ?assertEqual(error, Result).

%% ---------------------------------------------------------------------------
%% Test: Normal HTTP upgrade header is accepted
%% ---------------------------------------------------------------------------
http_header_normal_accepted_test() ->
    Header = <<"GET / HTTP/1.1\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n">>,
    Result = iris_ws_lite:parse_http_upgrade(Header),
    ?assertMatch({ok, _, _}, Result).
