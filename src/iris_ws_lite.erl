-module(iris_ws_lite).
-behaviour(gen_statem).

-export([start_link/1, set_socket/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([wait_for_socket/3, handshake/3, connected/3]).
-export([decode_frame/1, parse_http_upgrade/1]).  %% Exported for TDD (B-4 audit)

-record(data, {
    socket :: gen_tcp:socket() | ssl:sslsocket(),
    user :: binary() | undefined,
    buffer = <<>> :: binary(),
    frag_op = undefined :: atom()
}).

-define(WS_GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

%% B-4 AUDIT MITIGATION: Bound frame and header sizes to prevent OOM DoS
-define(MAX_FRAME_SIZE, 65536).          %% 64KB - RFC Section 8 payload limit
-define(MAX_HTTP_HEADER_SIZE, 8192).     %% 8KB - standard HTTP header limit
-define(MAX_FRAME_DEPTH, 100).           %% Max frames processed per read event

%% =============================================================================
%% Transport-agnostic socket helpers (TLS + plain TCP)
%% NFR-14: TLS is mandatory, but we handle both for robustness.
%% =============================================================================

sock_setopts({sslsocket, _, _} = Sock, Opts) -> ssl:setopts(Sock, Opts);
sock_setopts(Sock, Opts) -> inet:setopts(Sock, Opts).

sock_send({sslsocket, _, _} = Sock, Data) -> ssl:send(Sock, Data);
sock_send(Sock, Data) -> gen_tcp:send(Sock, Data).

%% API
start_link(Socket) ->
    gen_statem:start_link(?MODULE, Socket, []).

set_socket(Pid, Socket) ->
    gen_statem:cast(Pid, {socket_ready, Socket}).

%% Callbacks
init(_Args) ->
    {ok, wait_for_socket, #data{}}.

callback_mode() -> [state_functions, state_enter].

%% STATE: wait_for_socket
wait_for_socket(enter, _OldState, _Data) ->
    keep_state_and_data;
wait_for_socket(cast, {socket_ready, Socket}, Data) ->
    sock_setopts(Socket, [{active, once}]),
    {next_state, handshake, Data#data{socket = Socket}};
%% Socket closed/error before handshake even began
wait_for_socket(info, {Closed, _}, Data)
  when Closed =:= tcp_closed; Closed =:= ssl_closed ->
    {stop, normal, Data};
wait_for_socket(info, {Error, _, _}, Data)
  when Error =:= tcp_error; Error =:= ssl_error ->
    {stop, normal, Data}.

%% STATE: handshake (HTTP Upgrade)
handshake(enter, _OldState, _Data) ->
    keep_state_and_data;

%% Handle both TLS ({ssl, ...}) and plain TCP ({tcp, ...}) data messages
handshake(info, {Proto, Socket, Bin}, Data = #data{buffer = Buff})
  when Proto =:= tcp; Proto =:= ssl ->
    NewBuff = <<Buff/binary, Bin/binary>>,
    case parse_http_upgrade(NewBuff) of
        {ok, Key, Rest} ->
            %% Perform Upgrade
            AcceptKey = base64:encode(crypto:hash(sha, <<Key/binary, ?WS_GUID>>)),
            Resp = <<"HTTP/1.1 101 Switching Protocols\r\n",
                     "Upgrade: websocket\r\n",
                     "Connection: Upgrade\r\n",
                     "Sec-WebSocket-Accept: ", AcceptKey/binary, "\r\n\r\n">>,
            sock_send(Socket, Resp),
            %% Enter Connected State and process any remaining data
            {next_state, connected, Data#data{buffer = Rest}, {next_event, internal, check_buffer}};
        more ->
            sock_setopts(Socket, [{active, once}]),
            {keep_state, Data#data{buffer = NewBuff}};
        error ->
            io:format("WS: Handshake Error~n"),
            {stop, normal, Data}
    end;

%% Handle socket close/error during handshake (prevents function_clause crash)
handshake(info, {Closed, _}, Data)
  when Closed =:= tcp_closed; Closed =:= ssl_closed ->
    {stop, normal, Data};
handshake(info, {Error, _, _}, Data)
  when Error =:= tcp_error; Error =:= ssl_error ->
    {stop, normal, Data}.

%% STATE: connected (WebSocket Frames)
connected(enter, _OldState, _Data) ->
    keep_state_and_data;

connected(internal, check_buffer, Data = #data{buffer = Buff}) ->
    process_ws_frames(Buff, Data);

%% Handle both TLS and plain TCP data messages
connected(info, {Proto, _Socket, Bin}, Data = #data{buffer = Buff})
  when Proto =:= tcp; Proto =:= ssl ->
    NewBuff = <<Buff/binary, Bin/binary>>,
    process_ws_frames(NewBuff, Data);

%% Handle both TLS and plain TCP close/error
connected(info, {Closed, _}, Data)
  when Closed =:= tcp_closed; Closed =:= ssl_closed ->
    {stop, normal, Data};
connected(info, {Error, _, _}, Data)
  when Error =:= tcp_error; Error =:= ssl_error ->
    {stop, normal, Data};

%% Route msg from router
connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket}) ->
    %% Wrap in Binary Frame (Opcode 2)
    Frame = encode_frame(binary, Msg),
    sock_send(Socket, Frame),
    keep_state_and_data;

%% AUDIT 2.3a FIX: Handle session_overload from heap check
connected(info, session_overload, Data = #data{socket = Socket}) ->
    logger:warning("Session ~p: heap limit approaching, sending SERVER_OVERLOAD",
                   [Data#data.user]),
    sock_send(Socket, encode_frame(binary, <<"SERVER_OVERLOAD">>)),
    {stop, {shutdown, heap_limit}, Data};

connected(info, _Other, _Data) ->
    keep_state_and_data.

%% Frame Processing Loop (B-4/H-9 AUDIT MITIGATION: depth-limited)
process_ws_frames(Buff, Data) ->
    process_ws_frames(Buff, Data, 0).

process_ws_frames(_Buff, Data, Depth) when Depth > ?MAX_FRAME_DEPTH ->
    logger:warning("WS: Max frame depth ~p exceeded, closing connection", [?MAX_FRAME_DEPTH]),
    {stop, {shutdown, frame_depth_exceeded}, Data};
process_ws_frames(Buff, Data = #data{socket = Socket, user = User}, Depth) ->
    case decode_frame(Buff) of
        {ok, Opcode, Payload, Rest} ->
            %% Handle Frame
            case handle_frame_op(Opcode, Payload, Data) of
                {ok, NewData} -> process_ws_frames(Rest, NewData, Depth + 1);
                {packet, Packet, NewData} ->
                    %% Delegate Protocol Logic
                    {ok, NewUser, Actions} = iris_session:handle_packet(Packet, User, self(), ?MODULE),
                    handle_actions(Actions, Socket),
                    %% AUDIT 2.3a FIX: Check heap_size after packet processing.
                    case check_heap_size() of
                        ok ->
                            process_ws_frames(Rest, NewData#data{user = NewUser}, Depth + 1);
                        overload ->
                            logger:warning("Session ~p: heap_size exceeded soft limit, closing gracefully",
                                           [NewUser]),
                            sock_send(Socket, encode_frame(binary, <<"SERVER_OVERLOAD">>)),
                            {stop, {shutdown, heap_limit}, NewData#data{user = NewUser}}
                    end;
                close -> {stop, normal, Data}
            end;
        {error, Reason} ->
            %% B-4 AUDIT MITIGATION: Reject oversized or malformed frames
            logger:warning("WS frame rejected: ~p", [Reason]),
            {stop, {shutdown, Reason}, Data};
        more ->
            sock_setopts(Socket, [{active, once}]),
            {keep_state, Data#data{buffer = Buff}}
    end.

handle_actions([], _) -> ok;
handle_actions([{send, Bin} | T], Socket) ->
    sock_send(Socket, encode_frame(binary, Bin)),
    handle_actions(T, Socket);
handle_actions([{send_batch, Bins} | T], Socket) ->
    [sock_send(Socket, encode_frame(binary, B)) || B <- Bins],
    handle_actions(T, Socket);
handle_actions([close | _], Socket) ->
    sock_send(Socket, encode_frame(close, <<>>)).

%% AUDIT 2.3a FIX: Soft heap limit check (80% of 1M words = 800K words).
%% Called after each packet to detect approaching the max_heap_size limit
%% and send SERVER_OVERLOAD before the hard limit triggers.
-define(HEAP_SOFT_LIMIT, 800000).  %% 80% of 1,000,000 words (~6.4MB)

check_heap_size() ->
    case erlang:process_info(self(), heap_size) of
        {heap_size, Size} when Size > ?HEAP_SOFT_LIMIT -> overload;
        _ -> ok
    end.

%% WS Logic
handle_frame_op(ping, _, Data) -> 
    sock_send(Data#data.socket, encode_frame(pong, <<>>)),
    {ok, Data};
handle_frame_op(pong, _, Data) -> {ok, Data};
handle_frame_op(close, _, _Data) -> close;
handle_frame_op(binary, Payload, Data) ->
    %% Payload is iris_proto packet
    case iris_proto:decode(Payload) of
        {Packet, <<>>} -> {packet, Packet, Data};
        _ -> {ok, Data} %% Ignore if partial/malformed inside a frame (shouldn't happen with 1:1 mapping)
    end.

%% RFC 6455 Decoding (Simplified: No Fragmentation support for now)
decode_frame(<<>>) -> more;
decode_frame(<<_Fin:1, _Rsv:3, Op:4, _Mask:1, Len:7, Rest/binary>>) ->
    parse_len(Op, Len, Rest);
decode_frame(_) -> more.

parse_len(_, _, <<>>) -> more;
parse_len(Op, 126, <<ExtLen:16, Rest/binary>>) -> parse_mask(Op, ExtLen, Rest);
parse_len(Op, 127, <<ExtLen:64, Rest/binary>>) -> parse_mask(Op, ExtLen, Rest);
parse_len(_, 126, _) -> more;
parse_len(_, 127, _) -> more;
parse_len(Op, Len, Rest) -> parse_mask(Op, Len, Rest).

%% B-4 AUDIT MITIGATION: Reject frames exceeding MAX_FRAME_SIZE before allocation
parse_mask(_Op, Len, _Rest) when Len > ?MAX_FRAME_SIZE ->
    {error, frame_too_large};
parse_mask(Op, Len, <<MaskKey:32, Rest/binary>>) ->
    case Rest of
        <<MaskedPayload:Len/binary, Rem/binary>> ->
             Payload = unmask(MaskedPayload, MaskKey, <<>>),
             OpAtom = opcode(Op),
             {ok, OpAtom, Payload, Rem};
        _ -> more
    end;
parse_mask(_, _, _) -> more.

unmask(<<>>, _, Acc) -> Acc;
unmask(<<B:32, Rest/binary>>, Key, Acc) -> 
    unmask(Rest, Key, <<Acc/binary, (B bxor Key):32>>);
unmask(<<B:24>>, Key, Acc) -> 
    <<K:24, _/bits>> = <<Key:32>>,
    <<Acc/binary, (B bxor K):24>>;
unmask(<<B:16>>, Key, Acc) -> 
    <<K:16, _/bits>> = <<Key:32>>,
    <<Acc/binary, (B bxor K):16>>;
unmask(<<B:8>>, Key, Acc) -> 
    <<K:8, _/bits>> = <<Key:32>>,
    <<Acc/binary, (B bxor K):8>>.

encode_frame(OpAtom, Payload) ->
    Op = case OpAtom of text -> 1; binary -> 2; close -> 8; ping -> 9; pong -> 10 end,
    Len = byte_size(Payload),
    Header = if 
        Len < 126 -> <<1:1, 0:3, Op:4, 0:1, Len:7>>;
        Len < 65536 -> <<1:1, 0:3, Op:4, 0:1, 126:7, Len:16>>;
        true -> <<1:1, 0:3, Op:4, 0:1, 127:7, Len:64>>
    end,
    <<Header/binary, Payload/binary>>.

opcode(0) -> cont;
opcode(1) -> text;
opcode(2) -> binary;
opcode(8) -> close;
opcode(9) -> ping;
opcode(10) -> pong;
opcode(_) -> unknown.

%% B-4 AUDIT MITIGATION: Reject oversized HTTP headers
parse_http_upgrade(Bin) when byte_size(Bin) > ?MAX_HTTP_HEADER_SIZE ->
    error;
parse_http_upgrade(Bin) ->
    case binary:match(Bin, <<"\r\n\r\n">>) of
        {Pos, _} ->
            Head = binary:part(Bin, 0, Pos),
            Rest = binary:part(Bin, Pos+4, byte_size(Bin)-(Pos+4)),
            [_Req | Lines] = binary:split(Head, <<"\r\n">>, [global]),
            case find_header(<<"sec-websocket-key">>, Lines) of
                undefined -> error;
                Key -> {ok, Key, Rest}
            end;
        nomatch -> more
    end.

find_header(_, []) -> undefined;
find_header(TargetKey, [Line|T]) ->
    case binary:split(Line, <<":">>) of
        [K, V] ->
            KLower = string:lowercase(string:trim(K)),
            if KLower == TargetKey -> string:trim(V);
               true -> find_header(TargetKey, T)
            end;
        _ -> find_header(TargetKey, T)
    end.

terminate(_, _, #data{user = User}) ->
    iris_session:terminate(User),
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.
