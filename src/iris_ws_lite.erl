-module(iris_ws_lite).
-behaviour(gen_statem).

-export([start_link/1, set_socket/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([wait_for_socket/3, handshake/3, connected/3]).

-record(data, {
    socket :: gen_tcp:socket(),
    user :: binary() | undefined,
    buffer = <<>> :: binary(),
    frag_op = undefined :: atom()
}).

-define(WS_GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

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
    inet:setopts(Socket, [{active, once}]),
    {next_state, handshake, Data#data{socket = Socket}}.

%% STATE: handshake (HTTP Upgrade)
handshake(enter, _OldState, _Data) ->
    keep_state_and_data;

handshake(info, {tcp, Socket, Bin}, Data = #data{buffer = Buff}) ->
    NewBuff = <<Buff/binary, Bin/binary>>,
    case parse_http_upgrade(NewBuff) of
        {ok, Key, Rest} ->
            %% Perform Upgrade
            AcceptKey = base64:encode(crypto:hash(sha, <<Key/binary, ?WS_GUID>>)),
            Resp = <<"HTTP/1.1 101 Switching Protocols\r\n",
                     "Upgrade: websocket\r\n",
                     "Connection: Upgrade\r\n",
                     "Sec-WebSocket-Accept: ", AcceptKey/binary, "\r\n\r\n">>,
            gen_tcp:send(Socket, Resp),
            %% Enter Connected State and process any remaining data
            {next_state, connected, Data#data{buffer = Rest}, {next_event, internal, check_buffer}};
        more ->
            inet:setopts(Socket, [{active, once}]),
            {keep_state, Data#data{buffer = NewBuff}};
        error ->
            io:format("WS: Handshake Error~n"),
            {stop, normal, Data}
    end.

%% STATE: connected (WebSocket Frames)
connected(enter, _OldState, _Data) ->
    keep_state_and_data;

connected(internal, check_buffer, Data = #data{buffer = Buff}) ->
    process_ws_frames(Buff, Data);

connected(info, {tcp, _Socket, Bin}, Data = #data{buffer = Buff}) ->
    NewBuff = <<Buff/binary, Bin/binary>>,
    process_ws_frames(NewBuff, Data);

connected(info, {tcp_closed, _}, Data) -> {stop, normal, Data};
connected(info, {tcp_error, _, _}, Data) -> {stop, normal, Data};

%% Route msg from router
connected(info, {deliver_msg, Msg}, Data = #data{socket = Socket}) ->
    %% Wrap in Binary Frame (Opcode 2)
    Frame = encode_frame(binary, Msg),
    gen_tcp:send(Socket, Frame),
    keep_state_and_data.

%% Frame Processing Loop
%% Frame Processing Loop
process_ws_frames(Buff, Data = #data{socket = Socket, user = User}) ->
    case decode_frame(Buff) of
        {ok, Opcode, Payload, Rest} ->
            %% Handle Frame
            case handle_frame_op(Opcode, Payload, Data) of
                {ok, NewData} -> process_ws_frames(Rest, NewData);
                {packet, Packet, NewData} ->
                    %% Delegate Protocol Logic
                    {ok, NewUser, Actions} = iris_session:handle_packet(Packet, User, self(), ?MODULE),
                    handle_actions(Actions, Socket),
                    process_ws_frames(Rest, NewData#data{user = NewUser});
                close -> {stop, normal, Data}
            end;
        more ->
            inet:setopts(Socket, [{active, once}]),
            {keep_state, Data#data{buffer = Buff}}
    end.

handle_actions([], _) -> ok;
handle_actions([{send, Bin} | T], Socket) ->
    gen_tcp:send(Socket, encode_frame(binary, Bin)),
    handle_actions(T, Socket);
handle_actions([{send_batch, Bins} | T], Socket) ->
    [gen_tcp:send(Socket, encode_frame(binary, B)) || B <- Bins],
    handle_actions(T, Socket);
handle_actions([close | _], Socket) ->
    gen_tcp:send(Socket, encode_frame(close, <<>>)).

%% WS Logic
handle_frame_op(ping, _, Data) -> 
    gen_tcp:send(Data#data.socket, encode_frame(pong, <<>>)),
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
