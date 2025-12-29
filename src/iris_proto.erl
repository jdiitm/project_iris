-module(iris_proto).
-export([decode/1]).

-type packet() :: {login, binary()}
                | {send_message, binary(), binary()}
                | {ack, binary()}
                | {error, term()}.

%% Protocol Specification:
%% 0x01 | User (binary) -> {login, User}
%% 0x02 | TargetLen(16) | Target(binary) | Msg(binary) -> {send_message, Target, Msg}
%% 0x03 | MsgId (binary) -> {ack, MsgId}

-spec decode(binary()) -> packet().
decode(<<1, User/binary>>) ->
    {login, User};
decode(<<2, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<Target:TargetLen/binary, Msg/binary>> ->
            {send_message, Target, Msg};
        _ ->
            {error, invalid_send_message_packet}
    end;
decode(<<3, MsgId/binary>>) ->
    {ack, MsgId};
decode(_) ->
    {error, unknown_packet}.
