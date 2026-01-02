-module(iris_proto).
-export([decode/1, unpack_batch/1, encode_status/3]).

-type packet() :: {login, binary()}
                | {send_message, binary(), binary()}
                | {ack, binary()}
                | {error, term()}.

%% Protocol Specification:
%% 0x01 | User (binary) -> {login, User}  (Assumes single login per connection start, ignores rest?)
%% Actually, login shouldn't have rest usually, but let's be robust.
%% 0x02 | TargetLen(16) | Target(binary) | MsgLen(16) | Msg(binary) | Rest
%% 0x03 | MsgId (binary) -> {ack, MsgId}

-spec decode(binary()) -> {packet(), binary()} | {more, binary()}.

decode(<<1, Rest/binary>>) ->
    %% Login is simpler, let's assume it consumes the rest for User? No, that's bad for robust protocol.
    %% But existing client sends <<1, User>>. No length.
    %% To keeping backward compat strictly for Login (which happens once), we can stick to "User is Rest"
    %% BUT if we want proper framing, we strictly need length for everything or delimiter.
    %% Let's impose that Login is the only packet in the first chunk, or assume User is everything.
    %% Let's impose that Login is the only packet in the first chunk, or assume User is everything.
    { {login, Rest}, <<>> };

decode(<<2, _/binary>> = Bin) when byte_size(Bin) < 3 ->
    {more, Bin};  %% Partial header

decode(<<2, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<Target:TargetLen/binary, MsgLen:16, Msg:MsgLen/binary, Rem/binary>> ->
            { {send_message, Target, Msg}, Rem };
        _ ->
            {more, <<2, TargetLen:16, Rest/binary>>}
    end;
    
decode(<<3, MsgId/binary>>) ->
    { {ack, MsgId}, <<>> };

decode(<<4, _/binary>> = Bin) when byte_size(Bin) < 7 ->
     {more, Bin}; %% Need TLen(2)+BLen(4) min

decode(<<4, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<Target:TargetLen/binary, BatchLen:32, BatchBlob:BatchLen/binary, Rem/binary>> ->
             { {batch_send, Target, BatchBlob}, Rem };
        _ ->
             {more, <<4, TargetLen:16, Rest/binary>>}
    end;

decode(<<5, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<Target:TargetLen/binary, Rem/binary>> ->
             { {get_status, Target}, Rem };
        _ ->
             {more, <<5, TargetLen:16, Rest/binary>>}
    end;

decode(<<>>) -> {more, <<>>};
decode(_Bin) -> { {error, unknown_packet}, <<>> }.

encode_status(User, State, Time) ->
    StateByte = case State of online -> 1; offline -> 0 end,
    U = User, %% User is already binary
    ULen = byte_size(U),
    <<6, ULen:16, U/binary, StateByte, Time:64>>.

unpack_batch(Blob) -> unpack_batch(Blob, []).
unpack_batch(<<>>, Acc) -> lists:reverse(Acc);
unpack_batch(<<Len:16, Msg:Len/binary, Rest/binary>>, Acc) ->
    unpack_batch(Rest, [Msg | Acc]);
unpack_batch(_, Acc) -> lists:reverse(Acc). %% Tolerant of trailing garbage
