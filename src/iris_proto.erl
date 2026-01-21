-module(iris_proto).
-export([decode/1, unpack_batch/1, encode_status/3, encode_reliable_msg/2]).
-export([encode_seq_msg/3]).  %% AUDIT FIX: Sequence-numbered message encoder (RFC FR-5)
-export([generate_msg_id/0]). %% Unique message ID generator

-type packet() :: {login, binary()}
                | {send_message, binary(), binary()}
                | {ack, binary()}
                | {error, term()}.

%% =============================================================================
%% Protocol Length Validation (DoS Protection)
%% =============================================================================
-define(MAX_USERNAME_LEN, 256).      %% Max username length
-define(MAX_TARGET_LEN, 256).        %% Max recipient name length
-define(MAX_MSG_LEN, 65536).         %% Max single message (64KB)
-define(MAX_BATCH_LEN, 1048576).     %% Max batch size (1MB)
-define(MAX_MSGID_LEN, 64).          %% Max message ID length

%% Protocol Specification:
%% 0x01 | User (binary) -> {login, User}
%% 0x02 | TargetLen(16) | Target(binary) | MsgLen(16) | Msg(binary) | Rest
%% 0x03 | MsgId (binary) -> {ack, MsgId}
%% 0x04 | TargetLen(16) | Target | BatchLen(32) | BatchBlob -> {batch_send, Target, Blob}
%% 0x05 | TargetLen(16) | Target -> {get_status, Target}
%% 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg -> {send_seq, Target, SeqNo, Msg}
%%       AUDIT FIX: Sequence-numbered message for FIFO ordering (RFC FR-5)

-spec decode(binary()) -> {packet(), binary()} | {more, binary()} | {error, term()}.

decode(<<1, Rest/binary>>) when byte_size(Rest) =< ?MAX_USERNAME_LEN ->
    %% Login: Username is the rest of the packet
    { {login, Rest}, <<>> };

decode(<<1, Rest/binary>>) when byte_size(Rest) > ?MAX_USERNAME_LEN ->
    %% Username too long - reject to prevent DoS
    { {error, username_too_long}, <<>> };

decode(<<2, _/binary>> = Bin) when byte_size(Bin) < 3 ->
    {more, Bin};  %% Partial header

decode(<<2, TargetLen:16, _/binary>>) when TargetLen > ?MAX_TARGET_LEN ->
    %% Target name too long - reject early
    { {error, target_too_long}, <<>> };

decode(<<2, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<Target:TargetLen/binary, MsgLen:16, _/binary>> when MsgLen > ?MAX_MSG_LEN ->
            %% Message too long - reject
            { {error, message_too_long}, <<>> };
        <<Target:TargetLen/binary, MsgLen:16, Msg:MsgLen/binary, Rem/binary>> ->
            { {send_message, Target, Msg}, Rem };
        _ ->
            {more, <<2, TargetLen:16, Rest/binary>>}
    end;
    
decode(<<3, MsgId/binary>>) when byte_size(MsgId) =< ?MAX_MSGID_LEN ->
    { {ack, MsgId}, <<>> };

decode(<<3, MsgId/binary>>) when byte_size(MsgId) > ?MAX_MSGID_LEN ->
    { {error, msgid_too_long}, <<>> };

decode(<<4, _/binary>> = Bin) when byte_size(Bin) < 7 ->
     {more, Bin}; %% Need TLen(2)+BLen(4) min

decode(<<4, TargetLen:16, _/binary>>) when TargetLen > ?MAX_TARGET_LEN ->
    { {error, target_too_long}, <<>> };

decode(<<4, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<_Target:TargetLen/binary, BatchLen:32, _/binary>> when BatchLen > ?MAX_BATCH_LEN ->
            %% Batch too large - reject
            { {error, batch_too_large}, <<>> };
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

%% AUDIT FIX: Sequence-numbered message for FIFO ordering (RFC FR-5)
%% Format: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
decode(<<7, _/binary>> = Bin) when byte_size(Bin) < 3 ->
    {more, Bin};
    
decode(<<7, TargetLen:16, _/binary>>) when TargetLen > ?MAX_TARGET_LEN ->
    { {error, target_too_long}, <<>> };
    
decode(<<7, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<Target:TargetLen/binary, SeqNo:64, MsgLen:16, _/binary>> when MsgLen > ?MAX_MSG_LEN ->
            { {error, message_too_long}, <<>> };
        <<Target:TargetLen/binary, SeqNo:64, MsgLen:16, Msg:MsgLen/binary, Rem/binary>> ->
            { {send_seq, Target, SeqNo, Msg}, Rem };
        _ ->
            {more, <<7, TargetLen:16, Rest/binary>>}
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

%% Encode reliable message with proper framing (includes MsgLen)
encode_reliable_msg(MsgId, Msg) ->
    IdLen = byte_size(MsgId),
    MsgLen = byte_size(Msg),
    <<16, IdLen:16, MsgId/binary, MsgLen:32, Msg/binary>>.

%% Decode reliable message (opcode 16)
decode_reliable_msg(<<16, IdLen:16, Rest/binary>>) when byte_size(Rest) >= IdLen + 4 ->
    <<MsgId:IdLen/binary, MsgLen:32, Tail/binary>> = Rest,
    case Tail of
        <<Msg:MsgLen/binary, Rem/binary>> ->
            {{reliable_msg, MsgId, Msg}, Rem};
        _ ->
            {more, <<16, IdLen:16, Rest/binary>>}
    end;
decode_reliable_msg(Bin) ->
    {more, Bin}.

%% AUDIT FIX: Encode sequence-numbered message (RFC FR-5)
%% Format: 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg
encode_seq_msg(Target, SeqNo, Msg) ->
    TLen = byte_size(Target),
    MLen = byte_size(Msg),
    <<7, TLen:16, Target/binary, SeqNo:64, MLen:16, Msg/binary>>.

%% Generate unique message ID (RFC §5.2 - globally unique, sortable)
generate_msg_id() ->
    %% Combine monotonic time, unique integer, and node hash for uniqueness
    Time = erlang:monotonic_time(),
    Unique = erlang:unique_integer([positive]),
    Node = erlang:phash2(node(), 16#FFFF),
    iolist_to_binary(io_lib:format("~.16b~.16b~.4b", [Time band 16#FFFFFFFFFFFF, Unique band 16#FFFFFFFF, Node])).
