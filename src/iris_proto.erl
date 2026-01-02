-module(iris_proto).
-export([decode/1]).

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

decode(<<>>) -> {more, <<>>};
decode(_Bin) -> { {error, unknown_packet}, <<>> }.
