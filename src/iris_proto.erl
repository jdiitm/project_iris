-module(iris_proto).
-export([decode/1, unpack_batch/1, encode_status/3, encode_reliable_msg/2]).
-export([encode_seq_msg/3]).  %% AUDIT FIX: Sequence-numbered message encoder (RFC FR-5)
-export([generate_msg_id/0]). %% Unique message ID generator

%% CBOR Protocol Extensions (RFC-001-AMENDMENT-001, Opcode 0x10)
-export([encode_cbor_msg/2, decode_cbor_msg/1]).
-export([cbor_encode/1, cbor_decode/1]).

%% E2EE Protocol Extensions (RFC-001-AMENDMENT-001, Opcodes 0x20-0x24)
-export([encode_upload_prekeys/1, encode_fetch_prekeys/1]).
-export([encode_prekey_response/1, encode_e2ee_msg/2, encode_e2ee_delivery/2]).

-type packet() :: {login, binary()}
                | {send_message, binary(), binary()}
                | {cbor_msg, binary(), map()}  %% CBOR extensible message
                | {upload_prekeys, map()}      %% E2EE: Key bundle upload
                | {fetch_prekeys, binary()}    %% E2EE: Request keys for user
                | {prekey_response, map()}     %% E2EE: Key bundle response
                | {e2ee_msg, binary(), binary(), map()}  %% E2EE: Encrypted message
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
-define(MAX_CBOR_LEN, 262144).       %% Max CBOR payload (256KB)

%% E2EE Protocol Limits (RFC-001-AMENDMENT-001)
-define(MAX_KEYBUNDLE_LEN, 8192).    %% Max key bundle size (8KB)
-define(MAX_E2EE_HEADER_LEN, 1024).  %% Max E2EE header size (1KB)
-define(MAX_E2EE_CIPHER_LEN, 65536). %% Max ciphertext size (64KB)

%% =============================================================================
%% CBOR Major Types (RFC 8949)
%% =============================================================================
-define(CBOR_UINT, 0).          %% Unsigned integer
-define(CBOR_NEGINT, 1).        %% Negative integer  
-define(CBOR_BYTES, 2).         %% Byte string
-define(CBOR_TEXT, 3).          %% Text string (UTF-8)
-define(CBOR_ARRAY, 4).         %% Array
-define(CBOR_MAP, 5).           %% Map
-define(CBOR_TAG, 6).           %% Semantic tag
-define(CBOR_SIMPLE, 7).        %% Simple/float

%% Protocol Specification:
%% 0x01 | User (binary) -> {login, User}
%% 0x02 | TargetLen(16) | Target(binary) | MsgLen(16) | Msg(binary) | Rest
%% 0x03 | MsgId (binary) -> {ack, MsgId}
%% 0x04 | TargetLen(16) | Target | BatchLen(32) | BatchBlob -> {batch_send, Target, Blob}
%% 0x05 | TargetLen(16) | Target -> {get_status, Target}
%% 0x07 | TargetLen(16) | Target | SeqNo(64) | MsgLen(16) | Msg -> {send_seq, Target, SeqNo, Msg}
%%       AUDIT FIX: Sequence-numbered message for FIFO ordering (RFC FR-5)
%% 0x10 | TargetLen(16) | Target | CborLen(32) | CborPayload -> {cbor_msg, Target, Map}
%%       RFC-001-AMENDMENT-001: CBOR-encoded extensible message
%%
%% E2EE Protocol (RFC-001-AMENDMENT-001):
%% 0x20 | BundleLen(32) | Bundle(CBOR) -> {upload_prekeys, Bundle}
%%       Client uploads key bundle (IK, SPK, OPKs)
%% 0x21 | UserLen(16) | User -> {fetch_prekeys, User}
%%       Client requests recipient's public keys
%% 0x22 | BundleLen(32) | Bundle(CBOR) -> {prekey_response, Bundle}
%%       Server returns key bundle
%% 0x23 | RecipientLen(16) | Recipient | HeaderLen(16) | Header(CBOR) | CipherLen(32) | Ciphertext
%%       -> {e2ee_msg, Recipient, Ciphertext, Header}
%% 0x24 | SenderLen(16) | Sender | HeaderLen(16) | Header(CBOR) | CipherLen(32) | Ciphertext
%%       -> {e2ee_delivery, Sender, Ciphertext, Header}
%%       Server delivers E2EE message to recipient

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

%% =============================================================================
%% CBOR Message (Opcode 0x10) - RFC-001-AMENDMENT-001
%% Format: 0x10 | TargetLen(16) | Target | CborLen(32) | CborPayload
%% =============================================================================
decode(<<16#10, _/binary>> = Bin) when byte_size(Bin) < 7 ->
    {more, Bin};

decode(<<16#10, TargetLen:16, _/binary>>) when TargetLen > ?MAX_TARGET_LEN ->
    { {error, target_too_long}, <<>> };

decode(<<16#10, TargetLen:16, Rest/binary>>) ->
    case Rest of
        <<_Target:TargetLen/binary, CborLen:32, _/binary>> when CborLen > ?MAX_CBOR_LEN ->
            { {error, cbor_too_large}, <<>> };
        <<Target:TargetLen/binary, CborLen:32, CborPayload:CborLen/binary, Rem/binary>> ->
            case cbor_decode(CborPayload) of
                {ok, Map} when is_map(Map) ->
                    { {cbor_msg, Target, Map}, Rem };
                {ok, _Other} ->
                    { {error, cbor_not_map}, <<>> };
                {error, Reason} ->
                    { {error, {cbor_decode_error, Reason}}, <<>> }
            end;
        _ ->
            {more, <<16#10, TargetLen:16, Rest/binary>>}
    end;

%% =============================================================================
%% E2EE Protocol (Opcodes 0x20-0x24) - RFC-001-AMENDMENT-001
%% =============================================================================

%% 0x20: Upload Prekeys - Client uploads key bundle
decode(<<16#20, _/binary>> = Bin) when byte_size(Bin) < 5 ->
    {more, Bin};

decode(<<16#20, BundleLen:32, _/binary>>) when BundleLen > ?MAX_KEYBUNDLE_LEN ->
    { {error, keybundle_too_large}, <<>> };

decode(<<16#20, BundleLen:32, Bundle:BundleLen/binary, Rem/binary>>) ->
    case cbor_decode(Bundle) of
        {ok, Map} when is_map(Map) ->
            { {upload_prekeys, Map}, Rem };
        _ ->
            { {error, invalid_keybundle}, <<>> }
    end;

decode(<<16#20, _/binary>> = Bin) ->
    {more, Bin};

%% 0x21: Fetch Prekeys - Request recipient's keys
decode(<<16#21, _/binary>> = Bin) when byte_size(Bin) < 3 ->
    {more, Bin};

decode(<<16#21, UserLen:16, _/binary>>) when UserLen > ?MAX_USERNAME_LEN ->
    { {error, username_too_long}, <<>> };

decode(<<16#21, UserLen:16, User:UserLen/binary, Rem/binary>>) ->
    { {fetch_prekeys, User}, Rem };

decode(<<16#21, _/binary>> = Bin) ->
    {more, Bin};

%% 0x22: Prekey Response - Server returns key bundle
decode(<<16#22, _/binary>> = Bin) when byte_size(Bin) < 5 ->
    {more, Bin};

decode(<<16#22, BundleLen:32, _/binary>>) when BundleLen > ?MAX_KEYBUNDLE_LEN ->
    { {error, keybundle_too_large}, <<>> };

decode(<<16#22, BundleLen:32, Bundle:BundleLen/binary, Rem/binary>>) ->
    case cbor_decode(Bundle) of
        {ok, Map} when is_map(Map) ->
            { {prekey_response, Map}, Rem };
        _ ->
            { {error, invalid_keybundle}, <<>> }
    end;

decode(<<16#22, _/binary>> = Bin) ->
    {more, Bin};

%% 0x23: E2EE Message - Encrypted message to recipient
decode(<<16#23, _/binary>> = Bin) when byte_size(Bin) < 9 ->
    {more, Bin};

decode(<<16#23, RecipientLen:16, _/binary>>) when RecipientLen > ?MAX_TARGET_LEN ->
    { {error, recipient_too_long}, <<>> };

decode(<<16#23, RecipientLen:16, Rest/binary>>) ->
    case Rest of
        <<Recipient:RecipientLen/binary, HeaderLen:16, _/binary>> 
          when HeaderLen > ?MAX_E2EE_HEADER_LEN ->
            { {error, e2ee_header_too_large}, <<>> };
        <<Recipient:RecipientLen/binary, HeaderLen:16, Header:HeaderLen/binary,
          CipherLen:32, _/binary>> when CipherLen > ?MAX_E2EE_CIPHER_LEN ->
            { {error, e2ee_ciphertext_too_large}, <<>> };
        <<Recipient:RecipientLen/binary, HeaderLen:16, HeaderBytes:HeaderLen/binary,
          CipherLen:32, Ciphertext:CipherLen/binary, Rem/binary>> ->
            case cbor_decode(HeaderBytes) of
                {ok, Header} when is_map(Header) ->
                    { {e2ee_msg, Recipient, Ciphertext, Header}, Rem };
                _ ->
                    { {error, invalid_e2ee_header}, <<>> }
            end;
        _ ->
            {more, <<16#23, RecipientLen:16, Rest/binary>>}
    end;

%% 0x24: E2EE Delivery - Server delivers encrypted message
decode(<<16#24, _/binary>> = Bin) when byte_size(Bin) < 9 ->
    {more, Bin};

decode(<<16#24, SenderLen:16, Rest/binary>>) ->
    case Rest of
        <<Sender:SenderLen/binary, HeaderLen:16, _/binary>> 
          when HeaderLen > ?MAX_E2EE_HEADER_LEN ->
            { {error, e2ee_header_too_large}, <<>> };
        <<Sender:SenderLen/binary, HeaderLen:16, HeaderBytes:HeaderLen/binary,
          CipherLen:32, Ciphertext:CipherLen/binary, Rem/binary>> ->
            case cbor_decode(HeaderBytes) of
                {ok, Header} when is_map(Header) ->
                    { {e2ee_delivery, Sender, Ciphertext, Header}, Rem };
                _ ->
                    { {error, invalid_e2ee_header}, <<>> }
            end;
        _ ->
            {more, <<16#24, SenderLen:16, Rest/binary>>}
    end;

decode(<<>>) -> {more, <<>>};
decode(_Bin) -> { {error, unknown_packet}, <<>> }.

encode_status(User, State, Time) ->
    StateByte = case State of online -> 1; offline -> 0 end,
    U = User, %% User is already binary
    ULen = byte_size(U),
    <<6, ULen:16, U/binary, StateByte, Time:64>>.

unpack_batch(Blob) -> unpack_batch(Blob, 1000). %% Default limit 1000
unpack_batch(Blob, MaxCount) -> unpack_batch_loop(Blob, [], 0, MaxCount).

unpack_batch_loop(<<>>, Acc, _Count, _Max) -> lists:reverse(Acc);
unpack_batch_loop(<<Len:16, Msg:Len/binary, Rest/binary>>, Acc, Count, Max) ->
    if Count >= Max ->
        {error, batch_too_large};
    true ->
        unpack_batch_loop(Rest, [Msg | Acc], Count + 1, Max)
    end;
unpack_batch_loop(_, Acc, _Count, _Max) -> lists:reverse(Acc). %% Tolerant of trailing garbage

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

%% =============================================================================
%% CBOR Codec Implementation (RFC 8949)
%% RFC-001-AMENDMENT-001: Extensible message format
%% =============================================================================

%% @doc Encode a CBOR message with target
%% Format: 0x10 | TargetLen(16) | Target | CborLen(32) | CborPayload
-spec encode_cbor_msg(binary(), map()) -> binary().
encode_cbor_msg(Target, Payload) when is_binary(Target), is_map(Payload) ->
    CborPayload = cbor_encode(Payload),
    TLen = byte_size(Target),
    CLen = byte_size(CborPayload),
    <<16#10, TLen:16, Target/binary, CLen:32, CborPayload/binary>>.

%% @doc Decode a CBOR message (for testing/verification)
-spec decode_cbor_msg(binary()) -> {ok, binary(), map()} | {error, term()}.
decode_cbor_msg(<<16#10, TLen:16, Target:TLen/binary, CLen:32, CborPayload:CLen/binary, _/binary>>) ->
    case cbor_decode(CborPayload) of
        {ok, Map} -> {ok, Target, Map};
        Error -> Error
    end;
decode_cbor_msg(_) ->
    {error, invalid_cbor_msg}.

%% =============================================================================
%% E2EE Protocol Encoding (Opcodes 0x20-0x24) - RFC-001-AMENDMENT-001
%% =============================================================================

%% @doc Encode Upload Prekeys request (0x20)
%% Format: 0x20 | BundleLen(32) | KeyBundle (CBOR)
%% KeyBundle must contain: identity_key, signed_prekey, prekey_signature, one_time_prekeys
-spec encode_upload_prekeys(map()) -> binary().
encode_upload_prekeys(KeyBundle) when is_map(KeyBundle) ->
    CborBundle = cbor_encode(KeyBundle),
    BundleLen = byte_size(CborBundle),
    <<16#20, BundleLen:32, CborBundle/binary>>.

%% @doc Encode Fetch Prekeys request (0x21)
%% Format: 0x21 | UserLen(16) | User
-spec encode_fetch_prekeys(binary()) -> binary().
encode_fetch_prekeys(User) when is_binary(User) ->
    UserLen = byte_size(User),
    <<16#21, UserLen:16, User/binary>>.

%% @doc Encode Prekey Response (0x22) - Server to Client
%% Format: 0x22 | BundleLen(32) | KeyBundle (CBOR)
-spec encode_prekey_response(map()) -> binary().
encode_prekey_response(KeyBundle) when is_map(KeyBundle) ->
    CborBundle = cbor_encode(KeyBundle),
    BundleLen = byte_size(CborBundle),
    <<16#22, BundleLen:32, CborBundle/binary>>.

%% @doc Encode E2EE Message (0x23) - Client to Server
%% Format: 0x23 | RecipientLen(16) | Recipient | HeaderLen(16) | Header (CBOR) | CipherLen(32) | Ciphertext
%% Header contains: msg_type, dh_pub (for X3DH/Ratchet), prev_chain_len, msg_num
-spec encode_e2ee_msg(binary(), {map(), binary()}) -> binary().
encode_e2ee_msg(Recipient, {Header, Ciphertext}) when is_binary(Recipient), 
                                                       is_map(Header), 
                                                       is_binary(Ciphertext) ->
    RecipientLen = byte_size(Recipient),
    HeaderBytes = cbor_encode(Header),
    HeaderLen = byte_size(HeaderBytes),
    CipherLen = byte_size(Ciphertext),
    <<16#23, RecipientLen:16, Recipient/binary, 
      HeaderLen:16, HeaderBytes/binary,
      CipherLen:32, Ciphertext/binary>>.

%% @doc Encode E2EE Delivery (0x24) - Server to Client
%% Format: 0x24 | SenderLen(16) | Sender | HeaderLen(16) | Header (CBOR) | CipherLen(32) | Ciphertext
-spec encode_e2ee_delivery(binary(), {map(), binary()}) -> binary().
encode_e2ee_delivery(Sender, {Header, Ciphertext}) when is_binary(Sender),
                                                         is_map(Header),
                                                         is_binary(Ciphertext) ->
    SenderLen = byte_size(Sender),
    HeaderBytes = cbor_encode(Header),
    HeaderLen = byte_size(HeaderBytes),
    CipherLen = byte_size(Ciphertext),
    <<16#24, SenderLen:16, Sender/binary,
      HeaderLen:16, HeaderBytes/binary,
      CipherLen:32, Ciphertext/binary>>.

%% =============================================================================
%% CBOR Encoding (RFC 8949)
%% =============================================================================

%% @doc Encode an Erlang term to CBOR binary
-spec cbor_encode(term()) -> binary().
cbor_encode(Value) when is_integer(Value), Value >= 0 ->
    cbor_encode_uint(?CBOR_UINT, Value);
cbor_encode(Value) when is_integer(Value), Value < 0 ->
    cbor_encode_uint(?CBOR_NEGINT, -1 - Value);
cbor_encode(Value) when is_binary(Value) ->
    Len = byte_size(Value),
    <<(cbor_encode_uint(?CBOR_BYTES, Len))/binary, Value/binary>>;
cbor_encode(Value) when is_list(Value) ->
    %% Check if it's a printable string (list of printable ASCII) or array
    case is_printable_string(Value) of
        true ->
            Bin = list_to_binary(Value),
            Len = byte_size(Bin),
            <<(cbor_encode_uint(?CBOR_TEXT, Len))/binary, Bin/binary>>;
        false ->
            Len = length(Value),
            Items = << <<(cbor_encode(Item))/binary>> || Item <- Value >>,
            <<(cbor_encode_uint(?CBOR_ARRAY, Len))/binary, Items/binary>>
    end;
cbor_encode(Value) when is_map(Value) ->
    Pairs = maps:to_list(Value),
    Len = length(Pairs),
    Items = << <<(cbor_encode(K))/binary, (cbor_encode(V))/binary>> || {K, V} <- Pairs >>,
    <<(cbor_encode_uint(?CBOR_MAP, Len))/binary, Items/binary>>;
cbor_encode(true) ->
    <<16#F5>>;  %% Simple value 21
cbor_encode(false) ->
    <<16#F4>>;  %% Simple value 20
cbor_encode(null) ->
    <<16#F6>>;  %% Simple value 22
cbor_encode(undefined) ->
    <<16#F7>>;  %% Simple value 23
cbor_encode(Value) when is_float(Value) ->
    <<16#FB, Value:64/float>>;  %% Double-precision float
cbor_encode(Value) when is_atom(Value) ->
    %% Encode atoms as text strings
    Bin = atom_to_binary(Value, utf8),
    cbor_encode(Bin).

%% Encode unsigned integer with major type
cbor_encode_uint(Major, Value) when Value < 24 ->
    <<(Major bsl 5 bor Value)>>;
cbor_encode_uint(Major, Value) when Value < 256 ->
    <<(Major bsl 5 bor 24), Value:8>>;
cbor_encode_uint(Major, Value) when Value < 65536 ->
    <<(Major bsl 5 bor 25), Value:16>>;
cbor_encode_uint(Major, Value) when Value < 4294967296 ->
    <<(Major bsl 5 bor 26), Value:32>>;
cbor_encode_uint(Major, Value) ->
    <<(Major bsl 5 bor 27), Value:64>>.

%% Check if a list is a printable text string
%% Only treat as string if ALL characters are printable ASCII (32-126) or common control chars
is_printable_string([]) -> false;  %% Empty list is an empty array, not empty string
is_printable_string(List) -> is_printable_string_check(List).

is_printable_string_check([]) -> true;
is_printable_string_check([H|T]) when is_integer(H), H >= 32, H =< 126 -> 
    is_printable_string_check(T);
is_printable_string_check([H|T]) when H =:= $\n; H =:= $\r; H =:= $\t -> 
    is_printable_string_check(T);
is_printable_string_check(_) -> false.

%% =============================================================================
%% CBOR Decoding (RFC 8949)
%% =============================================================================

%% @doc Decode a CBOR binary to an Erlang term
-spec cbor_decode(binary()) -> {ok, term()} | {error, term()}.
cbor_decode(Bin) ->
    try
        {Value, _Rest} = cbor_decode_value(Bin),
        {ok, Value}
    catch
        _:Reason -> {error, Reason}
    end.

cbor_decode_value(<<Initial, Rest/binary>>) ->
    Major = Initial bsr 5,
    Additional = Initial band 16#1F,
    cbor_decode_by_major(Major, Additional, Rest).

%% Decode by major type
cbor_decode_by_major(?CBOR_UINT, Additional, Rest) ->
    cbor_decode_uint(Additional, Rest);
cbor_decode_by_major(?CBOR_NEGINT, Additional, Rest) ->
    {Value, Rest2} = cbor_decode_uint(Additional, Rest),
    {-1 - Value, Rest2};
cbor_decode_by_major(?CBOR_BYTES, Additional, Rest) ->
    {Len, Rest2} = cbor_decode_uint(Additional, Rest),
    <<Bytes:Len/binary, Rest3/binary>> = Rest2,
    {Bytes, Rest3};
cbor_decode_by_major(?CBOR_TEXT, Additional, Rest) ->
    {Len, Rest2} = cbor_decode_uint(Additional, Rest),
    <<Text:Len/binary, Rest3/binary>> = Rest2,
    {Text, Rest3};
cbor_decode_by_major(?CBOR_ARRAY, Additional, Rest) ->
    {Len, Rest2} = cbor_decode_uint(Additional, Rest),
    cbor_decode_array(Len, Rest2, []);
cbor_decode_by_major(?CBOR_MAP, Additional, Rest) ->
    {Len, Rest2} = cbor_decode_uint(Additional, Rest),
    cbor_decode_map(Len, Rest2, #{});
cbor_decode_by_major(?CBOR_SIMPLE, 20, Rest) ->
    {false, Rest};
cbor_decode_by_major(?CBOR_SIMPLE, 21, Rest) ->
    {true, Rest};
cbor_decode_by_major(?CBOR_SIMPLE, 22, Rest) ->
    {null, Rest};
cbor_decode_by_major(?CBOR_SIMPLE, 23, Rest) ->
    {undefined, Rest};
cbor_decode_by_major(?CBOR_SIMPLE, 27, <<Float:64/float, Rest/binary>>) ->
    {Float, Rest};
cbor_decode_by_major(?CBOR_SIMPLE, 26, <<Float:32/float, Rest/binary>>) ->
    {Float, Rest};
cbor_decode_by_major(?CBOR_SIMPLE, 25, <<Float:16, Rest/binary>>) ->
    %% Half-precision float (simplified - convert to double)
    {decode_half_float(Float), Rest}.

%% Decode unsigned integer
cbor_decode_uint(Additional, Rest) when Additional < 24 ->
    {Additional, Rest};
cbor_decode_uint(24, <<Value:8, Rest/binary>>) ->
    {Value, Rest};
cbor_decode_uint(25, <<Value:16, Rest/binary>>) ->
    {Value, Rest};
cbor_decode_uint(26, <<Value:32, Rest/binary>>) ->
    {Value, Rest};
cbor_decode_uint(27, <<Value:64, Rest/binary>>) ->
    {Value, Rest}.

%% Decode array
cbor_decode_array(0, Rest, Acc) ->
    {lists:reverse(Acc), Rest};
cbor_decode_array(N, Bin, Acc) ->
    {Value, Rest} = cbor_decode_value(Bin),
    cbor_decode_array(N - 1, Rest, [Value | Acc]).

%% Decode map
cbor_decode_map(0, Rest, Acc) ->
    {Acc, Rest};
cbor_decode_map(N, Bin, Acc) ->
    {Key, Rest1} = cbor_decode_value(Bin),
    {Value, Rest2} = cbor_decode_value(Rest1),
    cbor_decode_map(N - 1, Rest2, maps:put(Key, Value, Acc)).

%% Decode half-precision float (IEEE 754)
decode_half_float(Half) ->
    Sign = Half bsr 15,
    Exp = (Half bsr 10) band 16#1F,
    Mant = Half band 16#3FF,
    case Exp of
        0 when Mant == 0 ->
            case Sign of 0 -> 0.0; 1 -> -0.0 end;
        0 ->
            %% Subnormal
            math:pow(-1, Sign) * math:pow(2, -14) * (Mant / 1024);
        31 when Mant == 0 ->
            case Sign of 0 -> infinity; 1 -> neg_infinity end;
        31 ->
            nan;
        _ ->
            math:pow(-1, Sign) * math:pow(2, Exp - 15) * (1 + Mant / 1024)
    end.
