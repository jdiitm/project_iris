-module(iris_sender_keys).

%% =============================================================================
%% Sender Keys for Group E2EE
%% =============================================================================
%% Purpose: Implement Sender Keys algorithm for efficient group E2EE.
%% RFC Reference: FR-13 (Group Messaging), RFC-001-AMENDMENT-001
%%
%% Design:
%% 1. Each group member generates a unique sender key (32-byte symmetric key)
%% 2. Sender keys are distributed to other members via 1:1 E2EE channels
%% 3. Messages to the group are encrypted with sender's key
%% 4. Recipients decrypt using the sender's previously distributed key
%%
%% Key Structure:
%% - SenderKey = {ChainKey, SigningKeyPair}
%% - ChainKey: 32-byte key for symmetric ratchet
%% - SigningKeyPair: Ed25519 keypair for message authentication
%%
%% Message Format:
%% - KeyId: 8 bytes (identifies which sender key was used)
%% - ChainIndex: 4 bytes (message number in chain)
%% - Ciphertext: Variable (AES-256-GCM encrypted)
%% - Signature: 64 bytes (Ed25519 signature)
%% =============================================================================

-export([
    %% Key generation
    generate_sender_key/0,       %% () -> SenderKey
    
    %% Key distribution (creates E2EE message for distributing key)
    create_key_distribution_msg/3,  %% (SenderKey, RecipientId, OurRatchetState) -> {Header, Ciphertext}
    process_key_distribution_msg/3, %% (Ciphertext, Header, TheirRatchetState) -> SenderKey
    
    %% Group message encryption/decryption
    encrypt_group_msg/3,         %% (Plaintext, SenderKey, KeyId) -> {Ciphertext, NewSenderKey}
    decrypt_group_msg/3,         %% (Ciphertext, SenderKey, KeyId) -> {ok, Plaintext} | {error, Reason}
    
    %% Key management
    advance_chain/1,             %% (SenderKey) -> NewSenderKey
    get_key_id/1,                %% (SenderKey) -> KeyId
    serialize_sender_key/1,      %% (SenderKey) -> Binary
    deserialize_sender_key/1     %% (Binary) -> SenderKey
]).

%% Sender key record
-record(sender_key, {
    key_id      :: binary(),           %% 8-byte unique identifier
    chain_key   :: binary(),           %% 32-byte chain key for symmetric ratchet
    sign_priv   :: binary(),           %% 32-byte Ed25519 private key
    sign_pub    :: binary(),           %% 32-byte Ed25519 public key
    chain_index :: non_neg_integer()   %% Current message number in chain
}).

-define(KEY_ID_LEN, 8).
-define(CHAIN_KEY_LEN, 32).
-define(SIGN_KEY_LEN, 32).
-define(NONCE_LEN, 12).
-define(TAG_LEN, 16).
-define(SIGNATURE_LEN, 64).

%% =============================================================================
%% Key Generation
%% =============================================================================

%% @doc Generate a new sender key for group messaging.
-spec generate_sender_key() -> #sender_key{}.
generate_sender_key() ->
    %% Generate unique key ID
    KeyId = crypto:strong_rand_bytes(?KEY_ID_LEN),
    
    %% Generate chain key for symmetric encryption
    ChainKey = crypto:strong_rand_bytes(?CHAIN_KEY_LEN),
    
    %% Generate Ed25519 signing keypair
    %% crypto:generate_key returns {PublicKey, PrivateKey} for EdDSA
    {SignPub, SignPriv} = crypto:generate_key(eddsa, ed25519),
    
    #sender_key{
        key_id = KeyId,
        chain_key = ChainKey,
        sign_priv = SignPriv,
        sign_pub = SignPub,
        chain_index = 0
    }.

%% @doc Get the key ID of a sender key.
-spec get_key_id(#sender_key{}) -> binary().
get_key_id(#sender_key{key_id = KeyId}) ->
    KeyId.

%% =============================================================================
%% Key Distribution
%% =============================================================================

%% @doc Create a message to distribute our sender key to another group member.
%% This message should be sent over the 1:1 E2EE channel (Double Ratchet).
-spec create_key_distribution_msg(#sender_key{}, binary(), term()) -> 
    {map(), binary()}.
create_key_distribution_msg(SenderKey, _RecipientId, RatchetState) ->
    %% Serialize sender key for distribution
    KeyData = serialize_sender_key(SenderKey),
    
    %% Encrypt with Double Ratchet
    {Ciphertext, NewRatchetState} = iris_ratchet:encrypt(RatchetState, KeyData),
    
    %% Create header for key distribution message
    Header = #{
        msg_type => <<"sender_key_distribution">>,
        key_id => SenderKey#sender_key.key_id,
        sign_pub => SenderKey#sender_key.sign_pub
    },
    
    {Header, Ciphertext, NewRatchetState}.

%% @doc Process a received sender key distribution message.
-spec process_key_distribution_msg(binary(), map(), term()) -> 
    {ok, #sender_key{}, term()} | {error, term()}.
process_key_distribution_msg(Ciphertext, Header, RatchetState) ->
    %% Decrypt with Double Ratchet
    case iris_ratchet:decrypt(RatchetState, Ciphertext, Header) of
        {ok, KeyData, NewRatchetState} ->
            %% Deserialize sender key
            SenderKey = deserialize_sender_key(KeyData),
            {ok, SenderKey, NewRatchetState};
        {error, Reason} ->
            {error, Reason}
    end.

%% =============================================================================
%% Group Message Encryption/Decryption
%% =============================================================================

%% @doc Encrypt a message for the group using sender key.
-spec encrypt_group_msg(binary(), #sender_key{}, binary()) -> 
    {binary(), #sender_key{}}.
encrypt_group_msg(Plaintext, SenderKey, _KeyId) ->
    #sender_key{
        key_id = KeyId,
        chain_key = ChainKey,
        sign_priv = SignPriv,
        chain_index = ChainIndex
    } = SenderKey,
    
    %% Derive message key from chain key
    MessageKey = derive_message_key(ChainKey, ChainIndex),
    
    %% Generate nonce (unique per message)
    Nonce = <<ChainIndex:32, (crypto:strong_rand_bytes(8))/binary>>,
    
    %% Encrypt with AES-256-GCM
    AAD = <<KeyId/binary, ChainIndex:32>>,
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, 
        MessageKey, 
        Nonce, 
        Plaintext, 
        AAD, 
        ?TAG_LEN,
        true
    ),
    
    %% Sign the ciphertext for authentication
    DataToSign = <<KeyId/binary, ChainIndex:32, Nonce/binary, Ciphertext/binary, Tag/binary>>,
    Signature = crypto:sign(eddsa, sha512, DataToSign, [SignPriv, ed25519]),
    
    %% Assemble encrypted message
    EncryptedMsg = <<
        KeyId:?KEY_ID_LEN/binary,
        ChainIndex:32,
        Nonce:?NONCE_LEN/binary,
        (byte_size(Ciphertext)):32,
        Ciphertext/binary,
        Tag:?TAG_LEN/binary,
        Signature:?SIGNATURE_LEN/binary
    >>,
    
    %% Advance chain for next message
    NewSenderKey = advance_chain(SenderKey),
    
    {EncryptedMsg, NewSenderKey}.

%% @doc Decrypt a message from a group member using their sender key.
-spec decrypt_group_msg(binary(), #sender_key{}, binary()) -> 
    {ok, binary()} | {error, term()}.
decrypt_group_msg(EncryptedMsg, SenderKey, ExpectedKeyId) ->
    %% Parse encrypted message
    case EncryptedMsg of
        <<KeyId:?KEY_ID_LEN/binary, ChainIndex:32, Nonce:?NONCE_LEN/binary,
          CipherLen:32, Ciphertext:CipherLen/binary, Tag:?TAG_LEN/binary,
          Signature:?SIGNATURE_LEN/binary>> ->
            
            %% Verify key ID matches
            case KeyId of
                ExpectedKeyId ->
                    %% Get stored chain key for this sender
                    #sender_key{
                        chain_key = ChainKey,
                        sign_pub = SignPub
                    } = SenderKey,
                    
                    %% Verify signature
                    DataToSign = <<KeyId/binary, ChainIndex:32, Nonce/binary, 
                                   Ciphertext/binary, Tag/binary>>,
                    case crypto:verify(eddsa, sha512, DataToSign, Signature, 
                                      [SignPub, ed25519]) of
                        true ->
                            %% Derive message key for this chain index
                            MessageKey = derive_message_key(ChainKey, ChainIndex),
                            
                            %% Decrypt with AES-256-GCM
                            AAD = <<KeyId/binary, ChainIndex:32>>,
                            case crypto:crypto_one_time_aead(
                                aes_256_gcm,
                                MessageKey,
                                Nonce,
                                Ciphertext,
                                AAD,
                                Tag,
                                false
                            ) of
                                Plaintext when is_binary(Plaintext) ->
                                    {ok, Plaintext};
                                error ->
                                    {error, decryption_failed}
                            end;
                        false ->
                            {error, signature_invalid}
                    end;
                _ ->
                    {error, key_id_mismatch}
            end;
        _ ->
            {error, invalid_message_format}
    end.

%% =============================================================================
%% Key Management
%% =============================================================================

%% @doc Advance the sender key chain (symmetric ratchet step).
-spec advance_chain(#sender_key{}) -> #sender_key{}.
advance_chain(SenderKey = #sender_key{chain_key = ChainKey, chain_index = Index}) ->
    %% Derive new chain key using HKDF
    NewChainKey = kdf_chain(ChainKey),
    SenderKey#sender_key{
        chain_key = NewChainKey,
        chain_index = Index + 1
    }.

%% =============================================================================
%% Serialization
%% =============================================================================

%% @doc Serialize a sender key to binary for storage or distribution.
-spec serialize_sender_key(#sender_key{}) -> binary().
serialize_sender_key(#sender_key{
    key_id = KeyId,
    chain_key = ChainKey,
    sign_priv = SignPriv,
    sign_pub = SignPub,
    chain_index = ChainIndex
}) ->
    %% Version byte + fields
    <<1,  %% Version
      KeyId:?KEY_ID_LEN/binary,
      ChainKey:?CHAIN_KEY_LEN/binary,
      SignPriv:?SIGN_KEY_LEN/binary,
      SignPub:?SIGN_KEY_LEN/binary,
      ChainIndex:32>>.

%% @doc Deserialize a sender key from binary.
-spec deserialize_sender_key(binary()) -> #sender_key{}.
deserialize_sender_key(<<1,  %% Version 1
                         KeyId:?KEY_ID_LEN/binary,
                         ChainKey:?CHAIN_KEY_LEN/binary,
                         SignPriv:?SIGN_KEY_LEN/binary,
                         SignPub:?SIGN_KEY_LEN/binary,
                         ChainIndex:32>>) ->
    #sender_key{
        key_id = KeyId,
        chain_key = ChainKey,
        sign_priv = SignPriv,
        sign_pub = SignPub,
        chain_index = ChainIndex
    };
deserialize_sender_key(_) ->
    error(invalid_sender_key_format).

%% =============================================================================
%% Internal: Key Derivation
%% =============================================================================

%% @doc Derive a message key from the chain key and index.
derive_message_key(ChainKey, Index) ->
    %% Use HKDF-like derivation
    Info = <<"iris_sender_key_msg_", (integer_to_binary(Index))/binary>>,
    crypto:mac(hmac, sha256, ChainKey, Info).

%% @doc Derive new chain key (KDF chain step).
kdf_chain(ChainKey) ->
    crypto:mac(hmac, sha256, ChainKey, <<"iris_sender_key_chain">>).
