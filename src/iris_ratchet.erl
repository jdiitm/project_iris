-module(iris_ratchet).

%% =============================================================================
%% Double Ratchet Algorithm (RFC-001-AMENDMENT-001)
%% Signal Protocol Implementation
%% =============================================================================
%%
%% Implements the Double Ratchet algorithm for continuous key derivation
%% and forward secrecy. Per Signal Protocol specification:
%% https://signal.org/docs/specifications/doubleratchet/
%%
%% Components:
%% - DH Ratchet: Derives new root keys on each message exchange
%% - Symmetric-key Ratchet: Derives message keys from chain keys
%%
%% Properties:
%% - Forward Secrecy: Compromise of current keys doesn't reveal past messages
%% - Break-in Recovery: After key compromise, security is restored on next DH
%%
%% =============================================================================

-export([
    %% Session Initialization
    init_alice/2,          %% Alice initiates with shared secret from X3DH
    init_bob/3,            %% Bob initializes with shared secret + Alice's ratchet key
    
    %% Message Encryption/Decryption
    encrypt/2,             %% Encrypt a message
    decrypt/3,             %% Decrypt a message (Ciphertext, Header, State)
    
    %% State Management
    get_state/1,           %% Get serializable state
    from_state/1,          %% Restore from serialized state
    
    %% Utilities
    generate_ratchet_keypair/0
]).

%% HKDF info strings
-define(ROOT_INFO, <<"IrisRatchetRoot">>).
-define(CHAIN_INFO, <<"IrisRatchetChain">>).
-define(MSG_INFO, <<"IrisRatchetMsg">>).

%% Message key derivation constant
-define(MSG_KEY_SEED, <<16#01>>).
-define(CHAIN_KEY_SEED, <<16#02>>).

%% Maximum message skip (to prevent DoS)
-define(MAX_SKIP, 1000).

%% =============================================================================
%% Records
%% =============================================================================

-record(ratchet_state, {
    %% DH Ratchet
    dh_pair :: {PublicKey :: binary(), PrivateKey :: binary()},
    dh_remote :: binary() | undefined,
    
    %% Root chain
    root_key :: binary(),
    
    %% Sending chain
    send_chain_key :: binary() | undefined,
    send_msg_number :: non_neg_integer(),
    
    %% Receiving chain
    recv_chain_key :: binary() | undefined,
    recv_msg_number :: non_neg_integer(),
    
    %% Previous sending chain number (for header)
    prev_send_count :: non_neg_integer(),
    
    %% Skipped message keys (for out-of-order delivery)
    skipped_keys :: #{binary() => binary()}  %% {DHPub, N} -> MsgKey
}).

-record(message_header, {
    dh_public :: binary(),        %% Sender's current ratchet public key
    prev_chain_len :: non_neg_integer(),  %% Previous chain message count
    msg_number :: non_neg_integer()       %% Message number in current chain
}).

%% =============================================================================
%% Session Initialization
%% =============================================================================

%% @doc Initialize ratchet as Alice (initiator)
%% Alice has the shared secret from X3DH and generates first ratchet key pair
-spec init_alice(SharedSecret :: binary(), BobRatchetKey :: binary()) -> 
    {ok, #ratchet_state{}}.
init_alice(SharedSecret, BobRatchetKey) ->
    %% Alice generates her first ratchet key pair
    {AlicePub, AlicePriv} = generate_ratchet_keypair(),
    
    %% Perform initial DH ratchet step
    DHOutput = x25519_shared(AlicePriv, BobRatchetKey),
    {RootKey, SendChainKey} = kdf_rk(SharedSecret, DHOutput),
    
    State = #ratchet_state{
        dh_pair = {AlicePub, AlicePriv},
        dh_remote = BobRatchetKey,
        root_key = RootKey,
        send_chain_key = SendChainKey,
        send_msg_number = 0,
        recv_chain_key = undefined,
        recv_msg_number = 0,
        prev_send_count = 0,
        skipped_keys = #{}
    },
    
    {ok, State}.

%% @doc Initialize ratchet as Bob (responder)
%% Bob has the shared secret and his pre-generated ratchet key pair
-spec init_bob(SharedSecret :: binary(), BobKeyPair :: {binary(), binary()}, 
               AliceRatchetKey :: binary() | undefined) -> {ok, #ratchet_state{}}.
init_bob(SharedSecret, {BobPub, BobPriv}, undefined) ->
    %% Bob hasn't received Alice's ratchet key yet
    State = #ratchet_state{
        dh_pair = {BobPub, BobPriv},
        dh_remote = undefined,
        root_key = SharedSecret,
        send_chain_key = undefined,
        send_msg_number = 0,
        recv_chain_key = undefined,
        recv_msg_number = 0,
        prev_send_count = 0,
        skipped_keys = #{}
    },
    {ok, State};

init_bob(SharedSecret, {BobPub, BobPriv}, AliceRatchetKey) ->
    %% Bob can derive receive chain immediately
    DHOutput = x25519_shared(BobPriv, AliceRatchetKey),
    {RootKey, RecvChainKey} = kdf_rk(SharedSecret, DHOutput),
    
    State = #ratchet_state{
        dh_pair = {BobPub, BobPriv},
        dh_remote = AliceRatchetKey,
        root_key = RootKey,
        send_chain_key = undefined,
        send_msg_number = 0,
        recv_chain_key = RecvChainKey,
        recv_msg_number = 0,
        prev_send_count = 0,
        skipped_keys = #{}
    },
    {ok, State}.

%% =============================================================================
%% Message Encryption
%% =============================================================================

%% @doc Encrypt a plaintext message
-spec encrypt(Plaintext :: binary(), State :: #ratchet_state{}) ->
    {ok, Ciphertext :: binary(), Header :: map(), NewState :: #ratchet_state{}}.
encrypt(Plaintext, State) ->
    %% Ensure we have a send chain
    State1 = ensure_send_chain(State),
    
    %% Derive message key from send chain
    {MsgKey, NewChainKey} = kdf_ck(State1#ratchet_state.send_chain_key),
    
    %% Build header
    {DHPub, _} = State1#ratchet_state.dh_pair,
    Header = #{
        dh_public => DHPub,
        prev_chain_len => State1#ratchet_state.prev_send_count,
        msg_number => State1#ratchet_state.send_msg_number
    },
    
    %% Encrypt with AES-256-GCM
    Ciphertext = aead_encrypt(MsgKey, Plaintext, encode_header(Header)),
    
    %% Update state
    NewState = State1#ratchet_state{
        send_chain_key = NewChainKey,
        send_msg_number = State1#ratchet_state.send_msg_number + 1
    },
    
    {ok, Ciphertext, Header, NewState}.

%% =============================================================================
%% Message Decryption
%% =============================================================================

%% @doc Decrypt a ciphertext message
-spec decrypt(Ciphertext :: binary(), Header :: map(), State :: #ratchet_state{}) ->
    {ok, Plaintext :: binary(), NewState :: #ratchet_state{}} | {error, term()}.
decrypt(Ciphertext, Header, State) ->
    DHRemote = maps:get(dh_public, Header),
    MsgNum = maps:get(msg_number, Header),
    PrevChainLen = maps:get(prev_chain_len, Header),
    
    %% Try skipped keys first
    case try_skipped_keys(DHRemote, MsgNum, Ciphertext, Header, State) of
        {ok, Plaintext, NewState} ->
            {ok, Plaintext, NewState};
        not_found ->
            %% Check if this is a new ratchet
            {DHPub, _} = State#ratchet_state.dh_pair,
            case DHRemote =:= State#ratchet_state.dh_remote of
                true ->
                    %% Same ratchet - skip ahead in current chain
                    decrypt_same_chain(Ciphertext, Header, State);
                false when State#ratchet_state.dh_remote =:= undefined ->
                    %% First message from remote
                    decrypt_first_message(Ciphertext, Header, State);
                false ->
                    %% New ratchet key - perform DH ratchet
                    decrypt_new_ratchet(Ciphertext, Header, State)
            end
    end.

decrypt_same_chain(Ciphertext, Header, State) ->
    MsgNum = maps:get(msg_number, Header),
    
    %% Skip messages if needed
    State1 = skip_messages(State#ratchet_state.dh_remote, 
                           MsgNum, 
                           State#ratchet_state.recv_chain_key,
                           State#ratchet_state.recv_msg_number,
                           State),
    
    %% Derive and use message key
    {MsgKey, NewChainKey} = kdf_ck(State1#ratchet_state.recv_chain_key),
    
    case aead_decrypt(MsgKey, Ciphertext, encode_header(Header)) of
        {ok, Plaintext} ->
            NewState = State1#ratchet_state{
                recv_chain_key = NewChainKey,
                recv_msg_number = MsgNum + 1
            },
            {ok, Plaintext, NewState};
        error ->
            {error, decryption_failed}
    end.

decrypt_first_message(Ciphertext, Header, State) ->
    DHRemote = maps:get(dh_public, Header),
    {_, DHPriv} = State#ratchet_state.dh_pair,
    
    %% Derive receive chain
    DHOutput = x25519_shared(DHPriv, DHRemote),
    {RootKey, RecvChainKey} = kdf_rk(State#ratchet_state.root_key, DHOutput),
    
    State1 = State#ratchet_state{
        dh_remote = DHRemote,
        root_key = RootKey,
        recv_chain_key = RecvChainKey,
        recv_msg_number = 0
    },
    
    decrypt_same_chain(Ciphertext, Header, State1).

decrypt_new_ratchet(Ciphertext, Header, State) ->
    DHRemote = maps:get(dh_public, Header),
    PrevChainLen = maps:get(prev_chain_len, Header),
    {_, DHPriv} = State#ratchet_state.dh_pair,
    
    %% Store skipped keys from current receive chain
    State1 = skip_messages(State#ratchet_state.dh_remote,
                           PrevChainLen,
                           State#ratchet_state.recv_chain_key,
                           State#ratchet_state.recv_msg_number,
                           State),
    
    %% Perform DH ratchet
    DHOutput = x25519_shared(DHPriv, DHRemote),
    {RootKey, RecvChainKey} = kdf_rk(State1#ratchet_state.root_key, DHOutput),
    
    State2 = State1#ratchet_state{
        dh_remote = DHRemote,
        root_key = RootKey,
        recv_chain_key = RecvChainKey,
        recv_msg_number = 0,
        prev_send_count = State1#ratchet_state.send_msg_number,
        send_chain_key = undefined
    },
    
    decrypt_same_chain(Ciphertext, Header, State2).

%% =============================================================================
%% State Management
%% =============================================================================

%% @doc Export state as map for serialization
-spec get_state(#ratchet_state{}) -> map().
get_state(State) ->
    {DHPub, DHPriv} = State#ratchet_state.dh_pair,
    #{
        dh_public => DHPub,
        dh_private => DHPriv,
        dh_remote => State#ratchet_state.dh_remote,
        root_key => State#ratchet_state.root_key,
        send_chain_key => State#ratchet_state.send_chain_key,
        send_msg_number => State#ratchet_state.send_msg_number,
        recv_chain_key => State#ratchet_state.recv_chain_key,
        recv_msg_number => State#ratchet_state.recv_msg_number,
        prev_send_count => State#ratchet_state.prev_send_count,
        skipped_keys => State#ratchet_state.skipped_keys
    }.

%% @doc Restore state from map
-spec from_state(map()) -> #ratchet_state{}.
from_state(Map) ->
    #ratchet_state{
        dh_pair = {maps:get(dh_public, Map), maps:get(dh_private, Map)},
        dh_remote = maps:get(dh_remote, Map),
        root_key = maps:get(root_key, Map),
        send_chain_key = maps:get(send_chain_key, Map),
        send_msg_number = maps:get(send_msg_number, Map),
        recv_chain_key = maps:get(recv_chain_key, Map),
        recv_msg_number = maps:get(recv_msg_number, Map),
        prev_send_count = maps:get(prev_send_count, Map),
        skipped_keys = maps:get(skipped_keys, Map, #{})
    }.

%% @doc Generate a ratchet key pair (X25519)
-spec generate_ratchet_keypair() -> {PublicKey :: binary(), PrivateKey :: binary()}.
generate_ratchet_keypair() ->
    crypto:generate_key(ecdh, x25519).

%% =============================================================================
%% Internal: Key Derivation Functions
%% =============================================================================

%% KDF for root key and chain key derivation
kdf_rk(RootKey, DHOutput) ->
    %% HKDF with root key as salt
    Output = hkdf_sha256(DHOutput, RootKey, ?ROOT_INFO, 64),
    <<NewRootKey:32/binary, ChainKey:32/binary>> = Output,
    {NewRootKey, ChainKey}.

%% KDF for chain key and message key derivation
kdf_ck(ChainKey) ->
    MsgKey = crypto:mac(hmac, sha256, ChainKey, ?MSG_KEY_SEED),
    NewChainKey = crypto:mac(hmac, sha256, ChainKey, ?CHAIN_KEY_SEED),
    {MsgKey, NewChainKey}.

%% HKDF-SHA256
hkdf_sha256(IKM, Salt, Info, Length) ->
    PRK = crypto:mac(hmac, sha256, Salt, IKM),
    hkdf_expand(PRK, Info, Length, 1, <<>>).

hkdf_expand(_PRK, _Info, Length, _Counter, Acc) when byte_size(Acc) >= Length ->
    binary:part(Acc, 0, Length);
hkdf_expand(PRK, Info, Length, Counter, Acc) ->
    PrevT = case Acc of
        <<>> -> <<>>;
        _ -> binary:part(Acc, byte_size(Acc) - 32, 32)
    end,
    T = crypto:mac(hmac, sha256, PRK, <<PrevT/binary, Info/binary, Counter>>),
    hkdf_expand(PRK, Info, Length, Counter + 1, <<Acc/binary, T/binary>>).

%% =============================================================================
%% Internal: Cryptographic Primitives
%% =============================================================================

x25519_shared(PrivateKey, PublicKey) ->
    crypto:compute_key(ecdh, PublicKey, PrivateKey, x25519).

%% AES-256-GCM encryption
aead_encrypt(Key, Plaintext, AAD) ->
    IV = crypto:strong_rand_bytes(12),
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, Key, IV, Plaintext, AAD, 16, true
    ),
    <<IV/binary, Tag/binary, Ciphertext/binary>>.

%% AES-256-GCM decryption
aead_decrypt(Key, FullCiphertext, AAD) ->
    case FullCiphertext of
        <<IV:12/binary, Tag:16/binary, Ciphertext/binary>> ->
            try
                case crypto:crypto_one_time_aead(
                    aes_256_gcm, Key, IV, Ciphertext, AAD, Tag, false
                ) of
                    error -> 
                        %% Authentication failed (newer Erlang returns atom 'error')
                        error;
                    Plaintext when is_binary(Plaintext) -> 
                        {ok, Plaintext}
                end
            catch
                _:_ -> error
            end;
        _ ->
            error
    end.

%% =============================================================================
%% Internal: Helper Functions
%% =============================================================================

%% Ensure we have a send chain (perform DH ratchet if needed)
ensure_send_chain(State) when State#ratchet_state.send_chain_key =/= undefined ->
    State;
ensure_send_chain(State) ->
    %% Generate new DH key pair
    {NewPub, NewPriv} = generate_ratchet_keypair(),
    
    %% Derive new send chain
    DHOutput = x25519_shared(NewPriv, State#ratchet_state.dh_remote),
    {RootKey, SendChainKey} = kdf_rk(State#ratchet_state.root_key, DHOutput),
    
    State#ratchet_state{
        dh_pair = {NewPub, NewPriv},
        root_key = RootKey,
        send_chain_key = SendChainKey,
        send_msg_number = 0,
        prev_send_count = State#ratchet_state.send_msg_number
    }.

%% Encode header for AEAD associated data
encode_header(Header) ->
    DHPub = maps:get(dh_public, Header),
    PrevChainLen = maps:get(prev_chain_len, Header),
    MsgNum = maps:get(msg_number, Header),
    <<DHPub/binary, PrevChainLen:32, MsgNum:32>>.

%% Try to decrypt with skipped keys
try_skipped_keys(DHRemote, MsgNum, Ciphertext, Header, State) ->
    Key = {DHRemote, MsgNum},
    case maps:get(Key, State#ratchet_state.skipped_keys, undefined) of
        undefined ->
            not_found;
        MsgKey ->
            case aead_decrypt(MsgKey, Ciphertext, encode_header(Header)) of
                {ok, Plaintext} ->
                    NewSkipped = maps:remove(Key, State#ratchet_state.skipped_keys),
                    {ok, Plaintext, State#ratchet_state{skipped_keys = NewSkipped}};
                error ->
                    not_found
            end
    end.

%% Skip messages and store their keys
skip_messages(_DHRemote, TargetNum, _ChainKey, CurrentNum, State) 
  when TargetNum =< CurrentNum ->
    State;
skip_messages(_DHRemote, TargetNum, _ChainKey, CurrentNum, _State) 
  when TargetNum - CurrentNum > ?MAX_SKIP ->
    throw({error, too_many_skipped_messages});
skip_messages(DHRemote, TargetNum, ChainKey, CurrentNum, State) 
  when ChainKey =:= undefined ->
    State;  %% No chain to skip from
skip_messages(DHRemote, TargetNum, ChainKey, CurrentNum, State) ->
    {MsgKey, NewChainKey} = kdf_ck(ChainKey),
    Key = {DHRemote, CurrentNum},
    NewSkipped = maps:put(Key, MsgKey, State#ratchet_state.skipped_keys),
    skip_messages(DHRemote, TargetNum, NewChainKey, CurrentNum + 1, 
                  State#ratchet_state{
                      skipped_keys = NewSkipped,
                      recv_chain_key = NewChainKey
                  }).
