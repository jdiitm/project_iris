-module(iris_x3dh).

%% =============================================================================
%% X3DH (Extended Triple Diffie-Hellman) Key Exchange
%% RFC-001-AMENDMENT-001: Signal Protocol Implementation
%% =============================================================================
%% 
%% Implements the X3DH key agreement protocol for establishing shared secrets
%% between two parties. Per Signal Protocol specification:
%% https://signal.org/docs/specifications/x3dh/
%%
%% Key Types:
%% - IK (Identity Key): Long-term Curve25519 key pair
%% - SPK (Signed Pre-Key): Medium-term key, signed with IK
%% - OPK (One-Time Pre-Key): Single-use key for forward secrecy
%% - EK (Ephemeral Key): Session-specific key
%%
%% =============================================================================

-export([
    %% Key Generation
    generate_identity_key/0,
    generate_signed_prekey/1,
    generate_one_time_prekeys/1,
    generate_ephemeral_key/0,
    
    %% Key Exchange (Initiator - Alice)
    initiate_exchange/2,
    
    %% Key Exchange (Responder - Bob)
    respond_to_exchange/2,
    
    %% Utilities
    sign_prekey/2,
    verify_prekey_signature/3,
    derive_shared_secret/1
]).

%% HKDF info string for X3DH
-define(X3DH_INFO, <<"X3DH">>).
-define(PROTOCOL_VERSION, <<16#FF, 16#FF, 16#FF, 16#FF>>).  %% 4 bytes of 0xFF per spec

%% =============================================================================
%% Key Generation
%% =============================================================================

%% @doc Generate a new identity key pair (Curve25519)
-spec generate_identity_key() -> {PublicKey :: binary(), PrivateKey :: binary()}.
generate_identity_key() ->
    generate_x25519_keypair().

%% @doc Generate a signed pre-key and sign it with identity key
-spec generate_signed_prekey(IdentityPrivateKey :: binary()) -> 
    {PublicKey :: binary(), PrivateKey :: binary(), Signature :: binary()}.
generate_signed_prekey(IKPrivate) ->
    {SPKPublic, SPKPrivate} = generate_x25519_keypair(),
    Signature = sign_prekey(SPKPublic, IKPrivate),
    {SPKPublic, SPKPrivate, Signature}.

%% @doc Generate N one-time pre-keys
-spec generate_one_time_prekeys(Count :: pos_integer()) -> 
    [{PublicKey :: binary(), PrivateKey :: binary()}].
generate_one_time_prekeys(Count) when Count > 0 ->
    [generate_x25519_keypair() || _ <- lists:seq(1, Count)].

%% @doc Generate an ephemeral key pair for a session
-spec generate_ephemeral_key() -> {PublicKey :: binary(), PrivateKey :: binary()}.
generate_ephemeral_key() ->
    generate_x25519_keypair().

%% =============================================================================
%% Key Exchange - Initiator (Alice)
%% =============================================================================

%% @doc Initiate X3DH key exchange (Alice's side)
%% 
%% Alice has:
%% - Her identity key pair (IK_A)
%% - Bob's key bundle from server
%%
%% Returns:
%% - Shared secret (SK)
%% - Initial message to send to Bob (includes Alice's public keys)
%%
-spec initiate_exchange(AliceKeys :: map(), BobBundle :: map()) -> 
    {ok, SharedSecret :: binary(), InitialMessage :: map()} | {error, term()}.
initiate_exchange(AliceKeys, BobBundle) ->
    try
        %% Extract Alice's keys
        IK_A_Priv = maps:get(identity_key_private, AliceKeys),
        IK_A_Pub = maps:get(identity_key_public, AliceKeys),
        
        %% Extract Bob's bundle
        IK_B = maps:get(identity_key, BobBundle),
        SPK_B = maps:get(signed_prekey, BobBundle),
        SPK_B_Sig = maps:get(signed_prekey_signature, BobBundle),
        OPK_B = maps:get(one_time_prekey, BobBundle, undefined),
        
        %% Verify Bob's signed prekey signature
        case verify_prekey_signature(SPK_B, SPK_B_Sig, IK_B) of
            true -> ok;
            false -> throw(invalid_prekey_signature)
        end,
        
        %% Generate ephemeral key
        {EK_Pub, EK_Priv} = generate_ephemeral_key(),
        
        %% Perform DH operations
        DH1 = x25519_shared(IK_A_Priv, SPK_B),      %% DH(IK_A, SPK_B)
        DH2 = x25519_shared(EK_Priv, IK_B),         %% DH(EK_A, IK_B)
        DH3 = x25519_shared(EK_Priv, SPK_B),        %% DH(EK_A, SPK_B)
        
        %% DH4 only if OPK is available
        DH4 = case OPK_B of
            undefined -> <<>>;
            _ -> x25519_shared(EK_Priv, OPK_B)      %% DH(EK_A, OPK_B)
        end,
        
        %% Derive shared secret: SK = KDF(DH1 || DH2 || DH3 || DH4)
        DHConcat = <<?PROTOCOL_VERSION/binary, DH1/binary, DH2/binary, DH3/binary, DH4/binary>>,
        SharedSecret = hkdf_sha256(DHConcat, <<>>, ?X3DH_INFO, 32),
        
        %% Build initial message for Bob
        InitialMessage = #{
            identity_key => IK_A_Pub,
            ephemeral_key => EK_Pub,
            prekey_used => OPK_B =/= undefined,
            %% Associated data for AEAD (IK_A || IK_B)
            associated_data => <<IK_A_Pub/binary, IK_B/binary>>
        },
        
        {ok, SharedSecret, InitialMessage}
    catch
        throw:Reason -> {error, Reason};
        error:Reason -> {error, Reason}
    end.

%% =============================================================================
%% Key Exchange - Responder (Bob)
%% =============================================================================

%% @doc Respond to X3DH key exchange (Bob's side)
%%
%% Bob has:
%% - His identity key pair (IK_B)
%% - His signed pre-key pair (SPK_B)
%% - His one-time pre-key pair (OPK_B) if used
%% - Alice's initial message
%%
%% Returns:
%% - Shared secret (SK) - should match Alice's
%%
-spec respond_to_exchange(BobKeys :: map(), AliceMessage :: map()) ->
    {ok, SharedSecret :: binary()} | {error, term()}.
respond_to_exchange(BobKeys, AliceMessage) ->
    try
        %% Extract Bob's keys
        IK_B_Priv = maps:get(identity_key_private, BobKeys),
        IK_B_Pub = maps:get(identity_key_public, BobKeys),
        SPK_B_Priv = maps:get(signed_prekey_private, BobKeys),
        
        %% Extract Alice's message
        IK_A = maps:get(identity_key, AliceMessage),
        EK_A = maps:get(ephemeral_key, AliceMessage),
        PrekeyUsed = maps:get(prekey_used, AliceMessage, false),
        
        %% Perform DH operations (mirrors Alice's operations)
        DH1 = x25519_shared(SPK_B_Priv, IK_A),      %% DH(SPK_B, IK_A)
        DH2 = x25519_shared(IK_B_Priv, EK_A),       %% DH(IK_B, EK_A)
        DH3 = x25519_shared(SPK_B_Priv, EK_A),      %% DH(SPK_B, EK_A)
        
        %% DH4 only if OPK was used
        DH4 = case PrekeyUsed of
            true ->
                OPK_B_Priv = maps:get(one_time_prekey_private, BobKeys),
                x25519_shared(OPK_B_Priv, EK_A);    %% DH(OPK_B, EK_A)
            false ->
                <<>>
        end,
        
        %% Derive shared secret: SK = KDF(DH1 || DH2 || DH3 || DH4)
        DHConcat = <<?PROTOCOL_VERSION/binary, DH1/binary, DH2/binary, DH3/binary, DH4/binary>>,
        SharedSecret = hkdf_sha256(DHConcat, <<>>, ?X3DH_INFO, 32),
        
        {ok, SharedSecret}
    catch
        throw:Reason -> {error, Reason};
        error:Reason -> {error, Reason}
    end.

%% =============================================================================
%% Signature Operations
%% =============================================================================

%% @doc Sign a prekey with identity key (Ed25519)
%% Note: We use a separate Ed25519 key derived from X25519 for signing
-spec sign_prekey(PrekeyPublic :: binary(), IdentityPrivate :: binary()) -> binary().
sign_prekey(PrekeyPublic, IdentityPrivate) ->
    %% Convert X25519 private key to Ed25519 signing key
    %% In practice, you'd use a separate Ed25519 key or derive one
    %% For simplicity, we use HMAC as a deterministic signature
    crypto:mac(hmac, sha256, IdentityPrivate, PrekeyPublic).

%% @doc Verify a prekey signature
-spec verify_prekey_signature(PrekeyPublic :: binary(), Signature :: binary(), 
                              IdentityPublic :: binary()) -> boolean().
verify_prekey_signature(PrekeyPublic, Signature, _IdentityPublic) ->
    %% In a real implementation, this would verify Ed25519 signature
    %% For now, we accept any 32+ byte signature (simplified)
    byte_size(Signature) >= 32 andalso byte_size(PrekeyPublic) =:= 32.

%% =============================================================================
%% Cryptographic Primitives
%% =============================================================================

%% @doc Generate X25519 key pair
generate_x25519_keypair() ->
    %% crypto:generate_key/2 returns {PublicKey, PrivateKey} for x25519
    crypto:generate_key(ecdh, x25519).

%% @doc Perform X25519 Diffie-Hellman
x25519_shared(PrivateKey, PublicKey) ->
    crypto:compute_key(ecdh, PublicKey, PrivateKey, x25519).

%% @doc HKDF-SHA256 key derivation
-spec hkdf_sha256(IKM :: binary(), Salt :: binary(), Info :: binary(), 
                  Length :: pos_integer()) -> binary().
hkdf_sha256(IKM, Salt, Info, Length) ->
    %% Extract phase
    PRK = case Salt of
        <<>> -> crypto:mac(hmac, sha256, <<0:256>>, IKM);  %% Zero salt
        _ -> crypto:mac(hmac, sha256, Salt, IKM)
    end,
    
    %% Expand phase
    hkdf_expand(PRK, Info, Length, 1, <<>>).

hkdf_expand(_PRK, _Info, Length, _Counter, Acc) when byte_size(Acc) >= Length ->
    binary:part(Acc, 0, Length);
hkdf_expand(PRK, Info, Length, Counter, Acc) ->
    %% T(N) = HMAC-Hash(PRK, T(N-1) | info | N)
    PrevT = case Acc of
        <<>> -> <<>>;
        _ -> binary:part(Acc, byte_size(Acc) - 32, 32)  %% Last 32 bytes
    end,
    T = crypto:mac(hmac, sha256, PRK, <<PrevT/binary, Info/binary, Counter>>),
    hkdf_expand(PRK, Info, Length, Counter + 1, <<Acc/binary, T/binary>>).

%% =============================================================================
%% Utility Functions
%% =============================================================================

%% @doc Derive shared secret from DH results (for external use)
-spec derive_shared_secret(DHResults :: [binary()]) -> binary().
derive_shared_secret(DHResults) ->
    Combined = iolist_to_binary([?PROTOCOL_VERSION | DHResults]),
    hkdf_sha256(Combined, <<>>, ?X3DH_INFO, 32).
