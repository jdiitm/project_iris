-module(iris_auth).
-behaviour(gen_server).

%% =============================================================================
%% JWT Authentication Module
%% =============================================================================
%% Purpose: Validates JWT tokens for authenticated connections.
%% Design:
%% 1. Stateless JWT validation with configurable secret
%% 2. Token expiry validation
%% 3. User ID extraction from claims
%% 4. Revocation list support (optional)
%% =============================================================================

-export([start_link/0]).
-export([validate_token/1, validate_token/2]).
-export([create_token/1, create_token/2, create_token/3]).
-export([create_eddsa_token/1, create_eddsa_token/2, create_eddsa_token/3]). %% P1-4: EdDSA JWT
-export([revoke_token/1]).
-export([get_user_from_token/1]).
-export([is_auth_enabled/0]).
-export([get_eddsa_public_key/0]).  %% P1-4: EdDSA public key retrieval
-export([receive_revocation/2]).  %% P1-H2: Cross-node revocation propagation
%% IA-3: Refresh token API (RFC-001 v4.0 FR-11a)
-export([create_refresh_token/1, create_refresh_token/2, exchange_refresh_token/1]).
-export([validate_and_rotate_refresh/1]).  %% Mnesia-only validation (for cross-node RPC)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(REVOCATION_TABLE, iris_auth_revoked).
-define(DEFAULT_TTL, 86400).  %% 24 hours in seconds

%% RFC Section 10.1: Failed login rate limiting (10/hour per account)
-define(FAILED_LOGIN_TABLE, iris_auth_failed_logins).
-define(FAILED_LOGIN_MAX, 10).              %% Max 10 failures per window
-define(FAILED_LOGIN_WINDOW_SECS, 3600).    %% 1-hour window
-define(JTI_SEEN_TABLE, iris_auth_jti_seen).  %% RFC Section 9.1: JWT replay protection (GAP-15)

-record(state, {
    secret :: binary(),
    issuer :: binary(),
    revoked_count = 0 :: integer(),
    eddsa_pub :: binary() | undefined,    %% P1-4: Ed25519 public key
    eddsa_priv :: binary() | undefined,   %% P1-4: Ed25519 private key
    auth_mode = signer :: signer | verifier  %% RFC Section 9.1: Key isolation
}).

%% @doc Get current auth mode (signer or verifier).
-export([get_auth_mode/0]).
%% RFC Section 10.1: Failed login rate limiting
-export([check_login_rate/1, record_failed_login/1]).
-export([revoke_refresh_family/1]).  %% B-7/H-6: Exported for testability

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Check if authentication is enabled
-spec is_auth_enabled() -> boolean().
is_auth_enabled() ->
    %% AUDIT MITIGATION P0-2: Default to true (secure by default).
    %% Test configs explicitly set auth_enabled=false.
    application:get_env(iris_edge, auth_enabled, true).

%% @doc Validate a JWT token. Returns {ok, Claims} or {error, Reason}.
-spec validate_token(binary()) -> {ok, map()} | {error, term()}.
validate_token(Token) ->
    validate_token(Token, #{}).

-spec validate_token(binary(), map()) -> {ok, map()} | {error, term()}.
validate_token(Token, Opts) ->
    gen_server:call(?SERVER, {validate, Token, Opts}).

%% @doc Create a JWT token for a user with default claims.
-spec create_token(binary()) -> {ok, binary()} | {error, term()}.
create_token(UserId) ->
    create_token(UserId, #{}).

%% @doc Create a JWT token for a user with custom claims.
-spec create_token(binary(), map()) -> {ok, binary()} | {error, term()}.
create_token(UserId, Claims) ->
    create_token(UserId, Claims, ?DEFAULT_TTL).

-spec create_token(binary(), map(), integer()) -> {ok, binary()} | {error, term()}.
create_token(UserId, Claims, TTL) ->
    gen_server:call(?SERVER, {create, UserId, Claims, TTL}).

%% @doc Create an EdDSA-signed JWT token (P1-4: RFC-001 v4.0 Section 6.3).
-spec create_eddsa_token(binary()) -> {ok, binary()} | {error, term()}.
create_eddsa_token(UserId) ->
    create_eddsa_token(UserId, #{}).

-spec create_eddsa_token(binary(), map()) -> {ok, binary()} | {error, term()}.
create_eddsa_token(UserId, Claims) ->
    create_eddsa_token(UserId, Claims, ?DEFAULT_TTL).

-spec create_eddsa_token(binary(), map(), integer()) -> {ok, binary()} | {error, term()}.
create_eddsa_token(UserId, Claims, TTL) ->
    gen_server:call(?SERVER, {create_eddsa, UserId, Claims, TTL}).

%% @doc Get the EdDSA public key for token verification.
-spec get_eddsa_public_key() -> {ok, binary()} | {error, no_eddsa_key}.
get_eddsa_public_key() ->
    gen_server:call(?SERVER, get_eddsa_public_key).

%% @doc Get current auth mode (signer or verifier).
%% RFC Section 9.1: Only auth service holds private key.
-spec get_auth_mode() -> signer | verifier.
get_auth_mode() ->
    gen_server:call(?SERVER, get_auth_mode).

%% @doc Revoke a token by its JTI (extracted from token).
-spec revoke_token(binary()) -> ok | {error, term()}.
revoke_token(Token) ->
    case get_jti_from_token(Token) of
        {ok, Jti} -> gen_server:call(?SERVER, {revoke, Jti});
        Error -> Error
    end.

get_jti_from_token(Token) ->
    case decode_payload(Token) of
        {ok, Claims} ->
            case maps:get(<<"jti">>, Claims, undefined) of
                undefined -> {error, no_jti};
                Jti -> {ok, Jti}
            end;
        Error -> Error
    end.

%% @doc Extract user ID from a validated token (without full validation).
-spec get_user_from_token(binary()) -> {ok, binary()} | {error, term()}.
get_user_from_token(Token) ->
    case decode_payload(Token) of
        {ok, Claims} ->
            case maps:get(<<"sub">>, Claims, undefined) of
                undefined -> {error, no_subject};
                UserId -> {ok, UserId}
            end;
        Error -> Error
    end.

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    %% P0-C4 FIX: Require explicit JWT secret configuration
    %% Random secrets cause auth failures when users connect to different nodes
    %% AUDIT FIX: Check IRIS_JWT_SECRET env var first (secrets management).
    %% This allows operators to inject secrets via environment without config files.
    Secret = case os:getenv("IRIS_JWT_SECRET") of
        EnvVal when is_list(EnvVal), length(EnvVal) >= 32 ->
            logger:info("JWT secret loaded from IRIS_JWT_SECRET environment variable"),
            list_to_binary(EnvVal);
        _ ->
            case application:get_env(iris_edge, jwt_secret) of
                {ok, S} when is_binary(S), byte_size(S) >= 32 -> 
                    S;
                {ok, S} when is_list(S), length(S) >= 32 -> 
                    list_to_binary(S);
                {ok, S} when is_binary(S) ->
                    logger:error("SECURITY: jwt_secret is too short (~p bytes). Minimum 32 bytes required.", 
                                [byte_size(S)]),
                    error({jwt_secret_too_short, byte_size(S)});
                {ok, S} when is_list(S) ->
                    logger:error("SECURITY: jwt_secret is too short (~p chars). Minimum 32 chars required.", 
                                [length(S)]),
                    error({jwt_secret_too_short, length(S)});
                undefined -> 
                    %% P0-C4: Strict enforcement based on allow_random_secret flag
                    case application:get_env(iris_edge, allow_random_secret, false) of
                        true ->
                            logger:warning("JWT secret not configured, generating random (NOT FOR PRODUCTION)"),
                            logger:warning("Set iris_edge.jwt_secret or IRIS_JWT_SECRET env var"),
                            generate_secret();
                        false ->
                            logger:error("======================================================="),
                            logger:error("FATAL: jwt_secret not configured!"),
                            logger:error(""),
                            logger:error("In production: Set IRIS_JWT_SECRET env var (preferred)"),
                            logger:error("            or iris_edge.jwt_secret to a 32+ byte secret"),
                            logger:error("For testing:   Set iris_edge.allow_random_secret = true"),
                            logger:error(""),
                            logger:error("Random secrets cause authentication failures when"),
                            logger:error("users connect to different nodes in a cluster."),
                            logger:error("======================================================="),
                            error(jwt_secret_not_configured)
                    end
            end
    end,
    
    Issuer = case application:get_env(iris_edge, jwt_issuer) of
        {ok, I} -> list_to_binary(I);
        undefined -> <<"iris">>
    end,
    
    %% Create revocation table
    ets:new(?REVOCATION_TABLE, [set, named_table, public, {read_concurrency, true}]),
    
    %% RFC Section 10.1: Failed login tracking table
    %% Format: {UserId, FailedCount, WindowStartTimestamp}
    ets:new(?FAILED_LOGIN_TABLE, [set, named_table, public,
                                   {read_concurrency, true},
                                   {write_concurrency, true}]),
    
    %% AUDIT M7: Create JTI replay table eagerly to prevent race on first use
    ets:new(?JTI_SEEN_TABLE, [named_table, public, set,
                              {read_concurrency, true}]),
    
    %% Schedule cleanup of expired revocations
    erlang:send_after(3600000, self(), cleanup_revocations),
    
    %% P1-4: Generate or load EdDSA key pair
    EdDSAResult = case application:get_env(iris_edge, jwt_eddsa_private_key) of
        {ok, PrivKeyBin} when is_binary(PrivKeyBin), byte_size(PrivKeyBin) =:= 32 ->
            %% Derive public key from private key
            PubKey = crypto:generate_key(eddsa, ed25519, PrivKeyBin),
            case PubKey of
                {Pub, _Priv} -> {configured, Pub, PrivKeyBin};
                _ -> {configured, undefined, undefined}
            end;
        _ ->
            %% Generate ephemeral key pair (for testing/non-production)
            {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
            logger:info("JWT: Generated ephemeral EdDSA key pair (set jwt_eddsa_private_key for persistence)"),
            {ephemeral, Pub, Priv}
    end,

    %% AUDIT 2.1a FIX: Reject ephemeral keys when auth_enabled=true
    %% Ephemeral keys cause thundering herd on restart: all tokens become invalid,
    %% 100% of clients disconnect and re-login simultaneously.
    case {EdDSAResult, application:get_env(iris_edge, auth_enabled, false)} of
        {{ephemeral, _, _}, true} ->
            logger:error("FATAL: auth_enabled=true but jwt_eddsa_private_key not configured. "
                         "Ephemeral keys invalidate all tokens on restart, causing thundering herd. "
                         "Set jwt_eddsa_private_key in config."),
            {stop, {misconfiguration, ephemeral_key_with_auth_enabled}};
        {{_, EdDSAPub, EdDSAPriv}, _} ->
            %% RFC Section 9.1: Auth mode determines key isolation
            %% signer = holds private key, can create and validate tokens (auth service)
            %% verifier = public key only, can validate but not create EdDSA tokens (edge/core)
            AuthMode = application:get_env(iris_edge, auth_mode, signer),

            %% In verifier mode, discard the private key for security
            {FinalPub, FinalPriv, FinalMode} = case AuthMode of
                verifier ->
                    logger:info("JWT: Running in VERIFIER mode (no EdDSA private key)"),
                    {EdDSAPub, undefined, verifier};
                _ ->
                    {EdDSAPub, EdDSAPriv, signer}
            end,

            logger:info("JWT auth initialized (issuer: ~s, eddsa: ~s, mode: ~s)",
                        [Issuer, case FinalPub of undefined -> "disabled"; _ -> "enabled" end, FinalMode]),
            {ok, #state{secret = Secret, issuer = Issuer, eddsa_pub = FinalPub,
                        eddsa_priv = FinalPriv, auth_mode = FinalMode}}
    end.

handle_call({validate, Token, Opts}, _From, State) ->
    Result = do_validate(Token, Opts, State),
    {reply, Result, State};

handle_call({create, UserId, ExtraClaims, TTL}, _From, State) ->
    Result = do_create_token(UserId, ExtraClaims, TTL, State),
    {reply, Result, State};

handle_call({revoke, TokenId}, _From, State = #state{revoked_count = Count}) ->
    Now = os:system_time(second),
    %% P1-H2 FIX: Synchronous revocation with cross-node propagation
    %% 1. Store in local ETS (immediate effect on this node)
    ets:insert(?REVOCATION_TABLE, {TokenId, Now}),
    
    %% 2. Persist to Mnesia synchronously (distributed durability)
    case persist_revocation_sync(TokenId, Now) of
        ok ->
            %% 3. Push to other nodes for immediate effect (don't wait for Mnesia sync)
            propagate_revocation(TokenId, Now);
        {error, Reason} ->
            logger:warning("Revocation persistence failed: ~p (local ETS still valid)", [Reason])
    end,
    
    {reply, ok, State#state{revoked_count = Count + 1}};

handle_call({create_eddsa, _UserId, _ExtraClaims, _TTL}, _From, State = #state{auth_mode = verifier}) ->
    %% RFC Section 9.1: Verifier mode cannot create EdDSA tokens
    {reply, {error, verifier_mode_no_signing}, State};

handle_call({create_eddsa, UserId, ExtraClaims, TTL}, _From, State) ->
    Result = do_create_eddsa_token(UserId, ExtraClaims, TTL, State),
    {reply, Result, State};

handle_call(get_eddsa_public_key, _From, State = #state{eddsa_pub = Pub}) ->
    case Pub of
        undefined -> {reply, {error, no_eddsa_key}, State};
        _ -> {reply, {ok, Pub}, State}
    end;

handle_call(get_auth_mode, _From, State = #state{auth_mode = Mode}) ->
    {reply, Mode, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_revocations, State) ->
    %% Remove expired revocations (older than 24 hours)
    Now = os:system_time(second),
    Cutoff = Now - 86400,
    cleanup_revoked(Cutoff),
    %% Also clean expired failed-login windows
    cleanup_failed_logins(Now),
    %% RFC Section 9.1: Clean expired jti nonce entries (GAP-15)
    cleanup_jti_seen(Now),
    erlang:send_after(3600000, self(), cleanup_revocations),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% RFC Section 10.1: Failed Login Rate Limiting
%% =============================================================================

%% @doc Check if a user is allowed to attempt login.
%% Returns ok | {error, rate_limited}.
%% Gracefully returns ok if the ETS table doesn't exist (iris_auth not started).
-spec check_login_rate(binary()) -> ok | {error, rate_limited}.
check_login_rate(UserId) ->
    try
        Now = os:system_time(second),
        MaxFails = application:get_env(iris_core, failed_login_max, ?FAILED_LOGIN_MAX),
        WindowSecs = application:get_env(iris_core, failed_login_window_secs, ?FAILED_LOGIN_WINDOW_SECS),
        case ets:lookup(?FAILED_LOGIN_TABLE, UserId) of
            [] ->
                ok;
            [{UserId, Count, WindowStart}] ->
                case (Now - WindowStart) > WindowSecs of
                    true ->
                        %% Window expired, reset
                        ets:delete(?FAILED_LOGIN_TABLE, UserId),
                        ok;
                    false ->
                        case Count >= MaxFails of
                            true ->
                                {error, rate_limited};
                            false ->
                                ok
                        end
                end
        end
    catch
        error:badarg ->
            %% ETS table doesn't exist (iris_auth not started yet)
            ok
    end.

%% @doc Record a failed login attempt for a user.
%% Gracefully no-ops if the ETS table doesn't exist.
-spec record_failed_login(binary()) -> ok.
record_failed_login(UserId) ->
    try
        Now = os:system_time(second),
        WindowSecs = application:get_env(iris_core, failed_login_window_secs, ?FAILED_LOGIN_WINDOW_SECS),
        case ets:lookup(?FAILED_LOGIN_TABLE, UserId) of
            [] ->
                ets:insert(?FAILED_LOGIN_TABLE, {UserId, 1, Now});
            [{UserId, Count, WindowStart}] ->
                case (Now - WindowStart) > WindowSecs of
                    true ->
                        %% Window expired, start new window
                        ets:insert(?FAILED_LOGIN_TABLE, {UserId, 1, Now});
                    false ->
                        %% Increment counter within window
                        ets:insert(?FAILED_LOGIN_TABLE, {UserId, Count + 1, WindowStart})
                end
        end,
        ok
    catch
        error:badarg ->
            %% ETS table doesn't exist (iris_auth not started)
            ok
    end.

%% @doc Clean up expired failed-login windows.
cleanup_failed_logins(Now) ->
    WindowSecs = application:get_env(iris_core, failed_login_window_secs, ?FAILED_LOGIN_WINDOW_SECS),
    Cutoff = Now - WindowSecs,
    %% Use ets:foldl to collect expired entries, then delete them
    Expired = ets:foldl(
        fun({UserId, _Count, WindowStart}, Acc) ->
            case WindowStart < Cutoff of
                true -> [UserId | Acc];
                false -> Acc
            end
        end, [], ?FAILED_LOGIN_TABLE),
    lists:foreach(fun(Id) -> ets:delete(?FAILED_LOGIN_TABLE, Id) end, Expired),
    ok.

%% =============================================================================
%% Internal: JTI Replay Protection Cleanup (GAP-15)
%% =============================================================================

cleanup_jti_seen(Now) ->
    try
        case ets:info(?JTI_SEEN_TABLE) of
            undefined -> ok;
            _ ->
                %% Delete entries where token expiry has passed
                Expired = ets:foldl(
                    fun({Jti, Exp}, Acc) ->
                        case Exp < Now of
                            true -> [Jti | Acc];
                            false -> Acc
                        end
                    end, [], ?JTI_SEEN_TABLE),
                lists:foreach(fun(Id) -> ets:delete(?JTI_SEEN_TABLE, Id) end, Expired),
                ok
        end
    catch
        _:_ -> ok
    end.

%% =============================================================================
%% Internal: JWT Validation
%% =============================================================================

do_validate(Token, Opts, State = #state{secret = Secret, issuer = ExpectedIssuer}) ->
    case split_token(Token) of
        {ok, Header, Payload, Signature} ->
            %% Determine algorithm from header
            case get_header_alg(Header) of
                {error, invalid_header} ->
                    {error, invalid_header};
                Alg ->
            
            %% AUDIT: Algorithm whitelist — reject before signature verification
            AllowedAlgs = [<<"HS256">>, <<"EdDSA">>],
            case lists:member(Alg, AllowedAlgs) of
                false ->
                    {error, unsupported_algorithm};
                true ->
            
            SigningInput = <<Header/binary, ".", Payload/binary>>,
            
            %% IA-1: Check HMAC deprecation flag before validation
            HmacAllowed = application:get_env(iris_edge, allow_hmac_jwt, false),
            SigValid = case Alg of
                <<"EdDSA">> ->
                    %% P1-4: EdDSA verification
                    verify_eddsa_signature(SigningInput, Signature, State);
                _ when HmacAllowed =:= false ->
                    %% IA-1: HMAC deprecated - reject
                    hmac_deprecated;
                _ ->
                    %% Default: HMAC-SHA256
                    ExpectedSig = compute_signature(SigningInput, Secret),
                    constant_time_compare(Signature, ExpectedSig)
            end,
            
            case SigValid of
                true ->
                    %% Decode and validate claims
                    case decode_base64url(Payload) of
                        {ok, ClaimsJson} ->
                            case decode_json(ClaimsJson) of
                                {ok, Claims} ->
                                    validate_claims(Claims, ExpectedIssuer, Opts);
                                Error -> Error
                            end;
                        Error -> Error
                    end;
                hmac_deprecated ->
                    {error, hmac_deprecated};
                false ->
                    {error, invalid_signature}
            end
            end  %% end of AllowedAlgs check
            end;  %% end of get_header_alg case
        Error -> Error
    end.

validate_claims(Claims, ExpectedIssuer, Opts) ->
    Now = os:system_time(second),
    
    %% Check expiry
    Exp = maps:get(<<"exp">>, Claims, 0),
    case Exp < Now of
        true -> {error, token_expired};
        false ->
            %% Check issuer
            Iss = maps:get(<<"iss">>, Claims, <<>>),
            case Iss == ExpectedIssuer of
                false -> {error, invalid_issuer};
                true ->
                    %% Check not-before
                    Nbf = maps:get(<<"nbf">>, Claims, 0),
                    case Nbf > Now of
                        true -> {error, token_not_yet_valid};
                        false ->
                            %% Check revocation
                            Jti = maps:get(<<"jti">>, Claims, undefined),
                            case Jti =/= undefined andalso is_revoked(Jti) of
                                true -> {error, token_revoked};
                                false ->
                                    %% RFC Section 9.1: Replay protection via jti nonce (GAP-15)
                                    case Jti =/= undefined andalso check_jti_replay(Jti, Exp) of
                                        true -> {error, token_replayed};
                                        false -> {ok, Claims}
                                    end
                            end
                    end
            end
    end.

%% @doc Check if a jti has been seen before (replay attack detection) (GAP-15)
%% RFC Section 9.1: "Replay attacks: Nonce + timestamp validation"
%% Tracks seen jti values in ETS with TTL = token expiry time.
%% AUDIT M7: Table is now created eagerly in init/1, no lazy creation needed.
check_jti_replay(Jti, Exp) ->
    case ets:lookup(?JTI_SEEN_TABLE, Jti) of
        [{Jti, _}] ->
            %% Already seen -- this is a replay
            true;
        [] ->
            %% First use -- mark as seen with expiry for cleanup
            ets:insert(?JTI_SEEN_TABLE, {Jti, Exp}),
            false
    end.

is_revoked(TokenId) ->
    %% P0-4 FIX: Check local ETS first (fast), then Mnesia (distributed)
    case ets:member(?REVOCATION_TABLE, TokenId) of
        true -> true;
        false ->
            %% Check Mnesia for revocations from other nodes
            case mnesia:dirty_read(revoked_tokens, TokenId) of
                [] -> false;
                [_|_] -> 
                    %% Cache locally for fast subsequent checks
                    Now = os:system_time(second),
                    ets:insert(?REVOCATION_TABLE, {TokenId, Now}),
                    true
            end
    end.

%% P1-H2 FIX: Synchronous revocation persistence with proper error handling
persist_revocation_sync(TokenId, Timestamp) ->
    try
        F = fun() -> mnesia:write({revoked_tokens, TokenId, Timestamp}) end,
        case mnesia:activity(sync_transaction, F) of
            ok -> ok;
            {atomic, _} -> ok;
            {aborted, Reason} ->
                logger:warning("Failed to persist revocation: ~p", [Reason]),
                {error, Reason}
        end
    catch
        _:Error ->
            logger:warning("Revocation persistence error: ~p", [Error]),
            {error, Error}
    end.

%% P1-H2 FIX: Push revocation to all cluster nodes for immediate effect
%% This ensures revocation takes effect within ~60s across all nodes (RFC FR-11)
%% AUDIT 2.1b FIX: Use rpc:call (not rpc:cast) with timeout so failures are
%% detected and logged. Spawn wrapper kept to avoid blocking the gen_server.
propagate_revocation(TokenId, Timestamp) ->
    %% Get all connected nodes
    Nodes = nodes(),
    case Nodes of
        [] -> ok;  %% Single node deployment
        _ ->
            spawn(fun() ->
                lists:foreach(fun(Node) ->
                    case rpc:call(Node, ?MODULE, receive_revocation,
                                  [TokenId, Timestamp], 2000) of
                        ok -> ok;
                        {badrpc, Reason} ->
                            logger:warning("Revocation propagation to ~p failed: ~p "
                                           "(Mnesia will sync eventually)", [Node, Reason])
                    end
                end, Nodes)
            end)
    end.

%% P1-H2 FIX: Receive revocation push from another node
-spec receive_revocation(binary(), integer()) -> ok.
receive_revocation(TokenId, Timestamp) ->
    %% Insert into local ETS for immediate effect
    ets:insert(?REVOCATION_TABLE, {TokenId, Timestamp}),
    ok.

%% =============================================================================
%% Internal: JWT Creation
%% =============================================================================

do_create_token(UserId, ExtraClaims, TTL, #state{secret = Secret, issuer = Issuer}) ->
    Now = os:system_time(second),
    Jti = generate_jti(),
    
    Claims = maps:merge(ExtraClaims, #{
        <<"sub">> => UserId,
        <<"iss">> => Issuer,
        <<"iat">> => Now,
        <<"exp">> => Now + TTL,
        <<"jti">> => Jti
    }),
    
    Header = #{<<"alg">> => <<"HS256">>, <<"typ">> => <<"JWT">>},
    
    HeaderB64 = encode_base64url(encode_json(Header)),
    PayloadB64 = encode_base64url(encode_json(Claims)),
    SigningInput = <<HeaderB64/binary, ".", PayloadB64/binary>>,
    Signature = compute_signature(SigningInput, Secret),
    
    Token = <<SigningInput/binary, ".", Signature/binary>>,
    {ok, Token}.

%% =============================================================================
%% Internal: EdDSA Token Creation (P1-4)
%% =============================================================================

do_create_eddsa_token(UserId, ExtraClaims, TTL, #state{issuer = Issuer, eddsa_priv = PrivKey}) ->
    case PrivKey of
        undefined -> {error, no_eddsa_key};
        _ ->
            Now = os:system_time(second),
            Jti = generate_jti(),
            Claims = maps:merge(ExtraClaims, #{
                <<"sub">> => UserId,
                <<"iss">> => Issuer,
                <<"iat">> => Now,
                <<"exp">> => Now + TTL,
                <<"jti">> => Jti
            }),
            Header = #{<<"alg">> => <<"EdDSA">>, <<"typ">> => <<"JWT">>},
            HeaderB64 = encode_base64url(encode_json(Header)),
            PayloadB64 = encode_base64url(encode_json(Claims)),
            SigningInput = <<HeaderB64/binary, ".", PayloadB64/binary>>,
            Sig = crypto:sign(eddsa, none, SigningInput, [PrivKey, ed25519]),
            SigB64 = encode_base64url(Sig),
            Token = <<SigningInput/binary, ".", SigB64/binary>>,
            {ok, Token}
    end.

%% =============================================================================
%% Internal: Crypto Helpers
%% =============================================================================

compute_signature(Input, Secret) ->
    %% HMAC-SHA256
    Mac = crypto:mac(hmac, sha256, Secret, Input),
    encode_base64url(Mac).

%% P1-4: Extract algorithm from JWT header
%% AUDIT M2: Reject on decode failure instead of defaulting to HS256.
%% A garbage header must not silently bypass algorithm selection.
get_header_alg(HeaderB64) ->
    case decode_base64url(HeaderB64) of
        {ok, Json} ->
            case decode_json(Json) of
                {ok, Map} -> maps:get(<<"alg">>, Map, <<"HS256">>);
                _ -> {error, invalid_header}
            end;
        _ -> {error, invalid_header}
    end.

%% P1-4: Verify EdDSA signature
verify_eddsa_signature(SigningInput, SigB64, #state{eddsa_pub = PubKey}) ->
    case PubKey of
        undefined -> false;
        _ ->
            case decode_base64url(SigB64) of
                {ok, SigBytes} ->
                    crypto:verify(eddsa, none, SigningInput, SigBytes, [PubKey, ed25519]);
                _ -> false
            end
    end.

generate_secret() ->
    %% Generate 32-byte random secret
    Bytes = crypto:strong_rand_bytes(32),
    logger:warning("JWT: Generated random secret. Set jwt_secret in config for persistence."),
    Bytes.

generate_jti() ->
    %% Unique token identifier
    Bytes = crypto:strong_rand_bytes(16),
    encode_base64url(Bytes).

constant_time_compare(A, B) when byte_size(A) =/= byte_size(B) ->
    false;
constant_time_compare(A, B) ->
    constant_time_compare(A, B, 0).

constant_time_compare(<<>>, <<>>, Acc) ->
    Acc == 0;
constant_time_compare(<<A:8, RestA/binary>>, <<B:8, RestB/binary>>, Acc) ->
    constant_time_compare(RestA, RestB, Acc bor (A bxor B)).

%% =============================================================================
%% Internal: Encoding/Decoding
%% =============================================================================

split_token(Token) ->
    case binary:split(Token, <<".">>, [global]) of
        [Header, Payload, Signature] ->
            {ok, Header, Payload, Signature};
        _ ->
            {error, malformed_token}
    end.

decode_payload(Token) ->
    case split_token(Token) of
        {ok, _Header, Payload, _Sig} ->
            case decode_base64url(Payload) of
                {ok, Json} -> decode_json(Json);
                Error -> Error
            end;
        Error -> Error
    end.

encode_base64url(Bin) ->
    B64 = base64:encode(Bin),
    %% Convert to URL-safe and strip padding
    B64_1 = binary:replace(B64, <<"+">>, <<"-">>, [global]),
    B64_2 = binary:replace(B64_1, <<"/">>, <<"_">>, [global]),
    binary:replace(B64_2, <<"=">>, <<>>, [global]).

decode_base64url(Bin) ->
    try
        %% Add padding if needed
        PadLen = (4 - (byte_size(Bin) rem 4)) rem 4,
        Padded = <<Bin/binary, (binary:copy(<<"=">>, PadLen))/binary>>,
        %% Convert from URL-safe
        B64_1 = binary:replace(Padded, <<"-">>, <<"+">>, [global]),
        B64_2 = binary:replace(B64_1, <<"_">>, <<"/">>, [global]),
        {ok, base64:decode(B64_2)}
    catch
        _:_ -> {error, invalid_base64}
    end.

%% AUDIT FIX (Finding 1.2): Delegated to iris_auth_json module.
%% Old inline parser used O(N^2) binary append; new module uses iolists (O(N)).
encode_json(Map) -> iris_auth_json:encode(Map).
decode_json(Bin) -> iris_auth_json:decode(Bin).

%% =============================================================================
%% Internal: Cleanup
%% =============================================================================

cleanup_revoked(Cutoff) ->
    cleanup_fold(ets:first(?REVOCATION_TABLE), Cutoff).

cleanup_fold('$end_of_table', _Cutoff) ->
    ok;
cleanup_fold(Key, Cutoff) ->
    Next = ets:next(?REVOCATION_TABLE, Key),
    case ets:lookup(?REVOCATION_TABLE, Key) of
        [{Key, Timestamp}] when Timestamp < Cutoff ->
            ets:delete(?REVOCATION_TABLE, Key);
        _ -> ok
    end,
    cleanup_fold(Next, Cutoff).

%% =============================================================================
%% IA-3: Refresh Token Implementation (RFC-001 v4.0 FR-11a)
%% =============================================================================

-define(REFRESH_TABLE, refresh_tokens).
-define(REFRESH_TTL, 2592000).  %% 30 days in seconds

-spec create_refresh_token(binary()) -> {ok, binary()} | {error, term()}.
create_refresh_token(UserId) ->
    create_refresh_token(UserId, ?REFRESH_TTL).

-spec create_refresh_token(binary(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
create_refresh_token(UserId, TTL) ->
    TokenId = base64:encode(crypto:strong_rand_bytes(32)),
    FamilyId = base64:encode(crypto:strong_rand_bytes(16)),
    Now = os:system_time(second),
    ExpiresAt = Now + TTL,
    Record = {?REFRESH_TABLE, TokenId, UserId, FamilyId, false, Now, ExpiresAt},
    %% AUDIT P0-4: sync_transaction for refresh token durability
    try
        {atomic, ok} = mnesia:sync_transaction(fun() ->
            mnesia:write(Record)
        end),
        {ok, TokenId}
    catch
        _:Reason -> {error, Reason}
    end.

-spec exchange_refresh_token(binary()) -> {ok, binary(), binary()} | {error, term()}.
exchange_refresh_token(TokenId) ->
    Now = os:system_time(second),
    case mnesia:dirty_read(?REFRESH_TABLE, TokenId) of
        [] ->
            {error, token_reused};
        [{?REFRESH_TABLE, TokenId, UserId, FamilyId, Used, _CreatedAt, ExpiresAt}] ->
            case ExpiresAt =< Now of
                true ->
                    {error, refresh_expired};
                false ->
                    case Used of
                        true ->
                            %% Token reuse detected - revoke entire family
                            revoke_refresh_family(FamilyId),
                            {error, token_reused};
                        false ->
                            %% AUDIT P0-4: Mark as used with transaction
                            {atomic, ok} = mnesia:sync_transaction(fun() ->
                                mnesia:write({?REFRESH_TABLE, TokenId, UserId, FamilyId, true, Now, ExpiresAt})
                            end),
                            %% Create new tokens
                            {ok, NewAccess} = create_token(UserId),
                            {ok, NewRefresh} = create_refresh_token_in_family(UserId, FamilyId),
                            {ok, NewAccess, NewRefresh}
                    end
            end
    end.

%% @doc Validate refresh token and rotate (mnesia-only, no gen_server dependency).
%% Returns {ok, UserId, NewRefreshToken} so the caller can create access tokens locally.
-spec validate_and_rotate_refresh(binary()) -> {ok, binary(), binary()} | {error, term()}.
validate_and_rotate_refresh(TokenId) ->
    Now = os:system_time(second),
    case mnesia:dirty_read(?REFRESH_TABLE, TokenId) of
        [] ->
            {error, token_reused};
        [{?REFRESH_TABLE, TokenId, UserId, FamilyId, Used, _CreatedAt, ExpiresAt}] ->
            case ExpiresAt =< Now of
                true ->
                    {error, refresh_expired};
                false ->
                    case Used of
                        true ->
                            revoke_refresh_family(FamilyId),
                            {error, token_reused};
                        false ->
                            %% AUDIT P0-4: Mark as used with transaction
                            {atomic, ok} = mnesia:sync_transaction(fun() ->
                                mnesia:write({?REFRESH_TABLE, TokenId, UserId, FamilyId, true, Now, ExpiresAt})
                            end),
                            {ok, NewRefresh} = create_refresh_token_in_family(UserId, FamilyId),
                            {ok, UserId, NewRefresh}
                    end
            end
    end.

create_refresh_token_in_family(UserId, FamilyId) ->
    TokenId = base64:encode(crypto:strong_rand_bytes(32)),
    Now = os:system_time(second),
    ExpiresAt = Now + ?REFRESH_TTL,
    Record = {?REFRESH_TABLE, TokenId, UserId, FamilyId, false, Now, ExpiresAt},
    %% AUDIT P0-4: sync_transaction for refresh token durability
    {atomic, ok} = mnesia:sync_transaction(fun() ->
        mnesia:write(Record)
    end),
    {ok, TokenId}.

revoke_refresh_family(FamilyId) ->
    %% AUDIT P0-4: Transaction for family revocation durability
    %% H-6 AUDIT MITIGATION: Do not silently swallow errors.
    %% A failed revocation means stolen tokens may remain valid.
    try
        {atomic, ok} = mnesia:sync_transaction(fun() ->
            AllTokens = mnesia:match_object(?REFRESH_TABLE,
                {?REFRESH_TABLE, '_', '_', FamilyId, '_', '_', '_'}, write),
            lists:foreach(fun({?REFRESH_TABLE, TId, UId, FId, _Used, CAt, EAt}) ->
                mnesia:write({?REFRESH_TABLE, TId, UId, FId, true, CAt, EAt})
            end, AllTokens)
        end),
        ok
    catch
        Class:Reason ->
            logger:warning("revoke_refresh_family failed for ~p: ~p:~p",
                           [FamilyId, Class, Reason]),
            {error, {revocation_failed, Reason}}
    end.
