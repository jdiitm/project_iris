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
-export([revoke_token/1]).
-export([get_user_from_token/1]).
-export([is_auth_enabled/0]).
-export([receive_revocation/2]).  %% P1-H2: Cross-node revocation propagation
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(REVOCATION_TABLE, iris_auth_revoked).
-define(DEFAULT_TTL, 86400).  %% 24 hours in seconds

-record(state, {
    secret :: binary(),
    issuer :: binary(),
    revoked_count = 0 :: integer()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Check if authentication is enabled
-spec is_auth_enabled() -> boolean().
is_auth_enabled() ->
    application:get_env(iris_edge, auth_enabled, false).

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
    Secret = case application:get_env(iris_edge, jwt_secret) of
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
                    logger:warning("Set iris_edge.jwt_secret in production deployments"),
                    generate_secret();
                false ->
                    %% P0-C4 FIX: CRASH on startup if secret not configured
                    %% This prevents silent auth failures in production
                    logger:error("======================================================="),
                    logger:error("FATAL: jwt_secret not configured!"),
                    logger:error(""),
                    logger:error("In production: Set iris_edge.jwt_secret to a 32+ byte secret"),
                    logger:error("For testing:   Set iris_edge.allow_random_secret = true"),
                    logger:error(""),
                    logger:error("Random secrets cause authentication failures when"),
                    logger:error("users connect to different nodes in a cluster."),
                    logger:error("======================================================="),
                    error(jwt_secret_not_configured)
            end
    end,
    
    Issuer = case application:get_env(iris_edge, jwt_issuer) of
        {ok, I} -> list_to_binary(I);
        undefined -> <<"iris">>
    end,
    
    %% Create revocation table
    ets:new(?REVOCATION_TABLE, [set, named_table, public, {read_concurrency, true}]),
    
    %% Schedule cleanup of expired revocations
    erlang:send_after(3600000, self(), cleanup_revocations),
    
    logger:info("JWT auth initialized (issuer: ~s)", [Issuer]),
    {ok, #state{secret = Secret, issuer = Issuer}}.

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

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_revocations, State) ->
    %% Remove expired revocations (older than 24 hours)
    Now = os:system_time(second),
    Cutoff = Now - 86400,
    cleanup_revoked(Cutoff),
    erlang:send_after(3600000, self(), cleanup_revocations),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal: JWT Validation
%% =============================================================================

do_validate(Token, Opts, #state{secret = Secret, issuer = ExpectedIssuer}) ->
    case split_token(Token) of
        {ok, Header, Payload, Signature} ->
            %% Verify signature
            SigningInput = <<Header/binary, ".", Payload/binary>>,
            ExpectedSig = compute_signature(SigningInput, Secret),
            
            case constant_time_compare(Signature, ExpectedSig) of
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
                false ->
                    {error, invalid_signature}
            end;
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
                                false -> {ok, Claims}
                            end
                    end
            end
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
propagate_revocation(TokenId, Timestamp) ->
    %% Get all connected nodes
    Nodes = nodes(),
    case Nodes of
        [] -> ok;  %% Single node deployment
        _ ->
            %% Async push to all nodes (fire and forget, Mnesia is source of truth)
            spawn(fun() ->
                lists:foreach(fun(Node) ->
                    try
                        rpc:cast(Node, ?MODULE, receive_revocation, [TokenId, Timestamp])
                    catch
                        _:_ -> ok  %% Ignore errors, Mnesia will sync eventually
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
%% Internal: Crypto Helpers
%% =============================================================================

compute_signature(Input, Secret) ->
    %% HMAC-SHA256
    Mac = crypto:mac(hmac, sha256, Secret, Input),
    encode_base64url(Mac).

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

%% Simple JSON encoding/decoding (minimal implementation)
encode_json(Map) when is_map(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        KEnc = encode_json_value(K),
        VEnc = encode_json_value(V),
        [<<KEnc/binary, ":", VEnc/binary>> | Acc]
    end, [], Map),
    <<"{", (iolist_to_binary(lists:join(<<",">>, Pairs)))/binary, "}">>.

encode_json_value(V) when is_binary(V) ->
    <<"\"", V/binary, "\"">>;
encode_json_value(V) when is_integer(V) ->
    integer_to_binary(V);
encode_json_value(V) when is_atom(V) ->
    <<"\"", (atom_to_binary(V))/binary, "\"">>.

decode_json(Bin) ->
    try
        %% Simple JSON object parser
        {ok, parse_json_object(Bin)}
    catch
        _:_ -> {error, invalid_json}
    end.

parse_json_object(<<"{", Rest/binary>>) ->
    parse_json_pairs(Rest, #{}).

parse_json_pairs(<<"}", _/binary>>, Acc) ->
    Acc;
parse_json_pairs(<<",", Rest/binary>>, Acc) ->
    parse_json_pairs(Rest, Acc);
parse_json_pairs(<<" ", Rest/binary>>, Acc) ->
    parse_json_pairs(Rest, Acc);
parse_json_pairs(<<"\"", Rest/binary>>, Acc) ->
    {Key, Rest2} = parse_json_string(Rest, <<>>),
    Rest3 = skip_colon(Rest2),
    {Value, Rest4} = parse_json_value(Rest3),
    parse_json_pairs(Rest4, maps:put(Key, Value, Acc)).

skip_colon(<<":", Rest/binary>>) -> Rest;
skip_colon(<<" ", Rest/binary>>) -> skip_colon(Rest);
skip_colon(Rest) -> Rest.

parse_json_string(<<"\"", Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_json_string(<<"\\\"", Rest/binary>>, Acc) ->
    parse_json_string(Rest, <<Acc/binary, "\"">>);
parse_json_string(<<C, Rest/binary>>, Acc) ->
    parse_json_string(Rest, <<Acc/binary, C>>).

parse_json_value(<<" ", Rest/binary>>) ->
    parse_json_value(Rest);
parse_json_value(<<"\"", Rest/binary>>) ->
    {Str, Rest2} = parse_json_string(Rest, <<>>),
    {Str, Rest2};
parse_json_value(Bin) ->
    %% Try to parse number
    parse_json_number(Bin, <<>>).

parse_json_number(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_json_number(Rest, <<Acc/binary, C>>);
parse_json_number(Rest, Acc) when byte_size(Acc) > 0 ->
    {binary_to_integer(Acc), Rest};
parse_json_number(Rest, <<>>) ->
    {null, Rest}.

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
