-module(iris_edge_listener).
-behaviour(gen_server).

%% =============================================================================
%% Edge Listener with Optional TLS Support
%% =============================================================================
%% Configuration:
%% - {tls_enabled, true/false}
%% - {tls_certfile, "/path/to/cert.pem"}
%% - {tls_keyfile, "/path/to/key.pem"}
%% - {tls_cacertfile, "/path/to/ca.pem"} (optional, for client cert verification)
%% =============================================================================

-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    lsock :: gen_tcp:socket() | ssl:sslsocket(),
    handler :: atom(),
    tls_enabled :: boolean()
}).

start_link(Port) ->
    start_link(Port, iris_edge_conn).

start_link(Port, HandlerMod) ->
    Name = list_to_atom("iris_edge_listener_" ++ integer_to_list(Port)),
    gen_server:start_link({local, Name}, ?MODULE, [Port, HandlerMod], []).

init([Port, HandlerMod]) ->
    TlsEnabled = application:get_env(iris_edge, tls_enabled, false),
    
    %% RFC NFR-14: TLS MUST be mandatory for all client connections
    case check_tls_policy(TlsEnabled) of
        ok ->
            start_listener(Port, HandlerMod, TlsEnabled);
        {error, Reason} ->
            {stop, Reason}
    end.

%% Check TLS policy compliance
check_tls_policy(true) -> ok;
check_tls_policy(false) ->
    logger:warning("=== RFC VIOLATION: TLS DISABLED (NFR-14) ==="),
    logger:warning("TLS is MANDATORY per RFC-001. Set {tls_enabled, true}"),
    case application:get_env(iris_edge, allow_insecure, false) of
        true ->
            logger:warning("Running in INSECURE mode (allow_insecure=true)"),
            ok;
        false ->
            logger:error("Refusing to start without TLS. Set {allow_insecure, true} to override."),
            {error, tls_required}
    end.

%% Start the actual listener
start_listener(Port, HandlerMod, TlsEnabled) ->
    BaseOpts = [
        binary,
        {packet, 0},
        {active, false},
        {reuseaddr, true},
        {backlog, 4096}
    ],
    
    {ok, LSock} = case TlsEnabled of
        true ->
            %% Ensure SSL application is started before using ssl:listen
            ok = ensure_ssl_started(),
            TlsOpts = get_tls_options(),
            AllOpts = BaseOpts ++ TlsOpts,
            logger:info("Starting TLS listener on port ~p", [Port]),
            ssl:listen(Port, AllOpts);
        false ->
            logger:info("Starting TCP listener on port ~p (TLS disabled)", [Port]),
            gen_tcp:listen(Port, BaseOpts)
    end,
    
    io:format("Listener started on port ~p (Handler: ~p, TLS: ~p)~n", [Port, HandlerMod, TlsEnabled]),
    
    NumAcceptors = application:get_env(iris_edge, num_acceptors, 500),
    [spawn_acceptor(LSock, HandlerMod, TlsEnabled) || _ <- lists:seq(1, NumAcceptors)],
    
    {ok, #state{lsock = LSock, handler = HandlerMod, tls_enabled = TlsEnabled}}.

%% =============================================================================
%% SSL Application Management
%% =============================================================================

ensure_ssl_started() ->
    %% Check if ssl application exists before trying to start it
    case code:ensure_loaded(ssl) of
        {module, ssl} ->
            %% SSL module available, try to start application
            case application:ensure_all_started(ssl) of
                {ok, _Apps} ->
                    logger:info("SSL application started successfully"),
                    ok;
                {error, {already_started, ssl}} ->
                    ok;
                {error, Reason} ->
                    logger:error("Failed to start SSL application: ~p", [Reason]),
                    {error, {ssl_start_failed, Reason}}
            end;
        {error, _} ->
            %% SSL not available in this Erlang installation
            logger:error("SSL module not available - TLS cannot be enabled"),
            logger:error("Install Erlang with SSL support or run with {tls_enabled, false}"),
            {error, ssl_not_available}
    end.

%% =============================================================================
%% TLS Configuration
%% =============================================================================

get_tls_options() ->
    CertFile = application:get_env(iris_edge, tls_certfile, "certs/server.pem"),
    KeyFile = application:get_env(iris_edge, tls_keyfile, "certs/server.key"),
    
    BaseOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {versions, ['tlsv1.2', 'tlsv1.3']},
        {ciphers, tls_ciphers()},
        {honor_cipher_order, true},
        {reuse_sessions, true}
    ],
    
    %% Optional: Client certificate verification
    case application:get_env(iris_edge, tls_cacertfile) of
        {ok, CACertFile} ->
            BaseOpts ++ [
                {cacertfile, CACertFile},
                {verify, verify_peer},
                {fail_if_no_peer_cert, false}
            ];
        undefined ->
            BaseOpts ++ [{verify, verify_none}]
    end.

%% Secure TLS 1.2/1.3 cipher suites
tls_ciphers() ->
    [
        %% TLS 1.3 ciphers
        "TLS_AES_256_GCM_SHA384",
        "TLS_AES_128_GCM_SHA256",
        "TLS_CHACHA20_POLY1305_SHA256",
        %% TLS 1.2 ciphers (ECDHE for forward secrecy)
        "ECDHE-ECDSA-AES256-GCM-SHA384",
        "ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-ECDSA-AES128-GCM-SHA256",
        "ECDHE-RSA-AES128-GCM-SHA256",
        "ECDHE-ECDSA-CHACHA20-POLY1305",
        "ECDHE-RSA-CHACHA20-POLY1305"
    ].

%% =============================================================================
%% Acceptor Pool
%% =============================================================================

spawn_acceptor(LSock, HandlerMod, TlsEnabled) ->
    spawn_link(fun() -> acceptor(LSock, HandlerMod, TlsEnabled) end).

acceptor(LSock, HandlerMod, false) ->
    %% Plain TCP accept
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            handle_new_connection(Sock, HandlerMod, false),
            acceptor(LSock, HandlerMod, false);
        {error, emfile} ->
            timer:sleep(1000),
            acceptor(LSock, HandlerMod, false);
        _Error ->
            timer:sleep(200),
            acceptor(LSock, HandlerMod, false)
    end;

acceptor(LSock, HandlerMod, true) ->
    %% TLS accept with handshake
    case ssl:transport_accept(LSock, 30000) of
        {ok, TlsSock} ->
            case ssl:handshake(TlsSock, 10000) of
                {ok, SslSocket} ->
                    handle_new_connection(SslSocket, HandlerMod, true),
                    acceptor(LSock, HandlerMod, true);
                {error, Reason} ->
                    logger:warning("TLS handshake failed: ~p", [Reason]),
                    catch ssl:close(TlsSock),
                    acceptor(LSock, HandlerMod, true)
            end;
        {error, timeout} ->
            acceptor(LSock, HandlerMod, true);
        {error, emfile} ->
            timer:sleep(1000),
            acceptor(LSock, HandlerMod, true);
        {error, _Reason} ->
            timer:sleep(200),
            acceptor(LSock, HandlerMod, true)
    end.

handle_new_connection(Sock, HandlerMod, TlsEnabled) ->
    %% Start handler and transfer socket ownership
    case HandlerMod:start_link(Sock) of
        {ok, Pid} ->
            %% Transfer socket to handler
            case TlsEnabled of
                true -> ssl:controlling_process(Sock, Pid);
                false -> gen_tcp:controlling_process(Sock, Pid)
            end,
            HandlerMod:set_socket(Pid, Sock);
        {error, Reason} ->
            logger:error("Failed to start handler: ~p", [Reason]),
            case TlsEnabled of
                true -> ssl:close(Sock);
                false -> gen_tcp:close(Sock)
            end
    end.

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, #state{lsock = LSock, tls_enabled = TlsEnabled}) ->
    case TlsEnabled of
        true -> ssl:close(LSock);
        false -> gen_tcp:close(LSock)
    end,
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

