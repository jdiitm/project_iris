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
    
    %% Common socket options
    BaseOpts = [
        binary,
        {packet, 0},
        {active, false},
        {reuseaddr, true},
        {backlog, 4096}
    ],
    
    {ok, LSock} = case TlsEnabled of
        true ->
            %% TLS mode
            TlsOpts = get_tls_options(),
            AllOpts = BaseOpts ++ TlsOpts,
            logger:info("Starting TLS listener on port ~p", [Port]),
            ssl:listen(Port, AllOpts);
        false ->
            %% Plain TCP mode
            logger:info("Starting TCP listener on port ~p (TLS disabled)", [Port]),
            gen_tcp:listen(Port, BaseOpts)
    end,
    
    io:format("Listener started on port ~p (Handler: ~p, TLS: ~p)~n", [Port, HandlerMod, TlsEnabled]),
    
    %% Spawn parallel acceptors
    NumAcceptors = application:get_env(iris_edge, num_acceptors, 500),
    [spawn_acceptor(LSock, HandlerMod, TlsEnabled) || _ <- lists:seq(1, NumAcceptors)],
    
    {ok, #state{lsock = LSock, handler = HandlerMod, tls_enabled = TlsEnabled}}.

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

