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
-export([days_until_cert_expiry/1]).

-record(state, {
    lsock :: gen_tcp:socket() | ssl:sslsocket(),
    handler :: atom(),
    tls_enabled :: boolean()
}).

%% Per-IP connection rate limiting (RFC Section 10.1)
%% RFC: 5 connections per minute per IP (configurable via app env)
-define(CONN_RATE_TABLE, iris_conn_rate).
-define(CONN_RATE_WINDOW_MS, 60000). %% 1-minute sliding window
-define(CONN_RATE_MAX, 5).           %% Max 5 connections per IP per minute

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
    %% Trap exits so acceptor crashes don't kill the listener.
    %% Without this, a single handler crash cascades:
    %%   handler -> acceptor (spawn_link) -> listener -> all 500 acceptors
    %% With trap_exit, we absorb the EXIT and respawn the dead acceptor.
    process_flag(trap_exit, true),
    
    %% AUDIT FIX (Finding #7): TCP tuning for planet-scale
    %% - backlog: 65535 (was 4096) - handles thundering herd reconnects
    %% - nodelay: true - disable Nagle's algorithm (~40ms latency reduction)
    %% - send_timeout: prevent blocking sends from stalling acceptors
    BaseOpts = [
        binary,
        {packet, 0},
        {active, false},
        {reuseaddr, true},
        {backlog, 65535},           %% Increased from 4096 for mass reconnect scenarios
        {nodelay, true},            %% Disable Nagle's algorithm for low latency
        {send_timeout, 30000},      %% 30s send timeout prevents blocking
        {send_timeout_close, true}  %% Close socket on send timeout
    ],
    
    case do_listen(Port, TlsEnabled, BaseOpts, 3) of
        {ok, LSock} ->
            io:format("Listener started on port ~p (Handler: ~p, TLS: ~p)~n",
                      [Port, HandlerMod, TlsEnabled]),
            
            %% RFC Section 10: Per-IP connection rate limiting
            %% NOTE: ETS table iris_conn_rate is now owned by iris_edge_sup
            %% so it survives listener restarts. Verify it exists:
            case ets:info(?CONN_RATE_TABLE) of
                undefined ->
                    logger:warning("iris_conn_rate ETS table missing — creating fallback"),
                    ets:new(?CONN_RATE_TABLE, [public, named_table, bag,
                                               {write_concurrency, true},
                                               {read_concurrency, true}]);
                _ -> ok
            end,
            
            NumAcceptors = application:get_env(iris_edge, num_acceptors, 500),
            [spawn_acceptor(LSock, HandlerMod, TlsEnabled)
             || _ <- lists:seq(1, NumAcceptors)],
            
            {ok, #state{lsock = LSock, handler = HandlerMod,
                        tls_enabled = TlsEnabled}};
        {error, Reason} ->
            {stop, {listen_failed, Port, Reason}}
    end.

%% Listen with retry for transient eaddrinuse after SIGKILL restarts.
do_listen(Port, TlsEnabled, BaseOpts, Retries) ->
    Result = case TlsEnabled of
        true ->
            ok = ensure_ssl_started(),
            AllOpts = BaseOpts ++ get_tls_options(),
            logger:info("Starting TLS listener on port ~p", [Port]),
            ssl:listen(Port, AllOpts);
        false ->
            logger:info("Starting TCP listener on port ~p (TLS disabled)", [Port]),
            gen_tcp:listen(Port, BaseOpts)
    end,
    case Result of
        {ok, LSock} ->
            {ok, LSock};
        {error, eaddrinuse} when Retries > 0 ->
            logger:warning("Port ~p in use, retrying in 1s (~p retries left)",
                           [Port, Retries]),
            timer:sleep(1000),
            do_listen(Port, TlsEnabled, BaseOpts, Retries - 1);
        {error, Reason} ->
            logger:error("Failed to bind port ~p: ~p", [Port, Reason]),
            {error, Reason}
    end.

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
    %% AUDIT 7.2: Warn if certificate is near expiry
    check_cert_expiry_warning(CertFile),
    
    BaseOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        %% RFC NFR-14: TLS 1.3 MANDATORY for all client connections
        {versions, ['tlsv1.3']},
        {ciphers, tls_ciphers()},
        {honor_cipher_order, true}
        %% NOTE: reuse_sessions removed -- not applicable to TLS 1.3
        %% (TLS 1.3 uses built-in 0-RTT/PSK session resumption)
    ],
    
    %% Optional: Client certificate verification
    case application:get_env(iris_edge, tls_cacertfile) of
        {ok, CACertFile} ->
            Verify = application:get_env(iris_edge, tls_verify, verify_peer),
            FailNoPeer = application:get_env(iris_edge, tls_fail_if_no_peer_cert, false),
            BaseOpts ++ [
                {cacertfile, CACertFile},
                {verify, Verify},
                {fail_if_no_peer_cert, FailNoPeer}
            ];
        undefined ->
            BaseOpts ++ [{verify, verify_none}]
    end.

%% AUDIT 7.2: Certificate expiry detection and warning
check_cert_expiry_warning(CertFile) ->
    case days_until_cert_expiry(CertFile) of
        Days when is_integer(Days), Days < 0 ->
            logger:error("TLS certificate EXPIRED ~p days ago: ~s",
                         [abs(Days), CertFile]);
        Days when is_integer(Days), Days < 30 ->
            logger:warning("TLS certificate expires in ~p days: ~s",
                           [Days, CertFile]);
        _ -> ok
    end.

%% @doc Return the number of days until the certificate in CertFile expires.
%% Negative values mean the cert is already expired.
-spec days_until_cert_expiry(string()) -> integer() | {error, term()}.
days_until_cert_expiry(CertFile) ->
    case file:read_file(CertFile) of
        {ok, PemBin} ->
            try
                [Entry|_] = public_key:pem_decode(PemBin),
                Cert = public_key:pem_entry_decode(Entry),
                TBS = element(2, Cert),
                Validity = element(6, TBS),
                NotAfter = element(3, Validity),
                ExpirySeconds = utc_time_to_epoch(NotAfter),
                NowSeconds = calendar:datetime_to_gregorian_seconds(
                    calendar:universal_time()) - 62167219200,
                (ExpirySeconds - NowSeconds) div 86400
            catch _:Reason ->
                {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Parse UTCTime "YYMMDDHHMMSSZ" to Unix epoch seconds
utc_time_to_epoch({utcTime, TimeStr}) ->
    %% UTCTime format: YYMMDDHHMMSSZ
    [Y1, Y2, M1, M2, D1, D2, H1, H2, Min1, Min2, S1, S2, $Z] = TimeStr,
    YY = (Y1 - $0) * 10 + (Y2 - $0),
    Year = if YY >= 50 -> 1900 + YY; true -> 2000 + YY end,
    Month = (M1 - $0) * 10 + (M2 - $0),
    Day = (D1 - $0) * 10 + (D2 - $0),
    Hour = (H1 - $0) * 10 + (H2 - $0),
    Min = (Min1 - $0) * 10 + (Min2 - $0),
    Sec = (S1 - $0) * 10 + (S2 - $0),
    DateTime = {{Year, Month, Day}, {Hour, Min, Sec}},
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.

%% RFC NFR-14: TLS 1.3 cipher suites only
tls_ciphers() ->
    [
        %% TLS 1.3 ciphers (only -- TLS 1.2 ciphers removed per NFR-14)
        "TLS_AES_256_GCM_SHA384",
        "TLS_AES_128_GCM_SHA256",
        "TLS_CHACHA20_POLY1305_SHA256"
    ].

%% =============================================================================
%% Acceptor Pool
%% =============================================================================

spawn_acceptor(LSock, HandlerMod, TlsEnabled) ->
    spawn_link(fun() -> acceptor_safe(LSock, HandlerMod, TlsEnabled) end).

%% Wrap acceptor in try/catch so unexpected exceptions don't crash
%% the linked listener. Normal exits (e.g. listen socket closed) propagate.
acceptor_safe(LSock, HandlerMod, TlsEnabled) ->
    try
        acceptor(LSock, HandlerMod, TlsEnabled)
    catch
        error:closed -> ok;  %% Listen socket closed (shutdown)
        Class:Reason:Stack ->
            logger:error("Acceptor crash: ~p:~p~n~p", [Class, Reason, Stack]),
            exit({acceptor_crash, Reason})
    end.

acceptor(LSock, HandlerMod, false) ->
    %% Plain TCP accept + per-IP rate limiting (RFC Section 10)
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            case check_conn_rate_tcp(Sock) of
                allow ->
                    handle_new_connection(Sock, HandlerMod, false),
                    acceptor(LSock, HandlerMod, false);
                deny ->
                    gen_tcp:close(Sock),
                    acceptor(LSock, HandlerMod, false)
            end;
        {error, emfile} ->
            timer:sleep(1000),
            acceptor(LSock, HandlerMod, false);
        {error, closed} ->
            ok;  %% Listen socket closed (shutdown)
        _Error ->
            timer:sleep(200),
            acceptor(LSock, HandlerMod, false)
    end;

acceptor(LSock, HandlerMod, true) ->
    %% TLS accept with per-IP rate limiting AFTER handshake (RFC Section 10).
    %% BUG FIX: ssl:peername/1 returns {error, _} on transport-accepted sockets
    %% before handshake completes, causing rate checks to always return 'allow'.
    %% Moved check to after handshake where ssl:peername reliably returns the IP.
    case ssl:transport_accept(LSock, 30000) of
        {ok, TlsSock} ->
            %% Per-IP rate check BEFORE handshake (RFC Section 10).
            %% Using inet:peername on the underlying TCP socket since
            %% ssl:peername doesn't work before handshake. Denying here
            %% skips the ~20ms TLS overhead for rate-limited connections.
            PeerIP = get_tcp_peer_ip(TlsSock),
            case check_ip_rate_maybe(PeerIP) of
                deny ->
                    %% Close the underlying TCP socket directly for fast rejection.
                    %% ssl:close on a pre-handshake socket can hang; gen_tcp:close
                    %% sends an immediate RST to the client.
                    try
                        {sslsocket, {_, TcpPort, _, _}, _} = TlsSock,
                        gen_tcp:close(TcpPort)
                    catch Class:Reason ->
                        logger:warning("iris_edge_listener: TLS socket close fallback: ~p:~p", [Class, Reason]),
                        try ssl:close(TlsSock) catch C2:R2 ->
                            logger:debug("ssl:close also failed: ~p:~p", [C2, R2]),
                            ok
                        end
                    end,
                    acceptor(LSock, HandlerMod, true);
                allow ->
                    case ssl:handshake(TlsSock, 10000) of
                        {ok, SslSocket} ->
                            handle_new_connection(SslSocket, HandlerMod, true),
                            acceptor(LSock, HandlerMod, true);
                        {error, Reason} ->
                            logger:warning("TLS handshake failed: ~p", [Reason]),
                            try ssl:close(TlsSock) catch C3:R3 ->
                                logger:debug("Post-handshake ssl:close failed: ~p:~p", [C3, R3]),
                                ok
                            end,
                            acceptor(LSock, HandlerMod, true)
                    end
            end;
        {error, timeout} ->
            acceptor(LSock, HandlerMod, true);
        {error, closed} ->
            ok;  %% Listen socket closed (shutdown)
        {error, emfile} ->
            timer:sleep(1000),
            acceptor(LSock, HandlerMod, true);
        {error, _Reason} ->
            timer:sleep(200),
            acceptor(LSock, HandlerMod, true)
    end.

handle_new_connection(Sock, HandlerMod, TlsEnabled) ->
    %% Start handler and transfer socket ownership.
    %% CRITICAL: unlink handler after socket transfer so handler crashes
    %% do NOT cascade to this acceptor -> listener -> all other acceptors.
    case HandlerMod:start_link(Sock) of
        {ok, Pid} ->
            %% Transfer socket to handler
            case TlsEnabled of
                true -> ssl:controlling_process(Sock, Pid);
                false -> gen_tcp:controlling_process(Sock, Pid)
            end,
            %% Break the link: handler is now autonomous with its own socket.
            %% If it crashes, only it dies — not this acceptor or the listener.
            unlink(Pid),
            HandlerMod:set_socket(Pid, Sock);
        {error, Reason} ->
            logger:error("Failed to start handler: ~p", [Reason]),
            case TlsEnabled of
                true -> ssl:close(Sock);
                false -> gen_tcp:close(Sock)
            end
    end.

%% =============================================================================
%% Per-IP Connection Rate Limiting (RFC Section 10)
%% =============================================================================

%% Extract peer IP at the TCP level from a transport-accepted SSL socket.
%% The SSL socket wraps a TCP port; inet:peername works on the raw port
%% even before TLS handshake. This avoids the race where ssl:peername
%% returns {error, einval} if the client closes quickly after handshake.
get_tcp_peer_ip(TlsSock) ->
    try
        {sslsocket, {_, TcpPort, _, _}, _} = TlsSock,
        case inet:peername(TcpPort) of
            {ok, {IP, _Port}} -> {ok, IP};
            {error, _} -> error
        end
    catch Class:Reason ->
        logger:warning("iris_edge_listener:extract_tls_ip catch-all: ~p:~p", [Class, Reason]),
        error
    end.

%% Rate-check wrapper: only checks if we successfully extracted the IP.
check_ip_rate_maybe({ok, IP}) -> check_ip_rate(IP);
check_ip_rate_maybe(error)    -> allow.

check_conn_rate_tcp(Sock) ->
    try
        case inet:peername(Sock) of
            {ok, {IP, _Port}} -> check_ip_rate(IP);
            {error, _} -> allow
        end
    catch Class:Reason ->
        logger:warning("iris_edge_listener:check_conn_rate_tcp catch-all: ~p:~p", [Class, Reason]),
        allow
    end.

check_ip_rate(IP) ->
    Now = os:system_time(millisecond),
    Cutoff = Now - ?CONN_RATE_WINDOW_MS,
    MaxRate = application:get_env(iris_edge, conn_rate_max, ?CONN_RATE_MAX),
    
    %% Record this connection attempt
    ets:insert(?CONN_RATE_TABLE, {IP, Now}),
    
    %% Count recent connections from this IP (bag table: lookup returns all)
    AllEntries = ets:lookup(?CONN_RATE_TABLE, IP),
    RecentCount = length([1 || {_, T} <- AllEntries, T > Cutoff]),
    
    %% Cleanup old entries inline
    OldEntries = [{K, T} || {K, T} <- AllEntries, T =< Cutoff],
    lists:foreach(fun(E) -> ets:delete_object(?CONN_RATE_TABLE, E) end, OldEntries),
    
    case RecentCount > MaxRate of
        true ->
            logger:warning("Connection rate limited: ~p (~p conns in ~pms)",
                          [IP, RecentCount, ?CONN_RATE_WINDOW_MS]),
            deny;
        false ->
            allow
    end.

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

%% Acceptor died normally (listen socket closed during shutdown) — don't respawn.
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, shutdown}, State) ->
    {noreply, State};
%% Acceptor crashed — respawn a replacement to maintain pool size.
handle_info({'EXIT', _Pid, _Reason},
            State = #state{lsock = LSock, handler = Handler,
                           tls_enabled = TlsEnabled}) ->
    spawn_acceptor(LSock, Handler, TlsEnabled),
    {noreply, State};
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

