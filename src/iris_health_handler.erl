-module(iris_health_handler).
-behaviour(gen_server).

%% =============================================================================
%% Minimal HTTP Health Check / Metrics Endpoint
%% =============================================================================
%% Serves /health, /ready, and /metrics on a configurable port (default 9090).
%% Uses raw gen_tcp -- no external dependencies (cowboy/ranch not required).
%%
%% Endpoints:
%%   GET /health  -> 200 {"status":"ok","node":"...","uptime_s":...}
%%   GET /ready   -> 200 if Mnesia running and core reachable, else 503
%%   GET /metrics -> 200 Prometheus text format (from iris_metrics)
%%   *            -> 404
%% =============================================================================

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([dispatch/2]).  %% Exported for testability

-record(state, {
    listen_socket :: gen_tcp:socket(),
    port :: pos_integer()
}).

-define(DEFAULT_PORT, 9090).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    Port = application:get_env(iris_edge, health_port,
           application:get_env(iris_core, health_port, ?DEFAULT_PORT)),
    start_link(Port).

start_link(Port) ->
    %% Handle the case where iris_core supervisor already started us.
    %% When iris_edge depends on iris_core (P2-2), both supervisors run on
    %% the same node and both try to start this singleton. Return 'ignore'
    %% so the second supervisor skips the child cleanly.
    case whereis(?MODULE) of
        undefined ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []);
        _Pid ->
            ignore
    end.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(Port) ->
    case gen_tcp:listen(Port, [
        binary,
        {packet, http_bin},
        {active, false},
        {reuseaddr, true},
        {backlog, 64}
    ]) of
        {ok, LSock} ->
            logger:info("Health endpoint listening on port ~p", [Port]),
            %% Start accepting in a separate process
            self() ! accept,
            {ok, #state{listen_socket = LSock, port = Port}};
        {error, Reason} ->
            logger:warning("Health endpoint failed to listen on port ~p: ~p", [Port, Reason]),
            {ok, #state{listen_socket = undefined, port = Port}}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, #state{listen_socket = undefined} = State) ->
    {noreply, State};
handle_info(accept, #state{listen_socket = LSock} = State) ->
    %% Accept with a timeout so we can check for shutdown
    case gen_tcp:accept(LSock, 1000) of
        {ok, Sock} ->
            spawn(fun() -> handle_request(Sock) end),
            self() ! accept,
            {noreply, State};
        {error, timeout} ->
            self() ! accept,
            {noreply, State};
        {error, closed} ->
            {noreply, State#state{listen_socket = undefined}};
        {error, _Reason} ->
            self() ! accept,
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket = undefined}) ->
    ok;
terminate(_Reason, #state{listen_socket = LSock}) ->
    gen_tcp:close(LSock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% HTTP Request Handler
%% =============================================================================

handle_request(Sock) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, {http_request, 'GET', {abs_path, Path}, _}} ->
            %% Collect remaining headers (need Authorization for /metrics)
            Headers = collect_headers(Sock, #{}),
            %% Dispatch by path
            {Status, ContentType, Body} = dispatch(Path, Headers),
            send_response(Sock, Status, ContentType, Body);
        _ ->
            send_response(Sock, 400, <<"text/plain">>, <<"Bad Request">>)
    end,
    gen_tcp:close(Sock).

collect_headers(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 2000) of
        {ok, {http_header, _, 'Authorization', _, Value}} ->
            collect_headers(Sock, Acc#{authorization => Value});
        {ok, {http_header, _, _, _, _}} ->
            collect_headers(Sock, Acc);
        {ok, http_eoh} -> Acc;
        _ -> Acc
    end.

dispatch(Path, Headers) ->
    dispatch_normalized(normalize_path(Path), Headers).

dispatch_normalized(<<"/health">>, _Headers) -> health();
dispatch_normalized(<<"/ready">>, _Headers)  -> ready();
dispatch_normalized(<<"/metrics">>, Headers) ->
    %% Bearer-token auth for /metrics endpoint
    case check_metrics_auth(Headers) of
        ok -> metrics();
        unauthorized -> {401, <<"text/plain">>, <<"Unauthorized">>}
    end;
dispatch_normalized(_, _Headers) -> {404, <<"text/plain">>, <<"Not Found">>}.

%% Normalize path by stripping query string and trailing slash
normalize_path(Path) ->
    %% Strip query string
    P1 = case binary:split(Path, <<"?">>) of
        [Base | _] -> Base;
        _ -> Path
    end,
    %% Strip trailing slash (but not the root "/")
    case P1 of
        <<"/">> -> P1;
        _ ->
            case binary:last(P1) of
                $/ -> binary:part(P1, 0, byte_size(P1) - 1);
                _ -> P1
            end
    end.

%% @doc Check bearer token auth for metrics endpoint.
%% If no token is configured, metrics remain open (backward compatible for dev).
check_metrics_auth(Headers) ->
    case application:get_env(iris_core, metrics_bearer_token, undefined) of
        undefined -> ok;  %% No token configured - open access
        ExpectedToken ->
            case maps:get(authorization, Headers, undefined) of
                undefined -> unauthorized;
                AuthHeader ->
                    Expected = <<"Bearer ", ExpectedToken/binary>>,
                    case AuthHeader of
                        Expected -> ok;
                        _ -> unauthorized
                    end
            end
    end.

%% --- /health ---
health() ->
    Node = atom_to_binary(node(), utf8),
    Uptime = erlang:system_time(second) - erlang:convert_time_unit(
        erlang:system_info(start_time), native, second),
    Body = iolist_to_binary([
        <<"{\"status\":\"ok\",\"node\":\"">>, Node,
        <<"\",\"uptime_s\":">>, integer_to_binary(Uptime),
        <<"}">>
    ]),
    {200, <<"application/json">>, Body}.

%% --- /ready ---
ready() ->
    %% B-3: Return 503 immediately if application is draining
    case iris_edge_app:is_draining() of
        true ->
            {503, <<"application/json">>,
             <<"{\"ready\":false,\"reason\":\"draining\"}">>};
        false ->
            ready_check()
    end.

ready_check() ->
    MnesiaOk = try
        yes = mnesia:system_info(is_running),
        true
    catch C:R ->
        logger:debug("Health check: Mnesia not ready (~p:~p)", [C, R]),
        false
    end,
    CoreOk = case application:get_env(iris_edge, core_nodes, []) of
        [] -> true;  %% Core node doesn't need to check this
        Nodes ->
            lists:any(fun(N) -> net_adm:ping(N) =:= pong end, Nodes)
    end,
    case MnesiaOk andalso CoreOk of
        true ->
            {200, <<"application/json">>, <<"{\"ready\":true}">>};
        false ->
            Reason = iolist_to_binary([
                <<"{\"ready\":false,\"mnesia\":">>,
                atom_to_binary(MnesiaOk, utf8),
                <<",\"core_reachable\":">>,
                atom_to_binary(CoreOk, utf8),
                <<"}">>
            ]),
            {503, <<"application/json">>, Reason}
    end.

%% --- /metrics ---
metrics() ->
    try
        Body = iris_metrics:export_prometheus(),
        {200, <<"text/plain; version=0.0.4; charset=utf-8">>, Body}
    catch C:R ->
        logger:debug("Metrics export failed: ~p:~p", [C, R]),
        {503, <<"text/plain">>, <<"Metrics unavailable">>}
    end.

%% --- HTTP Response ---
send_response(Sock, Status, ContentType, Body) ->
    StatusLine = status_line(Status),
    Len = integer_to_binary(byte_size(Body)),
    Response = iolist_to_binary([
        <<"HTTP/1.1 ">>, StatusLine, <<"\r\n">>,
        <<"Content-Type: ">>, ContentType, <<"\r\n">>,
        <<"Content-Length: ">>, Len, <<"\r\n">>,
        <<"Connection: close\r\n">>,
        <<"\r\n">>,
        Body
    ]),
    %% Switch to raw mode for sending
    inet:setopts(Sock, [{packet, raw}]),
    gen_tcp:send(Sock, Response).

status_line(200) -> <<"200 OK">>;
status_line(400) -> <<"400 Bad Request">>;
status_line(401) -> <<"401 Unauthorized">>;
status_line(404) -> <<"404 Not Found">>;
status_line(503) -> <<"503 Service Unavailable">>;
status_line(N)   -> integer_to_binary(N).
