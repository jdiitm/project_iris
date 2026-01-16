-module(iris_discovery).

%% =============================================================================
%% Pluggable Service Discovery
%% =============================================================================
%% Purpose: Abstract service discovery for different environments.
%% Backends:
%% 1. pg (default) - Erlang's built-in process groups
%% 2. dns - DNS-based discovery (for Kubernetes)
%% 3. consul - Consul service catalog
%% 4. static - Static configuration
%% =============================================================================

-export([start_link/0]).
-export([register/2, register/3]).
-export([unregister/2]).
-export([discover/1, discover/2]).
-export([get_nodes/1]).
-export([get_backend/0, set_backend/1]).
-export([get_stats/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(PG_SCOPE, iris_discovery).
-define(DEFAULT_BACKEND, pg).
-define(REFRESH_INTERVAL_MS, 30000).  %% 30s for DNS/Consul refresh

-record(state, {
    backend :: atom(),
    services = #{} :: map(),       %% ServiceName => [Nodes]
    registrations = [] :: list(),  %% [{ServiceName, Meta}]
    refresh_timer :: reference() | undefined
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register this node for a service
-spec register(atom(), map()) -> ok.
register(ServiceName, Metadata) ->
    register(ServiceName, node(), Metadata).

-spec register(atom(), node(), map()) -> ok.
register(ServiceName, Node, Metadata) ->
    gen_server:call(?SERVER, {register, ServiceName, Node, Metadata}).

%% @doc Unregister from a service
-spec unregister(atom(), node()) -> ok.
unregister(ServiceName, Node) ->
    gen_server:call(?SERVER, {unregister, ServiceName, Node}).

%% @doc Discover all nodes for a service
-spec discover(atom()) -> [{node(), map()}].
discover(ServiceName) ->
    discover(ServiceName, #{}).

-spec discover(atom(), map()) -> [{node(), map()}].
discover(ServiceName, Opts) ->
    gen_server:call(?SERVER, {discover, ServiceName, Opts}).

%% @doc Get just the nodes for a service (without metadata)
-spec get_nodes(atom()) -> [node()].
get_nodes(ServiceName) ->
    [Node || {Node, _Meta} <- discover(ServiceName)].

%% @doc Get current discovery backend
-spec get_backend() -> atom().
get_backend() ->
    case application:get_env(iris_core, discovery_backend) of
        {ok, Backend} -> Backend;
        undefined -> ?DEFAULT_BACKEND
    end.

%% @doc Set discovery backend (requires restart for full effect)
-spec set_backend(atom()) -> ok.
set_backend(Backend) when Backend == pg; Backend == dns; Backend == consul; Backend == static ->
    application:set_env(iris_core, discovery_backend, Backend),
    gen_server:cast(?SERVER, {set_backend, Backend}).

%% @doc Get discovery statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% GenServer Callbacks
%% =============================================================================

init([]) ->
    Backend = get_backend(),
    
    %% Initialize backend
    init_backend(Backend),
    
    %% Start refresh timer for non-pg backends
    Timer = case Backend of
        pg -> undefined;
        _ -> erlang:send_after(?REFRESH_INTERVAL_MS, self(), refresh)
    end,
    
    {ok, #state{backend = Backend, refresh_timer = Timer}}.

handle_call({register, ServiceName, Node, Metadata}, _From, State = #state{backend = Backend}) ->
    Result = do_register(Backend, ServiceName, Node, Metadata),
    NewRegs = [{ServiceName, Metadata} | State#state.registrations],
    {reply, Result, State#state{registrations = NewRegs}};

handle_call({unregister, ServiceName, Node}, _From, State = #state{backend = Backend}) ->
    Result = do_unregister(Backend, ServiceName, Node),
    NewRegs = lists:filter(fun({S, _}) -> S =/= ServiceName end, State#state.registrations),
    {reply, Result, State#state{registrations = NewRegs}};

handle_call({discover, ServiceName, Opts}, _From, State = #state{backend = Backend, services = Cache}) ->
    %% Check cache first for non-pg backends
    Result = case Backend of
        pg ->
            do_discover_pg(ServiceName);
        _ ->
            case maps:get(ServiceName, Cache, undefined) of
                undefined -> do_discover(Backend, ServiceName, Opts);
                Cached -> Cached
            end
    end,
    {reply, Result, State};

handle_call(get_stats, _From, State = #state{backend = Backend, services = Cache, registrations = Regs}) ->
    Stats = #{
        backend => Backend,
        cached_services => maps:size(Cache),
        local_registrations => length(Regs),
        services => maps:map(fun(_K, V) -> length(V) end, Cache)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_backend, Backend}, State) ->
    %% Cancel old timer
    case State#state.refresh_timer of
        undefined -> ok;
        OldTimer -> erlang:cancel_timer(OldTimer)
    end,
    
    %% Initialize new backend
    init_backend(Backend),
    
    %% Start new refresh timer if needed
    Timer = case Backend of
        pg -> undefined;
        _ -> erlang:send_after(?REFRESH_INTERVAL_MS, self(), refresh)
    end,
    
    {noreply, State#state{backend = Backend, refresh_timer = Timer, services = #{}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State = #state{backend = Backend}) ->
    %% Refresh service cache
    NewCache = refresh_services(Backend, State#state.services),
    Timer = erlang:send_after(?REFRESH_INTERVAL_MS, self(), refresh),
    {noreply, State#state{services = NewCache, refresh_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{backend = Backend, registrations = Regs}) ->
    %% Unregister all services
    lists:foreach(fun({ServiceName, _}) ->
        do_unregister(Backend, ServiceName, node())
    end, Regs),
    ok.

%% =============================================================================
%% Backend: pg (Erlang Process Groups)
%% =============================================================================

init_backend(pg) ->
    case pg:start(?PG_SCOPE) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end;
init_backend(_) ->
    ok.

do_register(pg, ServiceName, _Node, Metadata) ->
    %% For pg, we register the caller's pid
    %% Store metadata in process dictionary for retrieval
    put({iris_discovery_meta, ServiceName}, Metadata),
    pg:join(?PG_SCOPE, ServiceName, self()),
    ok;
do_register(static, _ServiceName, _Node, _Metadata) ->
    %% Static doesn't support dynamic registration
    ok;
do_register(dns, _ServiceName, _Node, _Metadata) ->
    %% DNS registration is external (e.g., Kubernetes service)
    ok;
do_register(consul, ServiceName, Node, Metadata) ->
    %% Would call Consul HTTP API
    logger:info("Consul register: ~p on ~p with ~p", [ServiceName, Node, Metadata]),
    ok.

do_unregister(pg, ServiceName, _Node) ->
    pg:leave(?PG_SCOPE, ServiceName, self()),
    ok;
do_unregister(_, _ServiceName, _Node) ->
    ok.

do_discover_pg(ServiceName) ->
    case pg:get_members(?PG_SCOPE, ServiceName) of
        [] -> [];
        Members ->
            lists:map(fun(Pid) ->
                Node = node(Pid),
                Meta = get_pid_metadata(Pid, ServiceName),
                {Node, Meta}
            end, Members)
    end.

get_pid_metadata(Pid, ServiceName) when Pid == self() ->
    case get({iris_discovery_meta, ServiceName}) of
        undefined -> #{};
        Meta -> Meta
    end;
get_pid_metadata(Pid, ServiceName) ->
    try
        case rpc:call(node(Pid), erlang, apply, [fun() -> 
            get({iris_discovery_meta, ServiceName}) 
        end, []], 1000) of
            undefined -> #{};
            {badrpc, _} -> #{};
            Meta -> Meta
        end
    catch
        _:_ -> #{}
    end.

%% =============================================================================
%% Backend: DNS
%% =============================================================================

do_discover(dns, ServiceName, Opts) ->
    DnsSuffix = maps:get(dns_suffix, Opts, <<".svc.cluster.local">>),
    FullName = <<(atom_to_binary(ServiceName, utf8))/binary, DnsSuffix/binary>>,
    
    case inet:getaddrs(binary_to_list(FullName), inet) of
        {ok, IPs} ->
            [{ip_to_node(IP), #{ip => IP}} || IP <- IPs];
        {error, _} ->
            []
    end;

%% =============================================================================
%% Backend: Static
%% =============================================================================

do_discover(static, ServiceName, _Opts) ->
    case application:get_env(iris_core, {static_services, ServiceName}) of
        {ok, Nodes} when is_list(Nodes) ->
            [{N, #{}} || N <- Nodes];
        undefined ->
            []
    end;

%% =============================================================================
%% Backend: Consul
%% =============================================================================

do_discover(consul, ServiceName, Opts) ->
    ConsulAddr = maps:get(consul_addr, Opts, "localhost:8500"),
    %% Would make HTTP call to Consul
    logger:info("Would query Consul at ~s for ~p", [ConsulAddr, ServiceName]),
    [].

%% =============================================================================
%% Internal Helpers
%% =============================================================================

refresh_services(Backend, CurrentCache) ->
    %% Refresh all known services
    ServiceNames = maps:keys(CurrentCache),
    lists:foldl(fun(ServiceName, Acc) ->
        Nodes = do_discover(Backend, ServiceName, #{}),
        maps:put(ServiceName, Nodes, Acc)
    end, #{}, ServiceNames).

ip_to_node({A, B, C, D}) ->
    NodeName = lists:flatten(io_lib:format("iris@~p.~p.~p.~p", [A, B, C, D])),
    list_to_atom(NodeName).
