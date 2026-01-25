-module(iris_registry_ets).

%% =============================================================================
%% ETS-Backed Lockfree Registry
%% =============================================================================
%% Per PRINCIPAL_AUDIT_REPORT.md Hard Stop #3:
%% Replace gen_server serialized lookups with direct ETS access.
%%
%% Design:
%% - Public ETS table for lockfree read/write
%% - Process monitors for automatic cleanup on process death
%% - No gen_server call in the hot lookup path
%%
%% This eliminates the bottleneck where all lookups serialized through
%% a single gen_server process (~100K calls/sec limit).
%% =============================================================================

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register_process/2, unregister_process/1, lookup/1]).
-export([register_service/2, lookup_service/1]).
-export([get_all/0, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(REGISTRY_TABLE, iris_registry_table).
-define(SERVICE_TABLE, iris_service_table).
-define(MONITOR_TABLE, iris_monitor_table).

-record(state, {
    monitors = #{} :: #{reference() => binary()},
    stats = #{} :: map()
}).

%% =============================================================================
%% API - All lookups are lockfree ETS operations
%% =============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a process by name (lockfree ETS insert)
%% The process is automatically unregistered when it dies.
-spec register_process(binary(), pid()) -> ok | {error, already_registered}.
register_process(Name, Pid) ->
    case ets:insert_new(?REGISTRY_TABLE, {Name, Pid, erlang:system_time(millisecond)}) of
        true ->
            %% Monitor the process for automatic cleanup
            gen_server:cast(?SERVER, {monitor, Name, Pid}),
            ok;
        false ->
            %% Check if the existing registration is for a dead process
            case ets:lookup(?REGISTRY_TABLE, Name) of
                [{Name, OldPid, _}] ->
                    %% Check if old process is dead (can't use in guard)
                    case check_process_alive(OldPid) of
                        false ->
                            %% Old process is dead, replace it
                            ets:insert(?REGISTRY_TABLE, {Name, Pid, erlang:system_time(millisecond)}),
                            gen_server:cast(?SERVER, {monitor, Name, Pid}),
                            ok;
                        true ->
                            {error, already_registered}
                    end;
                [] ->
                    %% No existing registration, try again
                    register_process(Name, Pid)
            end
    end.

%% @doc Unregister a process by name
-spec unregister_process(binary()) -> ok.
unregister_process(Name) ->
    ets:delete(?REGISTRY_TABLE, Name),
    gen_server:cast(?SERVER, {demonitor, Name}),
    ok.

%% @doc Lookup a process by name (lockfree ETS lookup)
%% This is the HOT PATH - no gen_server call, direct ETS access.
-spec lookup(binary()) -> {ok, pid()} | {error, not_found}.
lookup(Name) ->
    case ets:lookup(?REGISTRY_TABLE, Name) of
        [{Name, Pid, _Timestamp}] ->
            %% Verify process is still alive
            case check_process_alive(Pid) of
                true -> {ok, Pid};
                false ->
                    %% Stale entry - remove it async
                    spawn(fun() -> ets:delete(?REGISTRY_TABLE, Name) end),
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc Register a service (for named services, not user sessions)
-spec register_service(atom(), pid()) -> ok.
register_service(ServiceName, Pid) ->
    ets:insert(?SERVICE_TABLE, {ServiceName, Pid, node()}),
    ok.

%% @doc Lookup a service
-spec lookup_service(atom()) -> {ok, pid(), node()} | {error, not_found}.
lookup_service(ServiceName) ->
    case ets:lookup(?SERVICE_TABLE, ServiceName) of
        [{ServiceName, Pid, Node}] -> {ok, Pid, Node};
        [] -> {error, not_found}
    end.

%% @doc Get all registered processes (for debugging)
-spec get_all() -> [{binary(), pid()}].
get_all() ->
    ets:foldl(fun({Name, Pid, _}, Acc) ->
        [{Name, Pid} | Acc]
    end, [], ?REGISTRY_TABLE).

%% @doc Get registry stats
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    %% Create public ETS tables with optimizations
    ?REGISTRY_TABLE = ets:new(?REGISTRY_TABLE, [
        named_table,
        public,
        set,
        {keypos, 1},
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    
    ?SERVICE_TABLE = ets:new(?SERVICE_TABLE, [
        named_table,
        public,
        set,
        {keypos, 1},
        {read_concurrency, true}
    ]),
    
    ?MONITOR_TABLE = ets:new(?MONITOR_TABLE, [
        named_table,
        private,
        set,
        {keypos, 1}
    ]),
    
    logger:info("iris_registry_ets started with lockfree ETS registry"),
    
    {ok, #state{
        monitors = #{},
        stats = #{
            registrations => 0,
            unregistrations => 0,
            lookups => 0,
            auto_cleanups => 0
        }
    }}.

handle_call(get_stats, _From, State) ->
    TableSize = ets:info(?REGISTRY_TABLE, size),
    ServiceCount = ets:info(?SERVICE_TABLE, size),
    MonitorCount = maps:size(State#state.monitors),
    
    Stats = maps:merge(State#state.stats, #{
        registered_processes => TableSize,
        registered_services => ServiceCount,
        active_monitors => MonitorCount
    }),
    
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({monitor, Name, Pid}, State) ->
    %% Monitor the process
    Ref = erlang:monitor(process, Pid),
    %% Store the monitor reference
    ets:insert(?MONITOR_TABLE, {Ref, Name}),
    
    NewMonitors = maps:put(Ref, Name, State#state.monitors),
    NewStats = maps:update_with(registrations, fun(V) -> V + 1 end, 1, State#state.stats),
    
    {noreply, State#state{monitors = NewMonitors, stats = NewStats}};

handle_cast({demonitor, Name}, State) ->
    %% Find and remove the monitor
    NewMonitors = maps:filter(fun(_Ref, N) -> N =/= Name end, State#state.monitors),
    NewStats = maps:update_with(unregistrations, fun(V) -> V + 1 end, 1, State#state.stats),
    
    {noreply, State#state{monitors = NewMonitors, stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    %% Process died - clean up its registration
    case maps:get(Ref, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        Name ->
            %% Remove from registry
            ets:delete(?REGISTRY_TABLE, Name),
            ets:delete(?MONITOR_TABLE, Ref),
            
            NewMonitors = maps:remove(Ref, State#state.monitors),
            NewStats = maps:update_with(auto_cleanups, fun(V) -> V + 1 end, 1, State#state.stats),
            
            {noreply, State#state{monitors = NewMonitors, stats = NewStats}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

check_process_alive(Pid) when is_pid(Pid) ->
    case node(Pid) of
        Node when Node =:= node() ->
            erlang:is_process_alive(Pid);
        _RemoteNode ->
            %% For remote pids, assume alive (will fail on send if not)
            true
    end.
