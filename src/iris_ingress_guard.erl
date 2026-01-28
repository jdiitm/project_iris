-module(iris_ingress_guard).
-behaviour(gen_server).

%% API
-export([start_link/0, check/0, close/0, get_active_count/0]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(MAX_CONNECTIONS, 100000). %% Hard limit per node (approx 4GB RAM usage at 40KB/conn)
-define(ATOMIC_INDEX, 1).

-record(state, {
    atomics_ref :: atomics:atomics_ref()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if a new connection is allowed.
%% Increments the counter if allowed. Caller MUST call close/0 when done.
-spec check() -> allow | {deny, limit_reached}.
check() ->
    case get_atomics_ref() of
        undefined -> allow; %% Fail open if not started (safe for dev)
        Ref ->
            %% Optimistic Increment
            atomics:add(Ref, ?ATOMIC_INDEX, 1),
            Count = atomics:get(Ref, ?ATOMIC_INDEX),
            case Count > ?MAX_CONNECTIONS of
                true ->
                    %% Revert and deny
                    atomics:sub(Ref, ?ATOMIC_INDEX, 1),
                    {deny, limit_reached};
                false ->
                    allow
            end
    end.

%% @doc Decrement active connection count.
-spec close() -> ok.
close() ->
    case get_atomics_ref() of
        undefined -> ok;
        Ref ->
            atomics:sub(Ref, ?ATOMIC_INDEX, 1),
            ok
    end.

-spec get_active_count() -> integer().
get_active_count() ->
    case get_atomics_ref() of
        undefined -> 0;
        Ref -> atomics:get(Ref, ?ATOMIC_INDEX)
    end.

%% =============================================================================
%% GenServer & Internal
%% =============================================================================

init([]) ->
    %% Create atomic counter array (size 1)
    Ref = atomics:new(1, [{signed, true}]),
    %% Store ref in persistent_term for fast access without gen_server bottleneck
    persistent_term:put(?MODULE, Ref),
    {ok, #state{atomics_ref = Ref}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    persistent_term:erase(?MODULE),
    ok.

get_atomics_ref() ->
    try persistent_term:get(?MODULE)
    catch error:badarg -> undefined
    end.
