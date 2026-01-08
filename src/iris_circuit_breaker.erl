-module(iris_circuit_breaker).
-behaviour(gen_server).

%% API
-export([start_link/0, call/4, record_success/1, record_failure/1]).
%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FAILURE_THRESHOLD, 5).
-define(RESET_TIMEOUT, 30000). % 30 seconds

-record(state, {
    breakers = #{} :: map() % Node => {State, Failures, LastFailureTime}
    %% State: closed | open | half_open
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Executes an RPC call protected by a circuit breaker.
-spec call(node(), module(), atom(), list()) -> term() | {error, circuit_open}.
call(Node, Mod, Fun, Args) ->
    case check_circuit(Node) of
        allow ->
            try rpc:call(Node, Mod, Fun, Args, 5000) of
                {badrpc, _} = Err ->
                    record_failure(Node),
                    Err;
                Result ->
                    record_success(Node),
                    Result
            catch
                _:Reason ->
                    record_failure(Node),
                    {error, Reason}
            end;
        deny ->
            {error, circuit_open}
    end.

record_success(Node) ->
    gen_server:cast(?SERVER, {success, Node}).

record_failure(Node) ->
    gen_server:cast(?SERVER, {failure, Node}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

check_circuit(Node) ->
    gen_server:call(?SERVER, {check, Node}).

%%%===================================================================
%%% Gen_server Callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({check, Node}, _From, State = #state{breakers = Breakers}) ->
    Result = case maps:get(Node, Breakers, {closed, 0, 0}) of
        {closed, _, _} -> allow;
        {open, _, LastFail} ->
            Now = os:system_time(millisecond),
            if Now - LastFail > ?RESET_TIMEOUT ->
                   %% Half-open: Allow this one call to test
                   allow; 
               true -> 
                   deny
            end;
        {half_open, _, _} -> allow % Actually, usually half-open allows 1, but for simplicity generic allow
    end,
    
    %% If we are transitioning to half-open from open effectively by allowing, 
    %% we should ideally track that, but let's keep it stateless for the 'check' 
    %% and update state on success/failure.
    {reply, Result, State}.

handle_cast({success, Node}, State = #state{breakers = Breakers}) ->
    NewBreakers = maps:put(Node, {closed, 0, 0}, Breakers),
    {noreply, State#state{breakers = NewBreakers}};

handle_cast({failure, Node}, State = #state{breakers = Breakers}) ->
    {Status, Failures, _} = maps:get(Node, Breakers, {closed, 0, 0}),
    NewFailures = Failures + 1,
    Now = os:system_time(millisecond),
    
    NewStatus = if NewFailures >= ?FAILURE_THRESHOLD -> open; true -> Status end,
    
    NewBreakers = maps:put(Node, {NewStatus, NewFailures, Now}, Breakers),
    {noreply, State#state{breakers = NewBreakers}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
