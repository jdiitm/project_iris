%%%-------------------------------------------------------------------
%%% @doc Hybrid Logical Clock (HLC) implementation for Iris.
%%%
%%% HLCs combine physical timestamps with logical counters to provide:
%%% - Total ordering within a node
%%% - Causal ordering across nodes (if A→B, then HLC[A] < HLC[B])
%%% - No central coordinator required
%%% - Clock skew tolerance up to MAX_DRIFT_MS
%%%
%%% Format (64-bit):
%%% ┌────────────────────────────────────────────────────────────┐
%%% │   Physical Time (ms)   │ Logical Ctr │     Node ID         │
%%% │        48 bits         │   8 bits    │      8 bits         │
%%% └────────────────────────────────────────────────────────────┘
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iris_hlc).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([now/0, now_for_node/1]).
-export([send/0, send/1]).
-export([recv/1, recv/2]).
-export([compare/2]).
-export([to_binary/1, from_binary/1]).
-export([to_integer/1, from_integer/1]).
-export([physical_time/1, logical_counter/1, node_id/1]).
-export([get_node_id/0, set_node_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(MAX_DRIFT_MS, 30000).  % Maximum tolerated clock drift (30 seconds)
-define(PHYSICAL_BITS, 48).
-define(LOGICAL_BITS, 8).
-define(NODE_BITS, 8).
-define(MAX_LOGICAL, 255).     % 2^8 - 1

%% HLC record
-record(hlc, {
    physical :: non_neg_integer(),  % milliseconds since epoch
    logical  :: non_neg_integer(),  % 0-255
    node_id  :: non_neg_integer()   % 0-255
}).

-record(state, {
    node_id :: non_neg_integer(),
    last_hlc :: #hlc{} | undefined
}).

-type hlc() :: #hlc{}.
-export_type([hlc/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the HLC gen_server with auto-detected node ID.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(auto).

%% @doc Start the HLC gen_server with specified node ID.
%% NodeId can be 'auto' (hash of node name), or 0-255.
-spec start_link(auto | non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(NodeId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [NodeId], []).

%% @doc Get current HLC timestamp for this node.
-spec now() -> hlc().
now() ->
    gen_server:call(?SERVER, now).

%% @doc Get current HLC timestamp for a specific node (testing).
-spec now_for_node(non_neg_integer()) -> hlc().
now_for_node(NodeId) ->
    PT = erlang:system_time(millisecond),
    #hlc{physical = PT, logical = 0, node_id = NodeId band 16#FF}.

%% @doc Generate a new HLC for sending a message.
%% Advances the local clock and returns the new timestamp.
-spec send() -> hlc().
send() ->
    gen_server:call(?SERVER, send).

%% @doc Generate a new HLC for sending with specified event context.
-spec send(term()) -> hlc().
send(_EventContext) ->
    send().

%% @doc Receive a remote HLC and update local clock.
%% Returns the updated local HLC (which is > received HLC).
-spec recv(hlc()) -> hlc().
recv(RemoteHLC) ->
    gen_server:call(?SERVER, {recv, RemoteHLC}).

%% @doc Receive a remote HLC with event context.
-spec recv(hlc(), term()) -> hlc().
recv(RemoteHLC, _EventContext) ->
    recv(RemoteHLC).

%% @doc Compare two HLCs.
%% Returns: lt (A < B), eq (A == B), gt (A > B).
-spec compare(hlc(), hlc()) -> lt | eq | gt.
compare(#hlc{physical = PA, logical = LA, node_id = NA},
        #hlc{physical = PB, logical = LB, node_id = NB}) ->
    if
        PA < PB -> lt;
        PA > PB -> gt;
        LA < LB -> lt;
        LA > LB -> gt;
        NA < NB -> lt;
        NA > NB -> gt;
        true -> eq
    end.

%% @doc Convert HLC to 8-byte binary (big-endian).
-spec to_binary(hlc()) -> binary().
to_binary(#hlc{physical = PT, logical = L, node_id = N}) ->
    <<PT:?PHYSICAL_BITS, L:?LOGICAL_BITS, N:?NODE_BITS>>.

%% @doc Parse HLC from 8-byte binary.
-spec from_binary(binary()) -> hlc() | {error, invalid_format}.
from_binary(<<PT:?PHYSICAL_BITS, L:?LOGICAL_BITS, N:?NODE_BITS>>) ->
    #hlc{physical = PT, logical = L, node_id = N};
from_binary(_) ->
    {error, invalid_format}.

%% @doc Convert HLC to 64-bit integer (for comparison/storage).
-spec to_integer(hlc()) -> non_neg_integer().
to_integer(#hlc{physical = PT, logical = L, node_id = N}) ->
    (PT bsl 16) bor (L bsl 8) bor N.

%% @doc Parse HLC from 64-bit integer.
-spec from_integer(non_neg_integer()) -> hlc().
from_integer(Int) when is_integer(Int), Int >= 0 ->
    N = Int band 16#FF,
    L = (Int bsr 8) band 16#FF,
    PT = Int bsr 16,
    #hlc{physical = PT, logical = L, node_id = N}.

%% @doc Extract physical time component (milliseconds).
-spec physical_time(hlc()) -> non_neg_integer().
physical_time(#hlc{physical = PT}) -> PT.

%% @doc Extract logical counter component.
-spec logical_counter(hlc()) -> non_neg_integer().
logical_counter(#hlc{logical = L}) -> L.

%% @doc Extract node ID component.
-spec node_id(hlc()) -> non_neg_integer().
node_id(#hlc{node_id = N}) -> N.

%% @doc Get the current node's ID.
-spec get_node_id() -> non_neg_integer().
get_node_id() ->
    gen_server:call(?SERVER, get_node_id).

%% @doc Set the node ID (for testing/reconfiguration).
-spec set_node_id(non_neg_integer()) -> ok.
set_node_id(NodeId) when NodeId >= 0, NodeId =< 255 ->
    gen_server:call(?SERVER, {set_node_id, NodeId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([NodeIdArg]) ->
    NodeId = case NodeIdArg of
        auto -> compute_node_id();
        N when is_integer(N), N >= 0, N =< 255 -> N;
        _ -> compute_node_id()
    end,
    InitialHLC = #hlc{
        physical = erlang:system_time(millisecond),
        logical = 0,
        node_id = NodeId
    },
    {ok, #state{node_id = NodeId, last_hlc = InitialHLC}}.

handle_call(now, _From, State = #state{last_hlc = LastHLC, node_id = NodeId}) ->
    PT = erlang:system_time(millisecond),
    NewHLC = #hlc{
        physical = max(PT, LastHLC#hlc.physical),
        logical = if
            PT > LastHLC#hlc.physical -> 0;
            true -> LastHLC#hlc.logical
        end,
        node_id = NodeId
    },
    {reply, NewHLC, State#state{last_hlc = NewHLC}};

handle_call(send, _From, State = #state{last_hlc = LastHLC, node_id = NodeId}) ->
    %% HLC send event: advance clock
    PT = erlang:system_time(millisecond),
    NewPhysical = max(PT, LastHLC#hlc.physical),
    NewLogical = if
        NewPhysical =:= LastHLC#hlc.physical ->
            %% Same physical time, increment logical
            min(LastHLC#hlc.logical + 1, ?MAX_LOGICAL);
        true ->
            %% New physical time, reset logical
            0
    end,
    NewHLC = #hlc{physical = NewPhysical, logical = NewLogical, node_id = NodeId},
    {reply, NewHLC, State#state{last_hlc = NewHLC}};

handle_call({recv, RemoteHLC}, _From, State = #state{last_hlc = LastHLC, node_id = NodeId}) ->
    %% HLC receive event: merge with remote
    PT = erlang:system_time(millisecond),
    RemotePT = RemoteHLC#hlc.physical,
    LastPT = LastHLC#hlc.physical,
    
    %% Check for excessive clock drift
    case abs(RemotePT - PT) > ?MAX_DRIFT_MS of
        true ->
            %% Remote clock is too far off; use local time but log warning
            error_logger:warning_msg("HLC: Remote clock drift exceeds ~p ms: remote=~p local=~p",
                                    [?MAX_DRIFT_MS, RemotePT, PT]),
            %% Proceed with bounded physical time
            BoundedRemotePT = min(RemotePT, PT + ?MAX_DRIFT_MS),
            do_recv_merge(PT, LastPT, BoundedRemotePT, LastHLC, RemoteHLC, NodeId, State);
        false ->
            do_recv_merge(PT, LastPT, RemotePT, LastHLC, RemoteHLC, NodeId, State)
    end;

handle_call(get_node_id, _From, State = #state{node_id = NodeId}) ->
    {reply, NodeId, State};

handle_call({set_node_id, NewNodeId}, _From, State = #state{last_hlc = LastHLC}) ->
    NewHLC = LastHLC#hlc{node_id = NewNodeId},
    {reply, ok, State#state{node_id = NewNodeId, last_hlc = NewHLC}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Compute node ID from Erlang node name.
compute_node_id() ->
    NodeName = atom_to_binary(node(), utf8),
    Hash = erlang:phash2(NodeName, 256),
    Hash.

%% @private Perform the receive merge operation.
do_recv_merge(PT, LastPT, RemotePT, LastHLC, RemoteHLC, NodeId, State) ->
    %% HLC receive algorithm:
    %% NewPT = max(PT, LastPT, RemotePT)
    %% NewL  = based on which PT was max
    NewPhysical = max(PT, max(LastPT, RemotePT)),
    NewLogical = if
        NewPhysical =:= LastPT, LastPT =:= RemotePT ->
            %% All three equal: max(LastL, RemoteL) + 1
            min(max(LastHLC#hlc.logical, RemoteHLC#hlc.logical) + 1, ?MAX_LOGICAL);
        NewPhysical =:= LastPT ->
            %% Local was max: LastL + 1
            min(LastHLC#hlc.logical + 1, ?MAX_LOGICAL);
        NewPhysical =:= RemotePT ->
            %% Remote was max: RemoteL + 1
            min(RemoteHLC#hlc.logical + 1, ?MAX_LOGICAL);
        true ->
            %% Current wall time is max: reset to 0
            0
    end,
    NewHLC = #hlc{physical = NewPhysical, logical = NewLogical, node_id = NodeId},
    {reply, NewHLC, State#state{last_hlc = NewHLC}}.
