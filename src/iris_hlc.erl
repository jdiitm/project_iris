%%%-------------------------------------------------------------------
%%% @doc Hybrid Logical Clock (HLC) implementation for Iris.
%%%
%%% HLCs combine physical timestamps with logical counters to provide:
%%% - Total ordering within a node
%%% - Causal ordering across nodes (if A→B, then HLC[A] < HLC[B])
%%% - No central coordinator required
%%% - Clock skew tolerance up to MAX_DRIFT_MS
%%%
%%% Format (80-bit, RFC-001 v4.0 Section 5.4):
%%% ┌────────────────────────────────────────────────────────────────────┐
%%% │   Physical Time (ms)   │  Logical Ctr  │       Node ID            │
%%% │        48 bits         │    16 bits    │       16 bits            │
%%% └────────────────────────────────────────────────────────────────────┘
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_DRIFT_MS, 30000).  % Maximum tolerated clock drift (30 seconds)
-define(PHYSICAL_BITS, 48).
-define(LOGICAL_BITS, 16).
-define(NODE_BITS, 16).
-define(MAX_LOGICAL, 65535).   % 2^16 - 1  (RFC-001 v4.0: 16-bit logical counter)

%% HLC record
-record(hlc, {
    physical :: non_neg_integer(),  % milliseconds since epoch
    logical  :: non_neg_integer(),  % 0-65535
    node_id  :: non_neg_integer()   % 0-65535
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
%% Lock-free read from atomics (no gen_server call).
-spec now() -> hlc().
now() ->
    case get_hlc_atomics() of
        undefined -> gen_server:call(?SERVER, now);
        {Ref, NodeId} ->
            Packed = atomics:get(Ref, 1),
            {PT, L} = unpack_hlc(Packed),
            #hlc{physical = PT, logical = L, node_id = NodeId}
    end.

%% @doc Get current HLC timestamp for a specific node (testing).
-spec now_for_node(non_neg_integer()) -> hlc().
now_for_node(NodeId) ->
    PT = erlang:system_time(millisecond),
    #hlc{physical = PT, logical = 0, node_id = NodeId band 16#FFFF}.

%% @doc Generate a new HLC for sending a message.
%% HLC-BOTTLENECK FIX: Lock-free CAS loop on atomics instead of gen_server:call.
%% This removes the single gen_server serialization point for the hot path.
%% At 10M msg/sec, the gen_server mailbox would become the bottleneck;
%% the atomic CAS loop scales with the number of schedulers.
-spec send() -> hlc().
send() ->
    case get_hlc_atomics() of
        undefined -> gen_server:call(?SERVER, send);
        {Ref, NodeId} -> send_cas(Ref, NodeId, 0)
    end.

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

%% @doc Convert HLC to 10-byte binary (big-endian, 80-bit format).
%% RFC-001 v4.0: 48-bit physical + 16-bit logical + 16-bit node ID = 80 bits
-spec to_binary(hlc()) -> binary().
to_binary(#hlc{physical = PT, logical = L, node_id = N}) ->
    <<PT:?PHYSICAL_BITS, L:?LOGICAL_BITS, N:?NODE_BITS>>.

%% @doc Parse HLC from binary.
%% Accepts both 10-byte (80-bit, v4.0) and 8-byte (64-bit, v3 legacy) formats.
%% RFC Section 5.4: "Dual-write period where both 64-bit and 80-bit IDs are accepted."
-spec from_binary(binary()) -> hlc() | {error, invalid_format}.
from_binary(<<PT:?PHYSICAL_BITS, L:?LOGICAL_BITS, N:?NODE_BITS>>) ->
    %% 10-byte (80-bit) format: 48-bit PT + 16-bit L + 16-bit N
    #hlc{physical = PT, logical = L, node_id = N};
from_binary(<<PT:48, L:16>>) ->
    %% 8-byte (64-bit) legacy format: 48-bit PT + 16-bit L, no node ID
    %% Migration compatibility: node_id defaults to 0
    DualWriteEnabled = application:get_env(iris_edge, hlc_dual_write, true),
    case DualWriteEnabled of
        true ->
            #hlc{physical = PT, logical = L, node_id = 0};
        false ->
            {error, legacy_format_rejected}
    end;
from_binary(_) ->
    {error, invalid_format}.

%% @doc Convert HLC to 80-bit integer (for comparison/storage).
%% RFC-001 v4.0: (PT << 32) | (L << 16) | N
-spec to_integer(hlc()) -> non_neg_integer().
to_integer(#hlc{physical = PT, logical = L, node_id = N}) ->
    (PT bsl 32) bor (L bsl 16) bor N.

%% @doc Parse HLC from 80-bit integer.
-spec from_integer(non_neg_integer()) -> hlc().
from_integer(Int) when is_integer(Int), Int >= 0 ->
    N = Int band 16#FFFF,
    L = (Int bsr 16) band 16#FFFF,
    PT = Int bsr 32,
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
set_node_id(NodeId) when NodeId >= 0, NodeId =< 65535 ->
    gen_server:call(?SERVER, {set_node_id, NodeId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([NodeIdArg]) ->
    NodeId = case NodeIdArg of
        auto -> compute_node_id();
        N when is_integer(N), N >= 0, N =< 65535 -> N;
        _ -> compute_node_id()
    end,
    PT = erlang:system_time(millisecond),
    InitialHLC = #hlc{
        physical = PT,
        logical = 0,
        node_id = NodeId
    },
    %% HLC-BOTTLENECK FIX: Set up atomics for lock-free send/0 path.
    %% Packs physical(48) + logical(16) into a single 64-bit atomic.
    Ref = atomics:new(1, [{signed, true}]),
    atomics:put(Ref, 1, pack_hlc(PT, 0)),
    persistent_term:put({?MODULE, atomics_ref}, Ref),
    persistent_term:put({?MODULE, atomics_node_id}, NodeId),
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
    %% HLC send event: advance clock (gen_server fallback path)
    {NewHLC, NewState} = do_send(LastHLC, NodeId, State),
    %% Sync atomics with gen_server state
    sync_atomics(NewHLC),
    {reply, NewHLC, NewState};

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
    persistent_term:put({?MODULE, atomics_node_id}, NewNodeId),
    {reply, ok, State#state{node_id = NewNodeId, last_hlc = NewHLC}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    catch persistent_term:erase({?MODULE, atomics_ref}),
    catch persistent_term:erase({?MODULE, atomics_node_id}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Lock-free HLC (atomics-based hot path)
%%====================================================================

%% @private Pack physical time (48 bits) and logical counter (16 bits)
%% into a signed 64-bit integer for atomic operations.
-spec pack_hlc(non_neg_integer(), non_neg_integer()) -> integer().
pack_hlc(Physical, Logical) ->
    (Physical bsl 16) bor Logical.

%% @private Unpack a 64-bit packed HLC value.
-spec unpack_hlc(integer()) -> {non_neg_integer(), non_neg_integer()}.
unpack_hlc(Packed) ->
    Logical = Packed band 16#FFFF,
    Physical = Packed bsr 16,
    {Physical, Logical}.

%% @private Get the atomics ref and node ID from persistent_term.
get_hlc_atomics() ->
    try
        Ref = persistent_term:get({?MODULE, atomics_ref}),
        NodeId = persistent_term:get({?MODULE, atomics_node_id}),
        {Ref, NodeId}
    catch error:badarg -> undefined
    end.

%% @private CAS loop for send/0 (lock-free HLC advance).
send_cas(_Ref, _NodeId, Attempts) when Attempts > 50 ->
    %% Safety valve: fall back to gen_server after excessive CAS failures
    gen_server:call(?SERVER, send);
send_cas(Ref, NodeId, Attempts) ->
    Current = atomics:get(Ref, 1),
    {CurPhys, CurLogical} = unpack_hlc(Current),
    PT = erlang:system_time(millisecond),
    NewPhys = max(PT, CurPhys),
    case NewPhys =:= CurPhys of
        true ->
            NextLogical = CurLogical + 1,
            case NextLogical > ?MAX_LOGICAL of
                true ->
                    %% Counter overflow: spin-wait for wall clock to advance
                    timer:sleep(1),
                    send_cas(Ref, NodeId, Attempts + 1);
                false ->
                    NewPacked = pack_hlc(NewPhys, NextLogical),
                    case atomics:compare_exchange(Ref, 1, Current, NewPacked) of
                        ok ->
                            #hlc{physical = NewPhys, logical = NextLogical, node_id = NodeId};
                        _CurrentVal ->
                            %% CAS failed (concurrent update), retry
                            send_cas(Ref, NodeId, Attempts + 1)
                    end
            end;
        false ->
            %% Wall clock advanced, reset logical counter
            NewPacked = pack_hlc(NewPhys, 0),
            case atomics:compare_exchange(Ref, 1, Current, NewPacked) of
                ok ->
                    #hlc{physical = NewPhys, logical = 0, node_id = NodeId};
                _CurrentVal ->
                    send_cas(Ref, NodeId, Attempts + 1)
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Sync the atomics value to be at least as high as the given HLC.
%% Used by gen_server handlers (recv, send fallback) to keep the atomic
%% in sync with gen_server state.
sync_atomics(#hlc{physical = PT, logical = L}) ->
    try
        Ref = persistent_term:get({?MODULE, atomics_ref}),
        NewPacked = pack_hlc(PT, L),
        sync_atomics_cas(Ref, NewPacked)
    catch error:badarg -> ok
    end.

sync_atomics_cas(Ref, NewPacked) ->
    Current = atomics:get(Ref, 1),
    case NewPacked > Current of
        true ->
            case atomics:compare_exchange(Ref, 1, Current, NewPacked) of
                ok -> ok;
                _  -> sync_atomics_cas(Ref, NewPacked)
            end;
        false ->
            ok
    end.

%% @private Send event with counter overflow handling.
%% When logical counter hits MAX_LOGICAL and wall clock hasn't advanced,
%% spin-wait up to 5ms for wall clock to advance, ensuring strict monotonicity.
do_send(LastHLC, NodeId, State) ->
    do_send(LastHLC, NodeId, State, 0).

do_send(LastHLC, NodeId, State, Attempts) when Attempts > 50 ->
    %% Safety valve: after 50 attempts (~5ms), force advance physical time
    %% This should never happen in practice (wall clock advances within 1ms)
    logger:warning("HLC: Counter overflow forced advance after ~p attempts", [Attempts]),
    NewPhysical = LastHLC#hlc.physical + 1,
    NewHLC = #hlc{physical = NewPhysical, logical = 0, node_id = NodeId},
    {NewHLC, State#state{last_hlc = NewHLC}};
do_send(LastHLC, NodeId, State, Attempts) ->
    PT = erlang:system_time(millisecond),
    NewPhysical = max(PT, LastHLC#hlc.physical),
    case NewPhysical =:= LastHLC#hlc.physical of
        true ->
            NextLogical = LastHLC#hlc.logical + 1,
            case NextLogical > ?MAX_LOGICAL of
                true ->
                    %% Counter overflow.
                    %% Spin-wait for wall clock to advance rather than
                    %% producing duplicate timestamps.
                    timer:sleep(1),
                    do_send(LastHLC, NodeId, State, Attempts + 1);
                false ->
                    NewHLC = #hlc{physical = NewPhysical, logical = NextLogical, node_id = NodeId},
                    {NewHLC, State#state{last_hlc = NewHLC}}
            end;
        false ->
            %% Wall clock advanced, reset logical counter
            NewHLC = #hlc{physical = NewPhysical, logical = 0, node_id = NodeId},
            {NewHLC, State#state{last_hlc = NewHLC}}
    end.

%% @private Compute node ID from Erlang node name.
compute_node_id() ->
    NodeName = atom_to_binary(node(), utf8),
    Hash = erlang:phash2(NodeName, 65536),
    Hash.

%% @private Perform the receive merge operation.
%% Uses min() to cap at MAX_LOGICAL; the send path
%% handles the spin-wait. Recv only merges -- the next send() will detect
%% the saturated counter and wait for wall clock advance.
do_recv_merge(PT, LastPT, RemotePT, LastHLC, RemoteHLC, NodeId, State) ->
    NewPhysical = max(PT, max(LastPT, RemotePT)),
    NewLogical = if
        NewPhysical =:= LastPT, LastPT =:= RemotePT ->
            min(max(LastHLC#hlc.logical, RemoteHLC#hlc.logical) + 1, ?MAX_LOGICAL);
        NewPhysical =:= LastPT ->
            min(LastHLC#hlc.logical + 1, ?MAX_LOGICAL);
        NewPhysical =:= RemotePT ->
            min(RemoteHLC#hlc.logical + 1, ?MAX_LOGICAL);
        true ->
            0
    end,
    NewHLC = #hlc{physical = NewPhysical, logical = NewLogical, node_id = NodeId},
    %% Sync atomics so lock-free send/0 sees the merged state
    sync_atomics(NewHLC),
    {reply, NewHLC, State#state{last_hlc = NewHLC}}.
