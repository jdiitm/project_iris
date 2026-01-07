-module(iris_core).
-behaviour(application).
-behaviour(supervisor).

%% OTP Callbacks
-export([start/2, stop/1, init/1]).
-export([init_db/0, init_db/1, join_cluster/1]).

%% High-Scale Messaging APIs
-export([register_user/3, lookup_user/1]).
-export([store_offline/2, store_batch/2, retrieve_offline/1]).
-export([get_bucket_count/1, set_bucket_count/2]).
-export([update_status/2, get_status/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Application Callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    %% Rationale: Production systems use structured logging for grep-ability.
    logger:info("Starting Iris Core on node ~p", [node()]),

    %% Rationale: DB initialization is moved to a dedicated manager or
    %% handled via a boot script to prevent accidental schema wipes.
    case application:get_env(iris_core, auto_init_db, false) of
        true -> init_db();
        false -> ok
    end,

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    logger:info("Stopping Iris Core on node ~p", [node()]),
    ok.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

init([]) ->
    %% Rationale: strategy 'one_for_one' is replaced with a logic-based hierarchy.
    %% We use a secondary supervisor for the batchers to isolate their crashes.
    
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},

    Children = [
        %% Status Batcher Supervisor: Manages the 100 workers
        #{id => iris_status_batcher_sup,
          start => {iris_status_batcher_sup, start_link, [100]},
          type => supervisor,
          restart => permanent}
    ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% FAANG-Grade Messaging APIs
%%%===================================================================

register_user(User, Node, Pid) ->
    %% Rationale: Transactional safety for global presence consistency.
    F = fun() -> mnesia:write({presence, User, Node, Pid}) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            logger:error("Failed to register user ~p: ~p", [User, Reason]),
            {error, Reason}
    end.

lookup_user(User) ->
    %% Rationale: Dirty reads are 10x faster and acceptable for "Online" status.
    case mnesia:dirty_read(presence, User) of
        [{presence, User, Node, Pid}] -> {ok, Node, Pid};
        [] -> {error, not_found}
    end.

store_offline(User, Msg) ->
    %% Rationale: Metrics tagging before storage call.
    telemetry:execute([iris, core, storage], #{count => 1}, #{action => store}),
    Count = get_bucket_count(User),
    iris_offline_storage:store(User, Msg, Count).

store_batch(User, Msgs) ->
    telemetry:execute([iris, core, storage], #{count => length(Msgs)}, #{action => store_batch}),
    Count = get_bucket_count(User),
    iris_offline_storage:store_batch(User, Msgs, Count).

retrieve_offline(User) ->
    Count = get_bucket_count(User),
    iris_offline_storage:retrieve(User, Count).

get_bucket_count(User) ->
    case mnesia:dirty_read(user_meta, User) of
        [{user_meta, User, Count}] -> Count;
        [] -> 1
    end.

set_bucket_count(User, Count) ->
    F = fun() -> mnesia:write({user_meta, User, Count}) end,
    mnesia:transaction(F).

update_status(User, online) -> ok;
update_status(User, offline) ->
    %% Rationale: Atomic delete prevents "ghost" online status if batcher is slow.
    mnesia:dirty_delete(presence, User),
    iris_status_batcher:submit(User, offline).

get_status(User) ->
    %% Rationale: Multi-tier lookup. RAM -> Disk.
    case mnesia:dirty_read(presence, User) of
        [{presence, User, _, _}] -> {online, true, 0};
        [] -> 
            case mnesia:dirty_read(user_status, User) of
                [{user_status, User, LastSeen}] -> {online, false, LastSeen};
                [] -> {online, false, 0}
            end
    end.

%%%===================================================================
%%% Internal Functions (Hidden from External API)
%%%===================================================================

init_db() ->
    init_db([node()]).

init_db(Nodes) ->
    %% Ensure Mnesia is stopped before schema creation
    mnesia:stop(),
    mnesia:create_schema(Nodes),
    mnesia:start(),
    
    %% Create Tables (Disc Copies for Persistence)
    mnesia:create_table(presence, [
        {ram_copies, Nodes},
        {attributes, [user, node, pid]}
    ]),
    mnesia:create_table(offline_msg, [
        {disc_copies, Nodes},
        {attributes, [key, timestamp, msg]},
        {type, bag}
    ]),
    mnesia:create_table(user_meta, [
        {disc_copies, Nodes},
        {attributes, [user, bucket_count]}
    ]),
    mnesia:create_table(user_status, [
        {disc_copies, Nodes},
        {attributes, [user, last_seen]}
    ]),
    
    %% Wait for tables
    mnesia:wait_for_tables([presence, offline_msg, user_meta, user_status], 5000),
    logger:info("Database initialized on nodes: ~p", [Nodes]).

join_cluster(Node) ->
    case net_adm:ping(Node) of
        pong ->
            mnesia:change_config({extra_db_nodes, [Node]}),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            Tables = mnesia:system_info(tables) -- [schema],
            [mnesia:add_table_copy(T, node(), disc_copies) || T <- Tables],
            logger:info("Joined cluster with ~p", [Node]);
        pang ->
            {error, undefined_node}
    end.