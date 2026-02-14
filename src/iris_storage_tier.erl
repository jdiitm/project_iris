-module(iris_storage_tier).

%% =============================================================================
%% AUDIT MITIGATION: Storage Tiering (Blocker 1 — Mnesia RAM Architecture)
%% =============================================================================
%% Root cause: disc_copies tables load full key space into RAM. At 100M+ users,
%% RAM exhaustion triggers iris_mnesia_guard read-only mode.
%%
%% Solution: Transparent hot/cold tiering for user_meta.
%%
%%   Hot tier:  user_meta       (disc_copies — fast, keys in RAM)
%%   Cold tier: user_meta_cold  (disc_only_copies — no RAM, DETS-backed)
%%
%% Eviction: Entries not accessed for >1 hour move from hot → cold.
%% Promotion: On cold-tier read, entry moves cold → hot.
%% Read: read_through/3 checks hot first, falls back to cold.
%%
%% Design: Stateless functions called by iris_core and periodic sweep.
%% Access tracking via ETS table (iris_storage_tier_access).
%% =============================================================================

-export([evict_cold/3, read_through/3, promote/3]).
-export([touch/1]).

-define(ACCESS_TABLE, iris_storage_tier_access).

%% @doc Evict cold entries from HotTable to ColdTable.
%% Entries whose last access (in ACCESS_TABLE) is older than Cutoff
%% are moved from HotTable to ColdTable.
%% Returns the number of entries evicted.
-spec evict_cold(atom(), atom(), integer()) -> non_neg_integer().
evict_cold(HotTable, ColdTable, Cutoff) ->
    ensure_access_table(),
    %% Collect all keys from access table that are older than cutoff
    ColdKeys = ets:foldl(fun({User, LastAccess}, Acc) ->
        case LastAccess < Cutoff of
            true -> [User | Acc];
            false -> Acc
        end
    end, [], ?ACCESS_TABLE),
    %% Move each cold entry from hot to cold tier
    Evicted = lists:foldl(fun(User, Count) ->
        case mnesia:dirty_read(HotTable, User) of
            [Record] ->
                %% Convert record tag from HotTable to ColdTable
                ColdRecord = setelement(1, Record, ColdTable),
                mnesia:dirty_write(ColdRecord),
                mnesia:dirty_delete(HotTable, User),
                ets:delete(?ACCESS_TABLE, User),
                Count + 1;
            [] ->
                %% Not in hot table (already evicted or deleted)
                ets:delete(?ACCESS_TABLE, User),
                Count
        end
    end, 0, ColdKeys),
    %% Emit metric
    case Evicted > 0 of
        true ->
            try iris_metrics:inc(storage_tier_evictions_total, Evicted)
            catch C1:R1 ->
                logger:warning("~p: metrics inc(evictions) failed ~p:~p", [?MODULE, C1, R1]),
                ok
            end;
        false -> ok
    end,
    Evicted.

%% @doc Read-through: check hot tier first, fall back to cold tier.
%% Returns {ok, Record} | not_found.
%% Emits storage_tier_cold_reads_total metric on cold-tier hit.
%% Gracefully handles missing cold table (returns not_found).
-spec read_through(atom(), atom(), binary()) -> {ok, tuple()} | not_found.
read_through(HotTable, ColdTable, Key) ->
    case safe_dirty_read(HotTable, Key) of
        [Record] ->
            %% Hot hit — update access timestamp
            touch(Key),
            {ok, Record};
        [] ->
            %% Hot miss — check cold tier
            case safe_dirty_read(ColdTable, Key) of
                [ColdRecord] ->
                    %% Cold hit — emit metric
                    try iris_metrics:inc(storage_tier_cold_reads_total)
                    catch C2:R2 ->
                        logger:warning("~p: metrics inc(cold_reads) failed ~p:~p", [?MODULE, C2, R2]),
                        ok
                    end,
                    {ok, ColdRecord};
                [] ->
                    not_found
            end
    end.

%% @doc Promote an entry from cold tier back to hot tier.
%% Called when cold data is accessed and should be made fast again.
%% Gracefully handles missing cold table (returns not_found).
-spec promote(atom(), atom(), binary()) -> ok | not_found.
promote(HotTable, ColdTable, Key) ->
    case safe_dirty_read(ColdTable, Key) of
        [ColdRecord] ->
            %% Convert record tag from ColdTable to HotTable
            HotRecord = setelement(1, ColdRecord, HotTable),
            mnesia:dirty_write(HotRecord),
            mnesia:dirty_delete(ColdTable, Key),
            touch(Key),
            ok;
        [] ->
            not_found
    end.

%% @doc Update the last-access timestamp for a user.
%% Called by iris_core on any user_meta read or write.
-spec touch(binary()) -> ok.
touch(User) ->
    ensure_access_table(),
    ets:insert(?ACCESS_TABLE, {User, os:system_time(second)}),
    ok.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

ensure_access_table() ->
    case ets:whereis(?ACCESS_TABLE) of
        undefined ->
            try
                ets:new(?ACCESS_TABLE, [
                    set, named_table, public,
                    {write_concurrency, true},
                    {read_concurrency, true}
                ])
            catch error:badarg -> ok  %% Race: another process created it
            end;
        _ -> ok
    end.

%% @doc Safe dirty_read that returns [] if the table doesn't exist.
%% Prevents crashes in tests or during startup when cold table isn't created yet.
-spec safe_dirty_read(atom(), term()) -> [tuple()].
safe_dirty_read(Table, Key) ->
    try mnesia:dirty_read(Table, Key)
    catch
        exit:{aborted, {no_exists, _}} -> [];
        _:_ -> []
    end.
