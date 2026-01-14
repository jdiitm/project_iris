-module(chaos_resources).
-export([burn_cpu/1, eat_memory/1, cpu_worker/0, mem_worker/3]).

%% --- CPU STRESSOR ---
%% Spawns N processes that perform tight loops to saturate schedulers
burn_cpu(N_Cores) ->
    io:format("[CHAOS] Starting CPU Anvil on ~p cores...~n", [N_Cores]),
    spawn_workers(N_Cores, fun() -> spawn(?MODULE, cpu_worker, []) end).
    
cpu_worker() ->
    %% Ackermann function or simple primes to burn cycles
    ackermann(3, 10),
    cpu_worker().

ackermann(0, N) -> N + 1;
ackermann(M, 0) -> ackermann(M - 1, 1);
ackermann(M, N) -> ackermann(M - 1, ackermann(M, N - 1)).

%% --- MEMORY STRESSOR ---
%% Allocates large binaries to pressure VM
eat_memory(MB) ->
    io:format("[CHAOS] Starting Memory Leak... Target: ~p MB~n", [MB]),
    %% Calculate chunk size (e.g., 10MB chunks)
    ChunkSize = 10 * 1024 * 1024,
    Chunks = MB div 10,
    spawn(?MODULE, mem_worker, [[], Chunks, ChunkSize]).

mem_worker(List, 0, _) ->
    io:format("[CHAOS] Memory Target Reached. Holding...~n"),
    timer:sleep(infinity);
mem_worker(List, N, Size) ->
    %% Allocate binary
    Bin = <<0:Size/unit:8>>,
    timer:sleep(100), %% Slow ramp to see effect
    mem_worker([Bin | List], N - 1, Size).

spawn_workers(0, _) -> ok;
spawn_workers(N, F) ->
    F(),
    spawn_workers(N - 1, F).
