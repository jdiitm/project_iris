-module(iris_async).

%% Monitored async spawn with failure observability.
%%
%% Replaces fire-and-forget spawn(fun() -> ... end) patterns with a
%% spawn_monitor wrapper that:
%% 1. Monitors the spawned process
%% 2. Logs failures at warning level
%% 3. Increments iris_metrics counter on failure (best-effort)
%%
%% The caller receives a {Pid, MonRef} tuple but does NOT need to
%% handle DOWN messages -- a background process handles them.

-export([spawn_monitored/2]).

%% @doc Spawn a monitored async operation.
%% Label is an atom identifying the operation (for logging/metrics).
%% Fun is the function to execute asynchronously.
%% Returns {Pid, MonitorRef}.
-spec spawn_monitored(atom(), fun(() -> any())) -> pid().
spawn_monitored(Label, Fun) ->
    %% Spawn the worker first (bare spawn — no monitor in the caller)
    WorkerPid = spawn(fun() ->
        try Fun()
        catch Class:Reason:Stack ->
            logger:warning("iris_async ~p failed: ~p:~p~n  ~p",
                           [Label, Class, Reason, Stack]),
            exit({async_failure, Label, {Class, Reason}})
        end
    end),
    %% Spawn a dedicated watcher that monitors the worker.
    %% The monitor is created IN the watcher process, so DOWN messages
    %% go to the watcher — not to the caller's mailbox.
    spawn(fun() ->
        MonRef = monitor(process, WorkerPid),
        receive
            {'DOWN', MonRef, process, WorkerPid, normal} ->
                ok;
            {'DOWN', MonRef, process, WorkerPid, Reason} ->
                logger:warning("iris_async ~p process ~p exited: ~p",
                               [Label, WorkerPid, Reason]),
                try iris_metrics:inc(async_spawn_failures)
                catch _:_ -> ok
                end
        after 30000 ->
            erlang:demonitor(MonRef, [flush])
        end
    end),
    WorkerPid.
