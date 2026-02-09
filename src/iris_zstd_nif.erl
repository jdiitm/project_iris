-module(iris_zstd_nif).

%% =============================================================================
%% Real Zstandard NIF wrapper (RFC-001 v4.0 Section 11.1)
%%
%% Provides native zstd compression/decompression via libzstd NIF.
%% Loaded at module load time via on_load. If the NIF .so is missing,
%% calls will fail with {error, nif_not_loaded}.
%% =============================================================================

-export([compress/1, decompress/1]).
-on_load(init/0).

init() ->
    %% Load NIF from priv/ relative to the application's ebin directory
    PrivDir = case code:priv_dir(iris_edge) of
        {error, _} ->
            case code:priv_dir(iris_core) of
                {error, _} ->
                    %% Fallback: look in ./priv relative to cwd
                    "priv";
                Dir -> Dir
            end;
        Dir -> Dir
    end,
    SoPath = filename:join(PrivDir, "iris_zstd_nif"),
    erlang:load_nif(SoPath, 0).

%% @doc Compress binary data using zstd (level 3).
%% Returns {ok, CompressedBinary} or {error, Reason}.
-spec compress(binary()) -> {ok, binary()} | {error, term()}.
compress(_Data) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Decompress zstd-compressed binary data.
%% Returns {ok, DecompressedBinary} or {error, Reason}.
-spec decompress(binary()) -> {ok, binary()} | {error, term()}.
decompress(_Data) ->
    erlang:nif_error(nif_not_loaded).
