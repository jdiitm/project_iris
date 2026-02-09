/*
 * iris_zstd_nif.c - Thin NIF wrapper for libzstd
 *
 * RFC-001 v4.0 Section 11.1: Real zstd compression support.
 * Replaces the fake zlib-with-tag implementation.
 *
 * Exports:
 *   compress_nif/1   - Compress binary data with zstd (default level 3)
 *   decompress_nif/1 - Decompress zstd-compressed binary data
 */

#include "erl_nif.h"
#include <zstd.h>
#include <string.h>

/* Default compression level (zstd range: 1-22, 3 is fast default) */
#define ZSTD_DEFAULT_LEVEL 3

/*
 * compress_nif(Data :: binary()) -> {ok, Compressed} | {error, Reason}
 */
static ERL_NIF_TERM
compress_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;

    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    size_t bound = ZSTD_compressBound(input.size);
    ErlNifBinary output;

    if (!enif_alloc_binary(bound, &output)) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "alloc_failed"));
    }

    size_t result = ZSTD_compress(output.data, output.size,
                                  input.data, input.size,
                                  ZSTD_DEFAULT_LEVEL);

    if (ZSTD_isError(result)) {
        enif_release_binary(&output);
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, ZSTD_getErrorName(result),
                             ERL_NIF_LATIN1));
    }

    /* Shrink to actual compressed size */
    enif_realloc_binary(&output, result);

    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_binary(env, &output));
}

/*
 * decompress_nif(Compressed :: binary()) -> {ok, Data} | {error, Reason}
 */
static ERL_NIF_TERM
decompress_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;

    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    /* Try to get the decompressed size from the frame header */
    unsigned long long decompressed_size =
        ZSTD_getFrameContentSize(input.data, input.size);

    if (decompressed_size == ZSTD_CONTENTSIZE_ERROR) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "invalid_zstd_frame"));
    }

    if (decompressed_size == ZSTD_CONTENTSIZE_UNKNOWN) {
        /* Unknown size — use a conservative estimate */
        decompressed_size = input.size * 16;
        if (decompressed_size < 4096)
            decompressed_size = 4096;
    }

    /* Cap at 256 MB to prevent memory bombs */
    if (decompressed_size > 256 * 1024 * 1024) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "decompressed_size_too_large"));
    }

    ErlNifBinary output;
    if (!enif_alloc_binary((size_t)decompressed_size, &output)) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "alloc_failed"));
    }

    size_t result = ZSTD_decompress(output.data, output.size,
                                    input.data, input.size);

    if (ZSTD_isError(result)) {
        enif_release_binary(&output);
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_string(env, ZSTD_getErrorName(result),
                             ERL_NIF_LATIN1));
    }

    /* Shrink to actual decompressed size */
    enif_realloc_binary(&output, result);

    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_binary(env, &output));
}

static ErlNifFunc nif_funcs[] = {
    {"compress",   1, compress_nif,   0},
    {"decompress", 1, decompress_nif, 0}
};

ERL_NIF_INIT(iris_zstd_nif, nif_funcs, NULL, NULL, NULL, NULL)
