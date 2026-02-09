-module(iris_rfc_msgid_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% RFC-001 v4.0 Section 5.4: Message ID Format Compliance Tests
%% =============================================================================
%% Requirement: "Message IDs use Hybrid Logical Clocks (HLC) ... HLC Message ID (80 bits)"
%%
%% generate_msg_id/0 MUST return a 10-byte binary that is a valid 80-bit HLC.
%% These tests encode the RFC requirement. They are written BEFORE the fix
%% and expected to FAIL against the current implementation.
%% =============================================================================

%% =============================================================================
%% Test Setup/Teardown
%% =============================================================================

setup_hlc() ->
    case whereis(iris_hlc) of
        undefined -> ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    {ok, _} = iris_hlc:start_link(42),
    ok.

cleanup_hlc(_) ->
    case whereis(iris_hlc) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

rfc_msgid_test_() ->
    {foreach,
     fun setup_hlc/0,
     fun cleanup_hlc/1,
     [
        {"RFC 5.4: generate_msg_id returns 10-byte binary (80-bit HLC)",
         fun test_msg_id_is_10_byte_binary/0},
        {"RFC 1.1: generated IDs are strictly monotonic",
         fun test_msg_ids_are_monotonic/0}
     ]}.

%% Fallback test does not need HLC running
rfc_msgid_fallback_test_() ->
    [
        {"Fallback without HLC returns 10-byte binary",
         fun test_msg_id_fallback_is_10_byte_binary/0}
    ].

%% =============================================================================
%% RED Tests — Expected to FAIL before fix
%% =============================================================================

test_msg_id_is_10_byte_binary() ->
    %% RFC 5.4: Message ID must be 80-bit HLC = 10 bytes
    Id = iris_proto:generate_msg_id(),
    ?assert(is_binary(Id)),
    ?assertEqual(10, byte_size(Id)),
    %% Must parse as valid HLC
    HLC = iris_hlc:from_binary(Id),
    ?assertNotEqual({error, invalid_format}, HLC).

test_msg_ids_are_monotonic() ->
    %% RFC 1.1: "Messages have strictly monotonic IDs (HLC)"
    Id1 = iris_proto:generate_msg_id(),
    Id2 = iris_proto:generate_msg_id(),
    ?assertEqual(10, byte_size(Id1)),
    ?assertEqual(10, byte_size(Id2)),
    HLC1 = iris_hlc:from_binary(Id1),
    HLC2 = iris_hlc:from_binary(Id2),
    %% Id2 must be strictly greater than Id1
    ?assertEqual(lt, iris_hlc:compare(HLC1, HLC2)).

test_msg_id_fallback_is_10_byte_binary() ->
    %% When HLC is not running, fallback must still produce 10-byte binary
    %% (not a variable-length hex string)
    case whereis(iris_hlc) of
        undefined -> ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    Id = iris_proto:generate_msg_id(),
    ?assert(is_binary(Id)),
    ?assertEqual(10, byte_size(Id)).
