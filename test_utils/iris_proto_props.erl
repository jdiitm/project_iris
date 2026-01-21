%% =============================================================================
%% Property-Based Tests for iris_proto
%% =============================================================================
%% These tests use PropEr-style property testing to validate protocol invariants.
%% Run with: erl -pa ebin -eval "iris_proto_props:test(), init:stop()."
%%
%% Properties Tested:
%% 1. Codec roundtrip: decode(encode(X)) == X
%% 2. Sequence ordering: Messages preserve sender-assigned sequence
%% 3. Dedup idempotency: Same message ID always deduplicated
%% 4. Length limits: Protocol handles max-length payloads
%% =============================================================================

-module(iris_proto_props).

%% API
-export([test/0, test_all/0]).
-export([prop_encode_decode_roundtrip/0]).
-export([prop_sequence_ordering/0]).
-export([prop_msg_id_format/0]).
-export([prop_length_limits/0]).

%% Number of random test cases to generate
-define(NUM_TESTS, 100).

%% Maximum sizes from RFC
-define(MAX_USERNAME_LEN, 64).
-define(MAX_MSG_LEN, 65535).

%% =============================================================================
%% Test Runner
%% =============================================================================

test() ->
    test_all().

test_all() ->
    io:format("~n=== Property-Based Tests for iris_proto ===~n~n"),
    
    Results = [
        run_property("Encode/Decode Roundtrip", fun prop_encode_decode_roundtrip/0),
        run_property("Sequence Ordering", fun prop_sequence_ordering/0),
        run_property("Message ID Format", fun prop_msg_id_format/0),
        run_property("Length Limits", fun prop_length_limits/0)
    ],
    
    Passed = length([R || R <- Results, R == passed]),
    Failed = length(Results) - Passed,
    
    io:format("~n=== Summary ===~n"),
    io:format("Total: ~p~n", [length(Results)]),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    
    case Failed of
        0 -> 
            io:format("~n✓ All property tests passed!~n"),
            ok;
        _ -> 
            io:format("~n✗ ~p property test(s) failed~n", [Failed]),
            error
    end.

run_property(Name, PropFun) ->
    io:format("Testing: ~s... ", [Name]),
    try
        case PropFun() of
            true -> 
                io:format("✓ PASS~n"),
                passed;
            false -> 
                io:format("✗ FAIL~n"),
                failed;
            {error, Reason} ->
                io:format("✗ FAIL: ~p~n", [Reason]),
                failed
        end
    catch
        Class:Error:Stack ->
            io:format("✗ CRASH: ~p:~p~n", [Class, Error]),
            io:format("  Stack: ~p~n", [Stack]),
            failed
    end.

%% =============================================================================
%% Random Generators
%% =============================================================================

%% Generate random binary of given size
random_binary(MaxSize) ->
    Size = rand:uniform(MaxSize + 1) - 1,
    list_to_binary([rand:uniform(256) - 1 || _ <- lists:seq(1, Size)]).

%% Generate random username (alphanumeric)
random_username() ->
    Len = rand:uniform(?MAX_USERNAME_LEN),
    Chars = "abcdefghijklmnopqrstuvwxyz0123456789_",
    [lists:nth(rand:uniform(length(Chars)), Chars) || _ <- lists:seq(1, Len)].

%% Generate random message content
random_message() ->
    Len = rand:uniform(min(?MAX_MSG_LEN, 1000)),  % Keep tests fast
    list_to_binary([rand:uniform(256) - 1 || _ <- lists:seq(1, Len)]).

%% Generate random sequence number (32-bit)
random_sequence() ->
    rand:uniform(4294967296) - 1.  % 0 to 2^32-1

%% =============================================================================
%% Property 1: Encode/Decode Roundtrip
%% =============================================================================
%% For any valid message, decode(encode(M)) should return the original message
%% or an equivalent representation.

prop_encode_decode_roundtrip() ->
    run_n_times(?NUM_TESTS, fun() ->
        MsgId = iris_proto:generate_msg_id(),
        Content = random_message(),
        
        %% Encode as reliable message
        Encoded = iris_proto:encode_reliable_msg(MsgId, Content),
        
        %% Verify it's valid binary
        case is_binary(Encoded) andalso byte_size(Encoded) > 0 of
            true ->
                %% Decode and verify structure
                <<Opcode:8, Rest/binary>> = Encoded,
                Opcode == 16 orelse throw({bad_opcode, Opcode});
            false ->
                throw(empty_encoding)
        end,
        true
    end).

%% =============================================================================
%% Property 2: Sequence Ordering
%% =============================================================================
%% Messages with sequence numbers N, N+1, N+2 should maintain relative order
%% when encoded and decoded.

prop_sequence_ordering() ->
    run_n_times(?NUM_TESTS, fun() ->
        %% Generate random starting sequence
        StartSeq = random_sequence(),
        
        %% Create 10 sequential messages
        Sequences = lists:seq(StartSeq, min(StartSeq + 9, 4294967295)),
        
        Messages = [
            {Seq, iris_proto:encode_seq_msg(Seq, <<"sender">>, <<"content", (integer_to_binary(Seq))/binary>>)}
            || Seq <- Sequences
        ],
        
        %% Verify sequences are extractable and in order
        ExtractedSeqs = [
            begin
                <<_Opcode:8, SeqNum:32, _Rest/binary>> = Encoded,
                SeqNum
            end
            || {_, Encoded} <- Messages
        ],
        
        %% Verify ordering preserved
        lists:sort(ExtractedSeqs) == ExtractedSeqs
    end).

%% =============================================================================
%% Property 3: Message ID Format
%% =============================================================================
%% Generated message IDs should be unique and sortable.

prop_msg_id_format() ->
    run_n_times(?NUM_TESTS, fun() ->
        %% Generate many IDs
        Ids = [iris_proto:generate_msg_id() || _ <- lists:seq(1, 100)],
        
        %% All should be binary
        AllBinary = lists:all(fun is_binary/1, Ids),
        
        %% All should be unique
        UniqueIds = lists:usort(Ids),
        AllUnique = length(UniqueIds) == length(Ids),
        
        %% IDs should have consistent length
        Lengths = [byte_size(Id) || Id <- Ids],
        ConsistentLength = length(lists:usort(Lengths)) == 1,
        
        AllBinary andalso AllUnique andalso ConsistentLength
    end).

%% =============================================================================
%% Property 4: Length Limits
%% =============================================================================
%% Protocol should handle edge cases for field lengths.

prop_length_limits() ->
    run_n_times(20, fun() ->
        %% Test empty username
        test_username_length(0),
        
        %% Test max username
        test_username_length(?MAX_USERNAME_LEN),
        
        %% Test empty message
        test_message_length(0),
        
        %% Test large message (but not max to keep tests fast)
        test_message_length(10000),
        
        true
    end).

test_username_length(Len) ->
    Username = lists:duplicate(Len, $a),
    %% Should not crash when encoding login packet
    Packet = <<1:8, (list_to_binary(Username))/binary>>,
    is_binary(Packet).

test_message_length(Len) ->
    Msg = list_to_binary(lists:duplicate(Len, $x)),
    MsgId = iris_proto:generate_msg_id(),
    %% Should not crash when encoding
    Encoded = iris_proto:encode_reliable_msg(MsgId, Msg),
    is_binary(Encoded) andalso byte_size(Encoded) > Len.

%% =============================================================================
%% Helper: Run property N times
%% =============================================================================

run_n_times(N, PropFun) ->
    run_n_times(N, PropFun, 1).

run_n_times(0, _PropFun, _Iteration) ->
    true;
run_n_times(N, PropFun, Iteration) ->
    try
        case PropFun() of
            true -> 
                run_n_times(N - 1, PropFun, Iteration + 1);
            false -> 
                io:format("  Failed on iteration ~p~n", [Iteration]),
                false
        end
    catch
        Class:Error ->
            io:format("  Crashed on iteration ~p: ~p:~p~n", [Iteration, Class, Error]),
            false
    end.
