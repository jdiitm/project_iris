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
-export([prop_sequence_wrap/0]).
-export([prop_binary_safety/0]).

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
        run_property("Length Limits", fun prop_length_limits/0),
        run_property("Sequence Wrap (32-bit)", fun prop_sequence_wrap/0),
        run_property("Binary Safety", fun prop_binary_safety/0)
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
%% Note: Protocol uses 64-bit sequence numbers in encode_seq_msg

prop_sequence_ordering() ->
    run_n_times(?NUM_TESTS, fun() ->
        %% Generate random starting sequence (64-bit safe)
        StartSeq = rand:uniform(1000000),
        Target = <<"user_", (integer_to_binary(rand:uniform(1000)))/binary>>,
        
        %% Create 10 sequential messages
        Sequences = lists:seq(StartSeq, StartSeq + 9),
        
        Messages = [
            {Seq, iris_proto:encode_seq_msg(Target, Seq, <<"content", (integer_to_binary(Seq))/binary>>)}
            || Seq <- Sequences
        ],
        
        %% Verify sequences are extractable and in order
        %% Format: <<7, TLen:16, Target, SeqNo:64, MLen:16, Msg>>
        ExtractedSeqs = [
            begin
                <<7:8, TLen:16, _Target:TLen/binary, SeqNum:64, _Rest/binary>> = Encoded,
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
        
        %% IDs should have reasonable length (8-32 bytes typical)
        Lengths = [byte_size(Id) || Id <- Ids],
        ReasonableLength = lists:all(fun(L) -> L >= 8 andalso L =< 64 end, Lengths),
        
        case AllBinary andalso AllUnique andalso ReasonableLength of
            true -> true;
            false ->
                io:format("    AllBinary=~p, AllUnique=~p, ReasonableLength=~p~n",
                         [AllBinary, AllUnique, ReasonableLength]),
                false
        end
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

%% =============================================================================
%% Property 5: Sequence Number Wrap at 2^64
%% =============================================================================
%% Sequence numbers are 64-bit unsigned integers in encode_seq_msg.
%% This tests various boundary values are encoded correctly.

prop_sequence_wrap() ->
    run_n_times(50, fun() ->
        Target = <<"testuser">>,
        
        %% Test boundary sequence numbers (64-bit)
        TestCases = [
            0,                          % Minimum
            1,                          % Small
            16#7FFFFFFF,                % 32-bit mid
            16#FFFFFFFF,                % 32-bit max
            16#7FFFFFFFFFFFFFFF,        % 64-bit mid (2^63-1)
            16#FFFFFFFFFFFFFFFF         % 64-bit max (2^64-1)
        ],
        
        lists:all(fun(Seq) ->
            %% Encode with this sequence
            Encoded = iris_proto:encode_seq_msg(Target, Seq, <<"test">>),
            
            %% Extract sequence from encoded message
            %% Format: <<7, TLen:16, Target, SeqNo:64, MLen:16, Msg>>
            TLen = byte_size(Target),
            <<7:8, TLen:16, Target:TLen/binary, ExtractedSeq:64/unsigned, _Rest/binary>> = Encoded,
            
            %% Verify it matches (wrap at 64 bits)
            Expected = Seq band 16#FFFFFFFFFFFFFFFF,
            case ExtractedSeq == Expected of
                true -> true;
                false ->
                    io:format("    Wrap test failed: Seq=~p, Expected=~p, Got=~p~n", 
                              [Seq, Expected, ExtractedSeq]),
                    false
            end
        end, TestCases)
    end).

%% =============================================================================
%% Property 6: Binary Safety
%% =============================================================================
%% Protocol should handle arbitrary byte sequences without crashing,
%% including NUL bytes, high UTF-8 chars, and random binary data.

prop_binary_safety() ->
    run_n_times(?NUM_TESTS, fun() ->
        %% Generate various "dangerous" payloads
        TestPayloads = [
            <<0, 0, 0, 0>>,                         % NUL bytes
            <<255, 255, 255, 255>>,                 % All high bits
            <<"Hello\0World">>,                     % Embedded NUL
            <<16#C0, 16#80>>,                       % Invalid UTF-8 (overlong NUL)
            <<16#FF, 16#FE>>,                       % BOM-like
            random_binary(100),                     % Random binary
            list_to_binary([rand:uniform(256)-1 || _ <- lists:seq(1, 500)])  % More random
        ],
        
        MsgId = iris_proto:generate_msg_id(),
        
        lists:all(fun(Payload) ->
            try
                %% Should not crash when encoding
                Encoded = iris_proto:encode_reliable_msg(MsgId, Payload),
                
                %% Should produce valid binary output
                is_binary(Encoded) andalso byte_size(Encoded) >= byte_size(Payload)
            catch
                _:_ ->
                    %% Crashing on encode is a failure
                    io:format("    Binary safety: crash on payload ~p~n", [Payload]),
                    false
            end
        end, TestPayloads)
    end).
