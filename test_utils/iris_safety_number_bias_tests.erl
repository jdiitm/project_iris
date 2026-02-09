-module(iris_safety_number_bias_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% GAP-1: Safety Number Uniformity Tests (Amendment 5.3.1)
%%
%% The safety number is displayed as 12 groups of 5 digits (60 digits total).
%% Each digit-pair is derived from hash bytes. The current implementation uses
%% `B rem 100` where B is a single byte (0-255). Since 256 is not evenly
%% divisible by 100, values 0-55 appear 3/256 times while 56-99 appear 2/256
%% times. This ~1.5x bias violates cryptographic uniformity.
%%
%% This test generates many safety numbers and performs a chi-squared test
%% to detect non-uniform distribution of digit-pairs.
%% =============================================================================

iris_safety_number_bias_test_() ->
    [
     {"Safety number digit-pairs are uniformly distributed",
      {timeout, 30, fun test_digit_pair_uniformity/0}},
     {"Safety number format is 12 groups of 5 digits",
      fun test_safety_number_format/0},
     {"Safety number is deterministic and symmetric",
      fun test_safety_number_deterministic/0}
    ].

test_digit_pair_uniformity() ->
    %% Generate N safety numbers from random key pairs
    N = 500,
    KeyPairs = [{crypto:strong_rand_bytes(32), crypto:strong_rand_bytes(32)} 
                || _ <- lists:seq(1, N)],
    
    %% Collect all digit-pairs (each safety number has 30 digit-pairs)
    AllPairs = lists:flatmap(fun({IK_A, IK_B}) ->
        {ok, SN} = iris_keys:compute_safety_number(IK_A, IK_B),
        extract_digit_pairs(SN)
    end, KeyPairs),
    
    %% Count frequency of each digit-pair (00-99)
    Counts = lists:foldl(fun(Pair, Acc) ->
        maps:update_with(Pair, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, AllPairs),
    
    %% Chi-squared test for uniformity.
    %% Expected count per digit-pair: TotalPairs / 100
    TotalPairs = length(AllPairs),
    Expected = TotalPairs / 100.0,
    
    ChiSquared = lists:foldl(fun(DigitPair, Acc) ->
        Observed = maps:get(DigitPair, Counts, 0),
        Acc + math:pow(Observed - Expected, 2) / Expected
    end, 0.0, lists:seq(0, 99)),
    
    %% Chi-squared critical value for 99 degrees of freedom at p=0.001
    %% (very conservative -- only fails for severe bias).
    %% Critical value ~148.23 for df=99, alpha=0.001
    CriticalValue = 148.23,
    
    ?assert(ChiSquared < CriticalValue),
    ok.

test_safety_number_format() ->
    IK_A = crypto:strong_rand_bytes(32),
    IK_B = crypto:strong_rand_bytes(32),
    {ok, SN} = iris_keys:compute_safety_number(IK_A, IK_B),
    
    %% Should be 12 groups of 5 digits separated by spaces
    %% Total length: 12*5 + 11 spaces = 71 bytes
    ?assertEqual(71, byte_size(SN)),
    
    Groups = binary:split(SN, <<" ">>, [global]),
    ?assertEqual(12, length(Groups)),
    
    %% Each group should be exactly 5 digits
    lists:foreach(fun(Group) ->
        ?assertEqual(5, byte_size(Group)),
        %% All characters should be digits 0-9
        lists:foreach(fun(C) ->
            ?assert(C >= $0 andalso C =< $9)
        end, binary_to_list(Group))
    end, Groups).

test_safety_number_deterministic() ->
    IK_A = crypto:strong_rand_bytes(32),
    IK_B = crypto:strong_rand_bytes(32),
    
    %% Same inputs -> same output
    {ok, SN1} = iris_keys:compute_safety_number(IK_A, IK_B),
    {ok, SN2} = iris_keys:compute_safety_number(IK_A, IK_B),
    ?assertEqual(SN1, SN2),
    
    %% Order doesn't matter (symmetric)
    {ok, SN3} = iris_keys:compute_safety_number(IK_B, IK_A),
    ?assertEqual(SN1, SN3).

%% Extract digit-pairs from a safety number string.
%% "12345 67890 ..." -> [12, 34, 56, 78, 90, ...]
extract_digit_pairs(SN) ->
    %% Remove spaces
    Digits = binary:replace(SN, <<" ">>, <<>>, [global]),
    extract_pairs(Digits, []).

extract_pairs(<<A, B, Rest/binary>>, Acc) ->
    Pair = (A - $0) * 10 + (B - $0),
    extract_pairs(Rest, [Pair | Acc]);
extract_pairs(<<>>, Acc) ->
    lists:reverse(Acc).
