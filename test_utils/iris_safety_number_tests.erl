-module(iris_safety_number_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Safety Number Computation Tests
%%
%% RFC-001-AMENDMENT-001 v1.3 Section 5.3.1:
%% Safety Number = SHA-256(sort(IK_A, IK_B))[:30] displayed as 12 groups
%% of 5 digits (60 digits total), matching Signal's UX pattern.
%%
%% Tests verify:
%% 1. Deterministic output for same input
%% 2. Symmetric: compute(A,B) == compute(B,A)
%% 3. Format: 60-digit string, 12 groups of 5 digits
%% 4. Different keys produce different safety numbers
%% 5. Invalid input rejected
%%
%% Pattern: follows iris_auth_eddsa_tests.erl for setup/cleanup.
%% =============================================================================

setup() ->
    application:ensure_all_started(crypto),
    ok.

cleanup(_) ->
    ok.

%% =============================================================================
%% Test Generator
%% =============================================================================

iris_safety_number_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Safety number is deterministic", fun test_safety_number_deterministic/0},
      {"Safety number is symmetric", fun test_safety_number_symmetric/0},
      {"Safety number format: 60 digits, 12 groups of 5", fun test_safety_number_format/0},
      {"Different keys produce different safety numbers", fun test_safety_number_different_keys_differ/0},
      {"Invalid input rejected", fun test_safety_number_invalid_input/0}
     ]}.

%% =============================================================================
%% Tests
%% =============================================================================

test_safety_number_deterministic() ->
    IK_A = crypto:strong_rand_bytes(32),
    IK_B = crypto:strong_rand_bytes(32),
    {ok, SN1} = iris_keys:compute_safety_number(IK_A, IK_B),
    {ok, SN2} = iris_keys:compute_safety_number(IK_A, IK_B),
    ?assertEqual(SN1, SN2).

test_safety_number_symmetric() ->
    IK_A = crypto:strong_rand_bytes(32),
    IK_B = crypto:strong_rand_bytes(32),
    {ok, SN_AB} = iris_keys:compute_safety_number(IK_A, IK_B),
    {ok, SN_BA} = iris_keys:compute_safety_number(IK_B, IK_A),
    ?assertEqual(SN_AB, SN_BA).

test_safety_number_format() ->
    IK_A = crypto:strong_rand_bytes(32),
    IK_B = crypto:strong_rand_bytes(32),
    {ok, SN} = iris_keys:compute_safety_number(IK_A, IK_B),
    %% SN should be a binary string
    ?assert(is_binary(SN)),
    %% Remove spaces to get raw digits
    Digits = binary:replace(SN, <<" ">>, <<>>, [global]),
    ?assertEqual(60, byte_size(Digits)),
    %% All characters should be digits 0-9
    lists:foreach(fun(C) ->
        ?assert(C >= $0 andalso C =< $9)
    end, binary_to_list(Digits)),
    %% Should be 12 groups of 5 digits separated by spaces
    Groups = binary:split(SN, <<" ">>, [global]),
    ?assertEqual(12, length(Groups)),
    lists:foreach(fun(G) ->
        ?assertEqual(5, byte_size(G))
    end, Groups).

test_safety_number_different_keys_differ() ->
    IK_A = crypto:strong_rand_bytes(32),
    IK_B = crypto:strong_rand_bytes(32),
    IK_C = crypto:strong_rand_bytes(32),
    {ok, SN_AB} = iris_keys:compute_safety_number(IK_A, IK_B),
    {ok, SN_AC} = iris_keys:compute_safety_number(IK_A, IK_C),
    ?assertNotEqual(SN_AB, SN_AC).

test_safety_number_invalid_input() ->
    ?assertEqual({error, invalid_key}, iris_keys:compute_safety_number(<<>>, <<>>)),
    ?assertEqual({error, invalid_key}, iris_keys:compute_safety_number(<<"short">>, <<"short">>)).
