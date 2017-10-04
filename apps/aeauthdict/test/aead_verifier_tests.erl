%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_miner
%%% @end
%%%=============================================================================
-module(aead_verifier_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("include/ad.hrl").

-define(PROVER, aead_prover).
-define(VERIFIER, aead_verifier).

prover_test_() ->
    {foreach,
     fun() ->
             crypto:start(),
             {ok, _} = ?PROVER:start_link()
     end,
     fun(_) ->
             ?PROVER:stop(),
             ?VERIFIER:stop(),
             crypto:stop()
     end,
     [
      {"Insert a node and verify proof",
       fun() ->
               %% Insert a node
               [{K1, V1}, {K2, V2}] = random_kv(2),
               ?debugFmt("Inserting ~p...~n", [{base64:encode(K1), V1}]),
               Insert1 = aead_operation:insert(K1, V1),
               ?assertEqual({ok, none}, ?PROVER:perform(Insert1)),
               _Proof1 = ?PROVER:generate_proof(),
               ?assertEqual({some, V1}, ?PROVER:unauthenticated_lookup(K1)),

               %% Insert another node
               Digest1 = ?PROVER:digest(),
               ?debugFmt("Inserting ~p...~n", [{base64:encode(K2), V2}]),
               Insert2 = aead_operation:insert(K2, V2),
               ?assertEqual({ok, none}, ?PROVER:perform(Insert2)),
               ?assertEqual({some, V2}, ?PROVER:unauthenticated_lookup(K2)),
               Proof2 = ?PROVER:generate_proof(),
               ?assert(is_binary(Proof2)),

               %% start verifier from the digest before the 2nd insertion,
               %% let it read in the proof
               ?debugFmt("launching prover~n", []),
               ?assertMatch({ok, _}, ?VERIFIER:start_link(Digest1, Proof2)),

               ?assertEqual({ok, none}, ?VERIFIER:perform(Insert2))
       end}
     ]}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

random_key() ->
    V = random_value(),
    aec_sha256:hash(V).

random_kv(N) ->
    random_kv(N, []).

random_kv(0, Acc) ->
    Acc;
random_kv(N, Acc) when N > 0 ->
    random_kv(N - 1, [{random_key(), random_value()} | Acc]).

random_value() ->
    crypto:rand_uniform(1, 16#100000000).

-endif.
