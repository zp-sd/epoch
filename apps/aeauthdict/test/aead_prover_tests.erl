%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_miner
%%% @end
%%%=============================================================================
-module(aead_prover_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("include/ad.hrl").

-define(TEST_MODULE, aead_prover).

prover_test_() ->
    {foreach,
     fun() ->
             crypto:start(),
             {ok, _} = ?TEST_MODULE:start_link()
     end,
     fun(_) ->
             ?TEST_MODULE:stop(),
             crypto:stop()
     end,
     [
      {"Insert and lookup a node",
               fun() ->
                       Key = aec_sha256:hash("samplekey"),
                       Value = 8,
                       Insert = aead_operation:insert(Key, Value),
                       ?assertEqual({ok, none}, ?TEST_MODULE:perform(Insert)),
                       %% Lookup = aead_operation:lookup(Key),
                       %% ?assertEqual({ok, {some, Value}}, ?TEST_MODULE:perform(Lookup)),
                       ?assertEqual({some, Value}, ?TEST_MODULE:unauthenticated_lookup(Key)),

                       ?debugFmt("Built tree: ~s~n", [?TEST_MODULE:top_node_to_string()]),
                       ?debugFmt("removing key ~p~n", [base64:encode(Key)]),
                       Remove = aead_operation:remove(Key),
                       ?assertEqual({ok, {some, Value}}, ?TEST_MODULE:perform(Remove)),
                       ?assertEqual(none, ?TEST_MODULE:unauthenticated_lookup(Key))
               end},
      {"Insert and lookup or delete several nodes",
       {timeout, 60,
        fun() ->
                NumKV = 4, %%crypto:rand_uniform(1, 40),
                KVs = lists:zip(random_kv(NumKV), [2,2,2,3]),

                [begin
                     ?debugFmt("~n-------------------------------------------"
                               "~nInserting ~p...~n", [{base64:encode(Key), Value}]),
                     Insert = aead_operation:insert(Key, Value),
                     ?assertEqual({ok, none}, ?TEST_MODULE:perform(Insert)),
                     ?debugFmt("Inserted pair {~p, ~p}~n", [base64:encode(Key), Value]),
                     ?debugFmt("Built tree: ~s~n", [?TEST_MODULE:top_node_to_string()]),

                     %% case crypto:rand_uniform(1, 4) of
                     case Op of
                         1 ->
                             Lookup = aead_operation:lookup(Key),
                             Lup = ?TEST_MODULE:perform(Lookup),
                             ?debugFmt("Looked up ~p: ~p~n", [base64:encode(Key), Lup]),
                             ?assertEqual({ok, {some, Value}}, Lup);
                         2 ->
                             ?assertEqual({some, Value}, ?TEST_MODULE:unauthenticated_lookup(Key));
                         3 ->
                             ?debugFmt("Removing key ~p~n", [base64:encode(Key)]),
                             Remove = aead_operation:remove(Key),
                             ?assertEqual({ok, {some, Value}}, ?TEST_MODULE:perform(Remove)),
                             ?assertEqual(none, ?TEST_MODULE:unauthenticated_lookup(Key))
                     end
                 end || {{Key, Value}, Op} <- KVs]
       end}
      },
      {"Insert and lookup a node",
       fun() ->
               Key = aec_sha256:hash("samplekey"),
               Value = 8,
               Insert = aead_operation:insert(Key, Value),
               ?assertEqual({ok, none}, ?TEST_MODULE:perform(Insert)),
               ?assertEqual({some, Value}, ?TEST_MODULE:unauthenticated_lookup(Key)),
               ?assert(is_binary(?TEST_MODULE:generate_proof()))
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
