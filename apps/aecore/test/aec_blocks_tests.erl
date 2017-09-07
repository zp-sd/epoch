-module(aec_blocks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_blocks).

new_block_test_() ->
    {"Generate new block with given txs and 0 nonce",
     fun() ->
             PrevBlock = #block{height = 11, difficulty = 17},
             BlockHeader = ?TEST_MODULE:to_header(PrevBlock),

             {ok, NewBlock} = ?TEST_MODULE:new(PrevBlock, [], #trees{}),

             ?assertEqual(12, NewBlock#block.height),
             ?assertEqual(aec_sha256:hash(BlockHeader), NewBlock#block.prev_hash),
             ?assertEqual([], NewBlock#block.txs),
             ?assertEqual(17, NewBlock#block.difficulty),
             ?assertEqual(1, NewBlock#block.version)
     end
    }.

apply_additional_txs_test_() ->
    {setup,
     fun() ->
             meck:new(aec_tx),
             meck:expect(aec_tx, apply_signed, 3, {ok, #trees{accounts = updated_accounts}})
     end,
     fun(_) ->
             meck:unload(aec_tx)
     end,
     [{"Apply new txs to existing block",
       fun() ->
               PrevHash = <<"prev_hash">>,
               RootHash = <<"root_hash">>,
               Trees = #trees{accounts = fake_accounts_tree},
               Txs = [tx_a, tx_b],
               Block0 = #block{height = 11,
                               prev_hash = PrevHash,
                               root_hash = RootHash,
                               trees = Trees,
                               txs = Txs,
                               difficulty = 17,
                               nonce = 15,
                               time = 198293,
                               version = 7},

               {ok, Block} = ?TEST_MODULE:apply_additional_txs(Block0, [new_tx_a, new_tx_b]),

               ?assertEqual(11, Block#block.height),
               ?assertEqual(Block0#block.prev_hash, Block#block.prev_hash),
               ?assertNotEqual(Block0#block.root_hash, Block#block.root_hash),
               ?assertEqual(#trees{accounts = updated_accounts}, Block#block.trees),
               ?assertEqual([new_tx_a, new_tx_b, tx_a, tx_b], Block#block.txs),
               ?assertEqual(15, Block#block.nonce),
               ?assertNotEqual(Block0#block.time, Block#block.time),
               ?assertEqual(7, Block#block.version)
       end}
     ]
    }.

-endif.
