-module(aec_blocks).

%% API
-export([height/1,
         trees/1,
         difficulty/1,
         time/1,
         set_nonce/2,
         top/0,
         new/3,
         apply_additional_txs/2,
         to_header/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(CURRENT_BLOCK_VERSION, 1).

height(Block) ->
    Block#block.height.

trees(Block) ->
    Block#block.trees.

difficulty(Block) ->
    Block#block.difficulty.

time(Block) ->
    Block#block.time.

txs(Block) ->
    Block#block.txs.

set_nonce(Block, Nonce) ->
    Block#block{nonce = Nonce}.

top() ->
    %% TODO: fetch the most recent block from storage
    %% and transform it to #block{} record
    {ok, #block{}}.

-spec new(block(), list(signed_tx()), trees()) -> {ok, block()}.
new(LastBlock, Txs, Trees0) ->
    LastBlockHeight = height(LastBlock),
    LastBlockHeader = to_header(LastBlock),
    Height = LastBlockHeight + 1,
    {ok, Trees} = aec_tx:apply_signed(Txs, Trees0, Height),
    {ok, #block{height = Height,
                prev_hash = aec_sha256:hash(LastBlockHeader),
                root_hash = aec_trees:all_trees_hash(Trees),
                trees = Trees,
                txs = Txs,
                difficulty = difficulty(LastBlock),
                time = aeu_time:now_in_msecs(),
                version = ?CURRENT_BLOCK_VERSION}}.

-spec apply_additional_txs(block(), list(signed_tx())) -> {ok, block()}.
apply_additional_txs(Block, Txs) ->
    BlockTxs = txs(Block),
    Trees0 = trees(Block),
    Height = height(Block),
    {ok, Trees} = aec_tx:apply_signed(Txs, Trees0, Height),
    {ok, Block#block{root_hash = aec_trees:all_trees_hash(Trees),
                     trees = Trees,
                     txs = Txs ++ BlockTxs,
                     time = aeu_time:now_in_msecs()}}.

-spec to_header(block()) -> header().
to_header(#block{height = Height,
                 prev_hash = PrevHash,
                 root_hash = RootHash,
                 difficulty = Difficulty,
                 nonce = Nonce,
                 time = Time,
                 version = Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            difficulty = Difficulty,
            nonce = Nonce,
            time = Time,
            version = Version}.
