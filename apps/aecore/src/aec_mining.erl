-module(aec_mining).

%% API
-export([mine/0,
         mine/1]).

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(DEFAULT_MINE_ATTEMPTS_COUNT, 10).
-define(ATTEMPTS_BATCH_SIZE, 10000).
-define(TIME_DELTA_TO_FETCH_NEW_TXS, 30000). %% 30 seconds

%% API

-spec mine() -> {ok, block()} | {error, term()}.
mine() ->
    mine(?DEFAULT_MINE_ATTEMPTS_COUNT).

-spec mine(non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Attempts) ->
    {ok, LastBlock} = aec_blocks:top(),
    Trees = aec_blocks:trees(LastBlock),
    Txs = get_txs_to_mine(Trees),
    {ok, Block0} = aec_blocks:new(LastBlock, Txs, Trees),
    Block = maybe_recalculate_difficulty(Block0),
    case mine(Block, Attempts) of
        {ok, _Block} = Ok ->
            Ok;
        {error, _Reason} = Error ->
            Error
    end.

%% Internal functions

-spec get_txs_to_mine(trees()) -> list(signed_tx()).
get_txs_to_mine(Trees) ->
    {ok, Txs0} = aec_tx_pool:get_txs_to_mine(),
    {ok, CoinbaseTx} = create_coinbase_tx(Trees),
    {ok, SignedCoinbaseTx} = aec_keys:sign(CoinbaseTx),
    [SignedCoinbaseTx | Txs0].

-spec create_coinbase_tx(trees()) -> {ok, coinbase_tx()}.
create_coinbase_tx(Trees) ->
    {ok, Pubkey} = aec_keys:pubkey(),
    {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => Pubkey}, Trees),
    {ok, CoinbaseTx}.

-spec maybe_recalculate_difficulty(block()) -> block().
maybe_recalculate_difficulty(Block) ->
    Height = aec_blocks:height(Block),
    case should_recalculate_difficulty(Height) of
        true ->
            %% Recalculate difficulty based on mining rate of N last blocks,
            %% where N = recalculate_difficulty_frequency.
            BlocksToCheckCount = aec_governance:recalculate_difficulty_frequency(),
            Difficulty = calculate_difficulty(Block, BlocksToCheckCount),
            Block#block{difficulty = Difficulty};
        false ->
            Block
    end.

-spec should_recalculate_difficulty(non_neg_integer()) -> boolean().
should_recalculate_difficulty(Height) ->
    RecalculateDifficultyFrequency = aec_governance:recalculate_difficulty_frequency(),
    (Height > 10) andalso %% do not change difficulty for the first 10 blocks
                    (Height > RecalculateDifficultyFrequency)
        andalso (0 == (Height rem RecalculateDifficultyFrequency)).

-spec calculate_difficulty(block(), non_neg_integer()) -> non_neg_integer().
calculate_difficulty(NewBlock, BlocksToCheckCount) ->
    CurrentDifficulty = NewBlock#block.difficulty,
    CurrentRate = get_current_rate(NewBlock, BlocksToCheckCount),
    ExpectedRate = aec_governance:expected_block_mine_rate(),
    aec_pow_sha256:recalculate_difficulty(CurrentDifficulty, ExpectedRate, CurrentRate).

-spec get_current_rate(block(), non_neg_integer()) -> non_neg_integer().
get_current_rate(Block, BlocksToCheckCount) ->
    BlockHeader = aec_blocks:to_header(Block),
    BlockHeight = aec_blocks:height(Block),

    FirstBlockHeight = BlockHeight - BlocksToCheckCount,
    {ok, FirstBlockHeader} = aec_headers:get_by_height(FirstBlockHeight),

    mining_rate_between_blocks(BlockHeader, FirstBlockHeader, BlocksToCheckCount).

-spec mining_rate_between_blocks(header(), header(), non_neg_integer()) -> non_neg_integer().
mining_rate_between_blocks(Block1Header, Block2Header, BlocksMinedCount) ->
    Time1 = aec_headers:time_in_secs(Block1Header),
    Time2 = aec_headers:time_in_secs(Block2Header),
    TimeDiff = Time1 - Time2,
    TimeDiff div BlocksMinedCount.


-spec mine(block(), non_neg_integer()) -> {ok, block()} | {error, term()}.
mine(Block, Attempts) ->
    BlockTime = aec_blocks:time(Block),
    TimeDeltaToFetchNewTxs = application:get_env(epoch, time_delta_to_fetch_new_txs, ?TIME_DELTA_TO_FETCH_NEW_TXS),
    NextTxsFetchTime = BlockTime + TimeDeltaToFetchNewTxs,
    BlockHash = aec_sha256:hash(Block),
    mine(Block, BlockHash, random, NextTxsFetchTime, TimeDeltaToFetchNewTxs, Attempts).

-spec mine(block(), binary(), non_neg_integer() | random, non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
                  {ok, block()} | {error, term()}.
mine(_Block, _BlockHash, _Nonce, _NextTxsFetchTime, _TimeDeltaToFetchNewTxs, 0) ->
    {error, generation_count_exhausted};
mine(Block, BlockHash, Nonce0, _NextTxsFetchTime, _TimeDeltaToFetchNewTxs, Attempts)
  when Attempts < ?ATTEMPTS_BATCH_SIZE ->
    case generate_pow(Block, BlockHash, Nonce0, Attempts) of
        {ok, Nonce} ->
            {ok, aec_blocks:set_nonce(Block, Nonce)};
        {error, generation_count_exhausted, _Nonce} ->
            {error, generation_count_exhausted}
    end;
mine(Block0, BlockHash0, Nonce0, NextTxsFetchTime0, TimeDeltaToFetchNewTxs, Attempts) ->
    case generate_pow(Block0, BlockHash0, Nonce0, Attempts) of
        {ok, Nonce} ->
            {ok, aec_blocks:set_nonce(Block0, Nonce)};
        {error, generation_count_exhausted, Nonce} ->
            {ok, {Block, BlockHash, NewNonce, NextTxsFetchTime}} =
                maybe_add_more_txs_to_block(Block0, BlockHash0, Nonce,
                                            NextTxsFetchTime0, TimeDeltaToFetchNewTxs),
            mine(Block, BlockHash, NewNonce,
                 NextTxsFetchTime, TimeDeltaToFetchNewTxs, Attempts - ?ATTEMPTS_BATCH_SIZE)
    end.

-spec generate_pow(block(), binary(), non_neg_integer() | random, non_neg_integer()) ->
                          {ok, integer()} | {error, generation_count_exhausted}.
generate_pow(Block, BlockHash, random, Attempts) ->
    Difficulty = aec_blocks:difficulty(Block),
    aec_pow_sha256:generate(BlockHash, Difficulty, Attempts);
generate_pow(Block, BlockHash, Nonce, Attempts) ->
    Difficulty = aec_blocks:difficulty(Block),
    aec_pow_sha256:generate(BlockHash, Difficulty, Nonce, Attempts).

-spec maybe_add_more_txs_to_block(block(), binary(), non_neg_integer() | random, non_neg_integer(), non_neg_integer()) ->
                                         {ok, {block(), binary(), non_neg_integer() | random, non_neg_integer()}}.
maybe_add_more_txs_to_block(Block0, BlockHash0, Nonce0, NextTxsFetchTime0, TimeDeltaToFetchNewTxs) ->
    case should_try_to_fetch_new_txs(NextTxsFetchTime0) of
        true ->
            NextTxFetchTime = NextTxsFetchTime0 + TimeDeltaToFetchNewTxs,
            {ok, {Block, BlockHash, Nonce}} = add_more_txs_to_block(Block0, BlockHash0, Nonce0),
            {ok, {Block, BlockHash, Nonce, NextTxFetchTime}};
        false ->
            {ok, {Block0, BlockHash0, Nonce0, NextTxsFetchTime0}}
    end.

-spec should_try_to_fetch_new_txs(non_neg_integer()) -> boolean().
should_try_to_fetch_new_txs(NextTxsFetchTime) ->
    aeu_time:now_in_msecs() > NextTxsFetchTime.

-spec add_more_txs_to_block(block(), binary(), non_neg_integer() | random) ->
                                   {ok, {block(), binary(), non_neg_integer() | random}}.
add_more_txs_to_block(Block, BlockHash, Nonce) ->
    case aec_tx_pool:has_pending_txs() of
        true ->
            {ok, {NewBlock, NewBlockHash}} = apply_pending_txs(Block),
            {ok, {NewBlock, NewBlockHash, random}};
        false ->
            {ok, {Block, BlockHash, Nonce}}
    end.

-spec apply_pending_txs(block()) -> {ok, {block(), binary()}}.
apply_pending_txs(Block) ->
    {ok, Txs} = aec_tx_pool:get_pending_txs(),
    {ok, NewBlock} = aec_blocks:apply_additional_txs(Block, Txs),
    NewBlockHash = aec_sha256:hash(NewBlock),
    {ok, {NewBlock, NewBlockHash}}.
