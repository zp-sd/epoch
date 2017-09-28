-module(aec_tx_pool_tests).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
     fun() -> {ok, _} = aec_tx_pool:start_link(), ok end,
     fun(_) -> ok = aec_tx_pool:stop() end,
     [{"No txs in mempool",
       fun() ->
               {ok, []} = aec_tx_pool:pop(1),
               {ok, []} = aec_tx_pool:pop(5),

               {ok, []} = aec_tx_pool:peek(1),
               {ok, []} = aec_tx_pool:peek(3),

               ok = aec_tx_pool:delete([])
       end},
      {"TODO",
      fun() ->
              todo
      end}]}.

%% Tx = aec_tx_sign:data(SignedTx)
%% aec_coinbase_tx:new(#{account := <<>>}, unused_trees})


%% * `push/1`

%%   * Push some transactions when received on the network;

%%   * Push some transactions when block that used to be in longest chain
%%     is orphaned;

%%   * Push some transactions when aborting mining attempt (in order to
%%     then pop transactions for starting new mining attempt).
%%     * This is a potential minor optimization.

%% * `pop/1`

%%   * Pop (i.e. read and delete) a certain number of transactions when
%%     attempting mining.

%% * `delete/1`

%%   * Delete some transactions when block including them included in
%%     chain.

%% * `peek/1`

%%   * Read a certain number of transactions when broadcasting
%%     transactions.
