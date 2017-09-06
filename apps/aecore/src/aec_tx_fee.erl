-module(aec_tx_fee).

%% API
-export([tx/1,
         fee/1,
         check/1]).

-include("common.hrl").
-include("txs.hrl").

-spec tx(feed_tx()) -> tx().
tx(FeedTx) ->
    FeedTx#feed_tx.tx.

-spec fee(feed_tx()) -> non_neg_integer().
fee(FeedTx) ->
    FeedTx#feed_tx.fee.

-spec check(feed_tx()) -> atom().
check(_FeedTx) ->
    ok.
