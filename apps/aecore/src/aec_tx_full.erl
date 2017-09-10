-module(aec_tx_full).

%% API
-export([tx/1,
         fee/1,
         verify/1]).

-include("common.hrl").
-include("txs.hrl").

-spec tx(full_tx()) -> tx().
tx(FullTx) ->
    FullTx#full_tx.tx.

-spec fee(full_tx()) -> non_neg_integer().
fee(FeedTx) ->
    FeedTx#full_tx.fee.

verify(failed_tx) ->
    %% TODO: Verify signature and fee
    {error, verification_failed};
verify(_Tx) ->
    ok.
