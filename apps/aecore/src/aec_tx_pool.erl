-module(aec_tx_pool).

%% API
-export([get_txs_to_mine/0,
         has_pending_txs/0,
         get_pending_txs/0]).

get_txs_to_mine() ->
    %% TODO: return signed txs from mempool
    %% Note that some users may want to selectively choose txs for a block
    {ok, []}.

has_pending_txs() ->
    %% TODO: check if there are any new txs that can be added for mining
    case rand:uniform(2) of
        1 ->
            true;
        2 ->
            false
    end.

get_pending_txs() ->
    %% TODO: get pending txs that can be added for mining
    {ok, []}.
