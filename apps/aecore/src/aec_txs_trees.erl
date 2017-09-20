-module(aec_txs_trees).

-export([new/1,
         get/2,
         get_with_proof/2,
         put/3,
         root_hash/1,
         verify_proof/4]).

-include("txs.hrl").
-include("common.hrl").
-include("trees.hrl").

-type key() :: aec_trees:hash().

%%%======================================================================
%%% API
%%%======================================================================

-spec new(list(aec_tx:signed_tx())) -> aec_trees:tree().
new(Txs) ->
    {ok, TxsTree} = aec_trees:new(),
    lists:foldl(fun(SignedTx, Acc) ->
                        Tx = aec_tx_sign:data(SignedTx),
                        Key = aec_sha256:hash(Tx),
                        {ok, Acc2} = put(Key, Tx, Acc),
                        Acc2
                end,
                TxsTree,
                Txs).

-spec get(key(), aec_trees:tree()) -> {'ok', tx()}.
get(Key, TxsTree) ->
    case aec_trees:get(Key, TxsTree) of
        {ok, SerializedTx} when is_binary(SerializedTx) ->
            {ok, deserialize(SerializedTx)};
        {error, notfound} = E ->
            E
    end.

-spec get_with_proof(key(), aec_trees:tree()) ->
                            {'ok', {tx(), aec_trees:proof()}}.
get_with_proof(Key, TxsTree) ->
    case aec_trees:get_with_proof(Key, TxsTree) of
        {ok, {SerializedTx, Proof}} when is_binary(SerializedTx) ->
            {ok, {deserialize(SerializedTx), Proof}};
        {error, notfound} = E ->
            E
    end.

-spec put(key(), tx(), aec_trees:tree()) -> aec_trees:tree().
put(Key, Tx, TxsTree) ->
    {ok, _NewTxsTree} =
        aec_trees:put(Key, serialize(Tx), TxsTree).

-spec root_hash(aec_trees:tree()) -> {'ok', aec_trees:hash()}.
root_hash(TxsTree) ->
    aec_trees:root_hash(TxsTree).

-spec verify_proof(key(), tx(), aec_trees:tree(), aec_trees:proof()) ->
                          {'ok', 'verified'} | {'error', term()}.
verify_proof(Key, Tx, RootHash, Proof) ->
    aec_trees:verify_proof(Key, serialize(Tx), RootHash, Proof).

-spec  serialize(tx()) -> binary().
serialize(Tx) ->
    term_to_binary(Tx).

-spec deserialize(binary()) -> tx().
deserialize(SerializedTx) ->
    binary_to_term(SerializedTx).
