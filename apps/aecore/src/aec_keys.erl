-module(aec_keys).

%% API
-export([pubkey/0,
         sign/1]).

-include("common.hrl").
-include("txs.hrl").

pubkey() ->
    {ok, <<>>}.

sign(Tx) ->
    %% TODO: Return signed tx
    %% Append current key signature to the list of signatures
    {ok, Tx#full_tx{signatures = Tx#full_tx.signatures}}.
