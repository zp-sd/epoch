%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module storing peers list and providing funtionc for peers interaction
%%% @end
%%%=============================================================================
-module(aec_tx_pool).

-behaviour(gen_server).

%% API
-export([all/0,
         clear/0,
         add/1,
         remove/1,
         get_iterator/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("common.hrl").
-include("txs.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Returns list of all transactions. Ordered from highest to lowest fee.
%%------------------------------------------------------------------------------
-spec all() -> list(full_tx()).
all() ->
    gen_server:call(?MODULE, all).

%%------------------------------------------------------------------------------
%% Removes all transactions from the tx_pool.
%%------------------------------------------------------------------------------
-spec clear() -> ok.
clear() ->
    gen_server:cast(?MODULE, clear),
    ok.

%%------------------------------------------------------------------------------
%% Verify and add transaction to mempool. Performs only signature and fee verification
%%------------------------------------------------------------------------------
-spec add(full_tx()) -> ok | {error, string()}.
add(FullTx) ->
    case aec_tx_sign:verify(FullTx) of
        ok ->
            gen_server:cast(?MODULE, {add, FullTx}),
            ok;
        {error, _Reason} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% Remove transaction or a list of transactions from mempool.
%% Usecase: We received a new block and we want to remove transactions from that block from our pending set.
%%------------------------------------------------------------------------------
-spec remove(full_tx() | list(full_tx())) -> ok.
remove([]) ->
    ok;
remove(List) when is_list(List) ->
    gen_server:cast(?MODULE, {remove, List}),
    ok;
remove(FullTx) ->
    remove([FullTx]).

%%------------------------------------------------------------------------------
%% Returns gb_tree iterator to current state of mempool - current list of transactions
%%------------------------------------------------------------------------------
-spec get_iterator() -> list(gb_trees:iter(non_pos_integer(), full_tx())).
get_iterator() ->
    gen_server:call(?MODULE, get_iterator).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-type non_pos_integer() :: neg_integer() | 0.

-record(state, {pool :: gb_trees:tree(binary(), full_tx())}).
%The tree Key is -fee (zero minus fee) in binary 64bit + Tx hash

start_link() ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, ok, []).

init(ok) ->
    {ok, #state{pool=gb_trees:empty()}}.

handle_call(all, _From, State) ->
    {reply, gb_trees:values(State#state.pool), State};

handle_call(get_iterator, _From, State) ->
    {reply, gb_trees:iterator(State#state.pool), State}.

handle_cast(clear, State) ->
    {noreply, State#state{pool=gb_trees:empty()}};

handle_cast({add, FullTx}, State) ->
    Key = get_tx_key(FullTx),
    NewPool = gb_trees:enter(Key, FullTx, State#state.pool),
    {noreply, State#state{pool=NewPool}};

handle_cast({remove, List}, State) ->
    NewPool = remove_txs_from_set(List, State#state.pool),
    {noreply, State#state{pool=NewPool}}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec get_tx_key(full_tx()) -> binary().
get_tx_key(FullTx) ->
    MinusFee = 0-aec_tx_full:fee(aec_tx_sign:data(FullTx)),
    Hash = aec_tx:hash(FullTx),
    <<MinusFee:64,Hash/binary>>.

-spec remove_txs_from_set(list(full_tx()), gb_trees:tree(binary(), full_tx())) -> gb_trees:tree(binary(), full_tx()).
remove_txs_from_set([], GbTree) ->
    GbTree;
remove_txs_from_set([Tx | RestTxs], GbTree) ->
    NewGbTree = gb_trees:delete(get_tx_key(Tx), GbTree),
    remove_txs_from_set(RestTxs, NewGbTree).
