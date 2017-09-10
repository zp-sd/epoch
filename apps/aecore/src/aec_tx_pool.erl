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
-spec all() -> list(signed_tx()).
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
-spec add(signed_tx()) -> ok | {error, string()}.
add(SignedTx) ->
    case aec_tx_sign:verify(SignedTx) of  %TODO export this codeblock to an function. used also in aec_tx
        ok ->
            FeedTx = aec_tx_sign:data(SignedTx),
            case aec_tx_fee:check(FeedTx) of
                ok ->
                    gen_server:cast(?MODULE, {add, SignedTx}),
                    ok
                %{error, _Reason} = Error->
                %    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% Returns gb_tree iterator to current state of mempool - current list of transactions
%%------------------------------------------------------------------------------
-spec get_iterator() -> list(gb_trees:iter(non_pos_integer(), signed_tx())).
get_iterator() ->
    gen_server:call(?MODULE, get_iterator).

%%%=============================================================================
%%% gen_server functions
%%%=============================================================================

-type non_pos_integer() :: neg_integer() | 0.

-record(state, {pool :: gb_trees:gb_trees(non_pos_integer(), signed_tx())}).
%The tree Key is -fee (zero minus fee)

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

handle_cast({add, SignedTx}, State) ->
    MinusFee = 0-aec_tx_fee:fee(aec_tx_sign:data(SignedTx)),
    Hash = aec_tx:hash(SignedTx),
    Key = <<MinusFee:64,Hash/binary>>,
    NewPool = gb_trees:enter(Key, SignedTx, State#state.pool),
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
