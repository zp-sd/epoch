%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity
%%% @doc
%%%     Service to hold merkle tree of state
%%% @end
%%% Created : 20 Sep 2017
%%%-------------------------------------------------------------------
-module(aec_state).

-behaviour(gen_server).

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

%% API
-export([start_link/0, stop/0]).

%% API designed for chain service
-export([get_trees/0,
         apply_txs/2,
         force_trees/2,
         async_check_chain_for_successor/0]).

-export([
    empty_state/0,
    rebuild_from_genesis/0]).

%% API to support consequences of allowing blocks out of order
-export([check_chain_for_successor/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_CALL_TIMEOUT, infinity).

-record(state, {trees  :: trees(),
                height = ?GENESIS_HEIGHT :: height()}).

-define(PRE_GENESIS_HEIGHT, ?GENESIS_HEIGHT-1).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_trees() -> {ok, {height(), trees()}}.
get_trees() ->
    gen_server:call(?SERVER, get_trees, ?DEFAULT_CALL_TIMEOUT).

-spec apply_txs(list(), height()) -> {ok, {height(), trees()}} | {{error, atom()}, trees()}.
apply_txs(Txs, AtHeight) ->
    gen_server:call(?SERVER, {apply_txs, {Txs, AtHeight}}, ?DEFAULT_CALL_TIMEOUT).

%% API needed when external fork has more POW and we need to restart tree from common ancestor
-spec force_trees(trees(), integer()) -> {ok, {height(), trees()}}.
force_trees(Trees, AtHeight) ->
    gen_server:call(?SERVER, {force_trees, {Trees, AtHeight}}, ?DEFAULT_CALL_TIMEOUT).

-spec async_check_chain_for_successor() -> ok.
async_check_chain_for_successor() ->
    gen_server:cast(?SERVER, {check_chain_for_successor}).

-spec empty_state() -> {ok, {height(), trees()}}.
empty_state() ->
    %% Initialize state service to pre-genesis state, handy for testing
    {ok, EmptyTrees} = aec_trees:all_trees_new(),
    force_trees(EmptyTrees, ?PRE_GENESIS_HEIGHT).

-spec rebuild_from_genesis() -> {ok, {height(), trees()}}.
rebuild_from_genesis() ->
    {ok, {TopHeight, TreesAtTopHeight}} = rebuild_state_from_chain(),
    force_trees(TreesAtTopHeight, TopHeight).

stop() ->
    gen_server:stop(?SERVER).

-spec check_chain_for_successor(trees(), height()) -> {trees(), height()}.
check_chain_for_successor(Trees, AtHeight) ->
    SuccessorHeight = AtHeight+1,
    case aec_chain:get_block_by_height(SuccessorHeight) of
        {ok, Block} ->
            {ok, TreesUpdated} = ?MODULE:apply_txs(aec_blocks:txs(Block), SuccessorHeight),
            check_chain_for_successor(TreesUpdated, SuccessorHeight);
        {error, {_, _}} -> %% block_not_found, chain_too_short
            {Trees, AtHeight}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, {TopHeight, TreesAtTopHeight}} = rebuild_state_from_chain(),

    process_flag(trap_exit, true),

    {ok, #state{trees = TreesAtTopHeight, height = TopHeight}}.

handle_call(get_trees, _From, #state{trees = Trees, height = Height} = State) ->
    {reply, {ok, {Height, Trees}}, State};

handle_call({force_trees, {Trees, AtHeight}}, _From, State) ->
    {reply, {ok, {AtHeight, Trees}}, State#state{trees = Trees, height = AtHeight}};

handle_call({apply_txs, {Txs, AtHeight}}, _From,
            #state{trees = Trees, height = CurrentHeigth} = State) ->
    {Reply, TreesUpdated, HeightUpdated}
        = case validate_height(CurrentHeigth, AtHeight) of
              true ->
                  {ok, TreesUpdated0} = apply_txs_internal(Txs, Trees, AtHeight),
                  spawn_link(?MODULE, check_chain_for_successor, [TreesUpdated0, AtHeight]),
                  {ok, TreesUpdated0, AtHeight};
              false ->
                  {{error, not_next_block}, Trees, CurrentHeigth}
          end,
    {reply, {Reply, {HeightUpdated, TreesUpdated}},
        State#state{trees=TreesUpdated, height = HeightUpdated}}.

handle_cast({check_chain_for_successor},
            #state{trees = Trees, height = Height} = State) ->
    spawn_link(?MODULE, check_chain_for_successor, [Trees, Height]),
    {noreply, State}.

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:info("FAILED: Block successor check crashed ~p ~p at ~p",
               [Pid, Reason, State#state.height]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec rebuild_state_from_chain() -> {ok, {height(), trees()}}.
rebuild_state_from_chain() ->
    {ok, TopBlock} = aec_chain:top_block(),
    TopHeight = aec_blocks:height(TopBlock),
    GenesisTrees = aec_blocks:trees(aec_block_genesis:genesis_block()),
    {ok, TreesAtTopHeight} = setup_trees(TopHeight, GenesisTrees),
    {ok, {TopHeight, TreesAtTopHeight}}.

%% INFO: not optimized (by local lookup of prev-block by hash in current-block)
%%       because of incoming optimization with check points
-spec setup_trees(non_neg_integer(), trees()) -> {ok, trees()}.
setup_trees(0, Trees) ->
    {ok, Trees};
setup_trees(N, Trees) ->
    do_setup_trees(0, N, Trees).

-spec do_setup_trees(non_neg_integer(), non_neg_integer(), trees()) -> {ok, trees()} | {error, term()}.
do_setup_trees(TopHeight, TopHeight, Trees) ->
    {ok, Trees};
do_setup_trees(CurrentHeight, TopHeight, TreesAtEndOfPreviousBlock) ->
    case aec_chain:get_block_by_height(CurrentHeight) of
        {ok, Block} ->
            Txs = aec_blocks:txs(Block),
            case apply_txs_internal(Txs, TreesAtEndOfPreviousBlock, CurrentHeight) of
                {ok, TreesUpdated} ->
                    do_setup_trees(CurrentHeight+1, TopHeight, TreesUpdated);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.



-spec apply_txs_internal(list(), trees(),  non_neg_integer()) -> {ok, trees()} | {error, term()}.
apply_txs_internal([], Trees, _) ->
    {ok, Trees};
apply_txs_internal(Txs, Trees, AtHeight) ->
    aec_tx:apply_signed(Txs, Trees, AtHeight).

%% TODO: introduce validation by hash (to cover forks with more PoW)
-spec validate_height(non_neg_integer(), height()) -> boolean().
validate_height(CurrentHeigth, AtHeight) ->
    CurrentHeigth + 1 == AtHeight.


