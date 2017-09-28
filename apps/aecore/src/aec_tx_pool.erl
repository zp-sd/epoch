%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Memory pool of unconfirmed transactions.
%%%
%%% @TODO Sort transactions by fee.
%%% @TODO Limit number of transactions considering memory usage.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_tx_pool).

-behaviour(gen_server).

-include("common.hrl").
-include("txs.hrl").

%% API
-export([start_link/0,
         stop/0]).
-export([push/1,
         pop/1,
         delete/1,
         peek/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% Ensure written.
-spec push([signed_tx(), ...]) -> ok.
push(_) ->
     ok.

%% Read and delete.
-spec pop(NumberOfTxs::pos_integer()) -> {ok, [signed_tx()]}.
pop(_) ->
    {ok, []}.

%% Delete.
-spec delete([signed_tx(), ...]) -> ok.
delete(_) ->
    ok.

%% Read.
-spec peek(NumberOfTxs::pos_integer()) -> {ok, [signed_tx()]}.
peek(_) ->
    {ok, []}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% No `process_flag(trap_exit, true)` because no cleaning up
    %% needed in `terminate/2`.
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
