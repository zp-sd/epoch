%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Implements the batch AVL prover from https://eprint.iacr.org/2016/994
%%%
%%%   @param keyLength        - length of keys in tree
%%%   @param valueLengthOpt   - length of values in tree.
%%%                             None if it is not fixed
%%%   @param oldRootAndHeight - option root node and height of old tree.
%%%                             Tree should contain new nodes only
%%%   WARNING if you pass it, all isNew and visited flags should be
%%%   set correctly and height should be correct
%%% @end
%%%-------------------------------------------------------------------
-module(aead_prover).

-behaviour(aead_agent).
-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-export([start_link/0,
         stop/0,
         digest/0,
         perform/1,
         generate_proof/0,
         unauthenticated_lookup/1,
         top_node_to_string/0]).

%%------------------------------------------------------------------------------
%% aead_agent behaviour callbacks
%%------------------------------------------------------------------------------
-export([key_matches_leaf/3,
         next_direction_is_left/3,
         add_node/3,
         replay_comparison/1]).

%%------------------------------------------------------------------------------
%% gen_server behaviour callbacks
%%------------------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("stdlib/include/assert.hrl").
-include("ad.hrl").


-define(SERVER, ?MODULE).

-type prover_state() :: #{'top_node'         => aead:tree_node(),
                          'old_top_node'     => aead:tree_node(),
                          'directions'       => list(boolean()),  %% = []
                          'replay_index'     => integer(),        %% = 0
                          'last_right_step'  => integer(),        %% = 0
                          'found'            => boolean(),
                          %% inherited from aead_agent:state():
                          'root_node_height' => integer(),        %% = 0
                          'saved_node'       => aead:maybe(aead:tree_node()), %% = none
                          'callback_module'  => module()}.

-type packaged_tree() :: binary().
-type serialized_proof() :: {packaged_tree(),
                             boolean()}.      %% if previous leaf is available

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec digest() -> aead:digest().
digest() ->
    gen_server:call(?SERVER, digest).

-spec perform(aead_operation:operation()) ->
                     aead:sucess(aead:maybe(aead:value())).
perform(Operation) ->
    gen_server:call(?SERVER, {perform, Operation}).

%% Resets visited/new flags, too
-spec generate_proof() -> serialized_proof().
generate_proof() ->
    gen_server:call(?SERVER, generate_proof).

-spec unauthenticated_lookup(aead:key()) -> aead:success(aead:value()).
unauthenticated_lookup(Key) ->
    gen_server:call(?SERVER, {unauthenticated_lookup, Key}).

-spec top_node_to_string() -> string().
top_node_to_string() ->
    gen_server:call(?SERVER, top_node_to_string).

%%%=============================================================================
%%% aead_agent behaviour callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%    Determines if the leaf r contains the key
%% @end
%%------------------------------------------------------------------------------
-spec key_matches_leaf(aead:key(), aead:leaf(), aed_agent:state()) ->
                              {boolean(), aed_agent:state()}.
key_matches_leaf(_Key, _Node, #{found := Found} = State) ->
    %% The prover doesn't actually need to look at the leaf key,
    %% because the prover would have already seen this key on the way
    %% down the to leaf if and only if the leaf matches the key that is being sought
    {Found, State#{found => false}}.

%%------------------------------------------------------------------------------
%% @doc
%%   Figures out whether to go left or right when from node r when searching for
%%   the key;
%%   records the appropriate bit in the directions bit string to be used in the proof
%%
%%   @return - true if to go left, false if to go right in the search
%% @end
%%------------------------------------------------------------------------------
-spec next_direction_is_left(aead:key(), aead:internal_node(), aed_agent:state()) ->
                                    {boolean(), aed_agent:state()}.
next_direction_is_left(_Key, _Node, #{found := true,
                                      directions := Directions} = State) ->
    {true, State#{directions => [true | Directions]}};
next_direction_is_left(Key, Node, #{found := false,
                                    directions := Directions} = State) ->
    case aead:compare_keys(Key, aead:key(Node)) of
        0 ->
            %% found in the tree -- go one step right, then left to the leaf
            {false, State#{found           => true,
                           last_right_step => length(Directions),
                           directions      => [false | Directions]}};
       -1 ->
            %% going left
            {true, State#{directions => [true | Directions]}};
        1 ->
            %% going right
            {false, State#{directions => [false | Directions]}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%  Return a new prover node with two leaves: R on the left and a new leaf
%%  containing Key and Value on the right
%% @end
%%------------------------------------------------------------------------------
-spec add_node(aead:leaf(), aead:key(), aead:value()) -> aead:internal_node().
add_node(R, Key, Value) ->
    NextKey = aead:next_leaf_key(R),
    #{key => Key,
      visited => true,
      is_new => true,
      balance => ?BALANCE_0,
      left => R#{next_leaf => Key},
      right => #{key => Key,
                 visited => true,
                 is_new => true,
                 value => Value,
                 next_leaf => NextKey}}.

%%------------------------------------------------------------------------------
%% @doc
%% Deletions go down the tree twice -- once to find the leaf and realize
%% that it needs to be deleted, and the second time to actually perform the
%% deletion. This method will re-create comparison results using directions
%% array and last_right_step variable. Each time it's called, it will give the
%% next comparison result of key and node.key, where node starts at the root
%% and progresses down the tree according to the comparison results.
%%
%% @return - result of previous comparison of key and relevant node's key
%% @end
%%------------------------------------------------------------------------------
-spec replay_comparison(aead_agent:state()) -> {integer(), aead_agent:state()}.
replay_comparison(#{replay_index    := ReplayIndex,
                    last_right_step := LastRightStep,
                    directions      := Directions} = State) ->
    State2 = State#{replay_index => ReplayIndex + 1},
    case LastRightStep of
        ReplayIndex ->
            {0, State2};
        _ ->
            case lists:nth(ReplayIndex + 1, Directions) of
                false -> {1, State2};
                true  -> {-1, State2}
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(_) -> {ok, prover_state()}.
init(_) ->
    TopNode = aead:initial_node(),
    {ok, #{top_node         => TopNode,
           old_top_node     => TopNode,
           directions       => [],
           replay_index     => 0,
           last_right_step  => 0,
           found            => false,
           root_node_height => 0,
           saved_node       => none,
           callback_module  => ?MODULE}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), gen_server:server_ref(), prover_state()) ->
                         {'reply', term(), prover_state()}.
handle_call(digest, _From, State) ->
    {reply, digest(State), State};
handle_call({perform, Operation}, _From, State) ->
    case perform(Operation, State) of
        {ok, {MaybeValue, State2}} ->
            {reply, {ok, MaybeValue}, State2};
        {error, _} = Error ->
            lager:error("Operation ~p failed: ~p", [Operation, Error]),
            {reply, Error, State}
    end;
handle_call(generate_proof, _From, State) ->
    {Proof, State2} = generate_proof(State),
    {reply, Proof, State2};
handle_call({unauthenticated_lookup, Key}, _From, State) ->
    Res = unauthenticated_lookup(Key, State),
    {reply, Res, State};
handle_call(top_node_to_string, _From, #{top_node := TopNode} = State) ->
    {reply, aead:to_string(TopNode, 0), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), prover_state()) ->
                         {'noreply', prover_state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), prover_state()) ->
                         {'noreply', prover_state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    This function is called by a gen_server when it is about to
%%    terminate. It should be the opposite of Module:init/1 and do any
%%    necessary cleaning up. When it returns, the gen_server terminates
%%    with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(atom(), prover_state()) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%    If operation.key exists in the tree and the operation succeeds,
%%    returns Success(Some(v)), where v is the value associated with operation.key
%%    before the operation.
%%    If operation.key does not exists in the tree and the operation succeeds,
%%    returns Success(None).
%%    Returns Failure if the operation fails.
%%    Does not modify the tree or the proof in case return is Failure.
%% @end
%%------------------------------------------------------------------------------
-spec perform(aead_operation:operation(), prover_state()) ->
                     aead:sucess(aead:maybe({aead:value(), prover_state()})).
perform(Operation, #{directions := Directions,
                     top_node   := TopNode} = State) ->
    case aead_agent:perform(Operation, TopNode,
                            State#{replay_index => length(Directions)}) of
        {ok, TopNode2, MaybeValue, State2} ->
            {ok, {MaybeValue, State2#{top_node     := TopNode2,
                                      old_top_node := TopNode}}};
        {error, _} = Error ->
            %% if topNode is None, the line above will fail and nothing will change
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%%    Generates the proof for all the operations performed (except the ones that failed)
%%    since the last generateProof call
%% @end
%%------------------------------------------------------------------------------
-spec generate_proof(prover_state()) -> {serialized_proof(), prover_state()}.
generate_proof(#{top_node     := TopNode,
                 old_top_node := OldTopNode,
                 directions   := Directions} = State) ->
    {ResetTopNode, PackagedTree} = pack_tree(TopNode),
    DirectionsBin = directions_to_binary(Directions),
    {<<PackagedTree/binary, ?END_OF_TREE_IN_PACKAGED_PROOF, DirectionsBin/binary>>,
     State#{top_node     => ResetTopNode,
            old_top_node => ResetTopNode,
            diections    => []}}.

%%------------------------------------------------------------------------------
%% @doc
%%   A simple non-modifying non-proof-generating lookup.
%%   Does not mutate the data structure
%%
%%   returns {'some', Value} for value associated with the given key if key is
%%   in the tree, and 'none' otherwise
%% @end
%%------------------------------------------------------------------------------
-spec unauthenticated_lookup(aead:key(), prover_state()) -> aead:success(aead:value()).
unauthenticated_lookup(Key, #{top_node := TopNode}) ->
    unauthenticated_lookup(TopNode, Key, false).

%%------------------------------------------------------------------------------
%% @doc
%%    Returns the current digest of the authenticated data structure,
%%    which contains the root hash and the root height
%% @end
%%------------------------------------------------------------------------------
-spec digest(prover_state()) -> aead:digest().
digest(#{top_node := TopNode} = State) ->
    aead_agent:digest(TopNode, State).

%%------------------------------------------------------------------------------
%% @doc
%% (original) TODO Possible optimizations:
%% - Don't put in the key if it's in the modification stream somewhere
%%   (savings ~32 bytes per proof for transactions with existing key; 0 for insert)
%%   (problem is that then verifier logic has to change --
%%   can't verify tree immediately)
%% - Condense a sequence of balances and other non-full-byte info using
%%   bit-level stuff and maybe even "changing base without losing space"
%%   by Dodis-Patrascu-Thorup STOC 2010 (expected savings: 5-15 bytes
%%   per proof for depth 20, based on experiments with gzipping the array
%%   that contains only this info)
%% - Condense the sequence of values if they are mostly not randomly distributed
%% @end
%%------------------------------------------------------------------------------
-spec pack_tree(aead:prover_node()) -> {aead:prover_node(), packaged_tree()}.
pack_tree(Node) ->
    {NewNode, _, Pack} = pack_tree(Node, false, <<>>),
    {NewNode, Pack}.

-spec pack_tree(aead:prover_node(), PrevNodeAvailable :: boolean(), packaged_tree()) ->
                       {aead:prover_node(), boolean(), packaged_tree()}.
pack_tree(Node, PreviousLeafAvailable, PackagedTree) ->
    %% Post order traversal to pack up the tree
    case aead:visited(Node) of
        false ->
            Label = aead:label(Node),
            {Node#{is_new => false}, false,
             <<PackagedTree/binary, ?LABEL_IN_PACKAGED_PROOF, Label/binary>>};
        true ->
            case Node of
                ?PROVER_LEAF ->
                    NodeUnvisited = aead:reset_new_visited(Node),
                    Value = aead:value(Node),
                    ValueBin = case ?VALUE_BITS of
                                      none ->
                                       <<64:32, Value:(8*64)>>;
                                      {some, L} ->
                                       <<Value:L>>
                                  end,
                    NextLeafKey = aead:next_leaf_key(Node),
                    case PreviousLeafAvailable of
                        true ->
                            {NodeUnvisited, true,
                             <<PackagedTree/binary, ?LEAF_IN_PACKAGED_PROOF,
                               NextLeafKey/binary, ValueBin/binary>>};
                        false ->
                            Key = aead:key(Node),
                            {NodeUnvisited, true,
                             <<PackagedTree/binary, ?LEAF_IN_PACKAGED_PROOF,
                               Key/binary, NextLeafKey/binary, ValueBin/binary>>}
                    end;
                ?PROVER_INTERNAL_NODE ->
                    {LeftNode, PLAL, PTL} = pack_tree(aead:left(Node),
                                                      PreviousLeafAvailable, PackagedTree),
                    {RightNode, PLAR, PTR} = pack_tree(aead:right(Node), PLAL, PTL),
                    Balance = aead:balance(Node),
                    {aead:new(Node, #{left    => LeftNode,
                                      right   => RightNode,
                                      is_new  => false,
                                      visited => false}),
                     PLAR, <<PackagedTree/binary, PTR/binary, Balance:8>>}
            end
    end.

-spec directions_to_binary(list(boolean())) -> binary().
directions_to_binary(Directions) ->
    %% make sure binary consists of full bytes
    Len = length(Directions),
    Dirs = case Len rem 8 of
               0 -> Directions;
               M -> Directions ++ lists:duplicate(8 - M, false)
           end,
    directions_to_binary(Dirs, <<>>).

-spec directions_to_binary(list(boolean()), binary()) -> binary().
directions_to_binary([], Acc) ->
    Acc;
directions_to_binary(Dirs, Acc) ->
    {H, T} = lists:split(8, Dirs),
    [I1, I2, I3, I4, I5, I6, I7, I8] = lists:map(fun(true) -> 1;
                                                    (false) -> 0
                                                 end, H),
    directions_to_binary(T, <<Acc/binary, I1:1, I2:1, I3:1, I4:1, I5:1, I6:1, I7:1, I8:1>>).

-spec unauthenticated_lookup(aead:prover_node(), aead:key(), boolean()) ->
                                    aead:maybe(aead:value()).
unauthenticated_lookup(Node, Key, Found) ->
    case {Node, Found} of
        {?PROVER_LEAF, true} ->
            {some, aead:value(Node)};
        {?PROVER_LEAF, false} ->
            none;
        {?PROVER_INTERNAL_NODE, true} ->
            unauthenticated_lookup(aead:left(Node), Key, true);
        {?PROVER_INTERNAL_NODE, false} ->
            case aead:compare_keys(Key, aead:key(Node)) of
                0 ->
                    %% found in the tree -- go one step right, then left to the leaf
                    unauthenticated_lookup(aead:right(Node), Key, true);
                -1 ->
                    %% going left, not yet found
                    unauthenticated_lookup(aead:left(Node), Key, false);
                1 ->
                    %% going right, not yet found
                    unauthenticated_lookup(aead:right(Node), Key, false)
            end
    end.
