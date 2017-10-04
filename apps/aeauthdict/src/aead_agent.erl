%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aead_agent).

-export([perform/3,
         digest/2]).


-include("ad.hrl").


-define(KEY_LENGTH, 256).
-define(VALUE_LENGTH, none).

%% 'root_node_height' should never be more than 255, so the toByte conversion
%% is safe (though it may cause an incorrect sign on the signed byte if rootHeight>127,
%% but we handle that case correctly on decoding the byte back to int in the
%% verifier, by adding 256 if it's negative).  The reason rootNodeHeight should
%% never be more than 255 is that if height is more than 255, then the AVL tree
%% has at least  2^{255/1.4405} = 2^177 leaves, which is more than the number of
%% atoms on planet Earth.
-type state() :: #{'root_node_height' => integer(),
                   'saved_node'       => aead:maybe(aead:tree_node()),
                   'callback_module'  => module()}.

-export_type([state/0]).

%%%=============================================================================
%%% aead_agent behaviour
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%  whether we found the correct leaf and the key contains it
%% @end
%%------------------------------------------------------------------------------
-callback key_matches_leaf(aead:key(), aead:leaf(), state()) ->
    {boolean(), state()}.

%%------------------------------------------------------------------------------
%% @doc
%%  whether to go left or right when searching for key and standing at r
%% @end
%%------------------------------------------------------------------------------
-callback next_direction_is_left(aead:key(), aead:internal_node(), state()) ->
    {boolean(), state()}.

%%------------------------------------------------------------------------------
%% @doc
%%  a new node with two leaves: r on the left and a new leaf containing key and
%%  value on the right
%% @end
%%------------------------------------------------------------------------------
-callback add_node(aead:leaf(), aead:key(), aead:value()) -> aead:internal_node().

%%------------------------------------------------------------------------------
%% @doc
%%  Deletions go down the tree twice -- once to find the leaf and realize
%%  that it needs to be deleted, and the second time to actually perform the deletion.
%%  This method will re-create comparison results. Each time it's called, it will give
%%  the next comparison result of
%%  key and node.key, where node starts at the root and progresses down the tree
%%  according to the comparison results.
%%
%%  Rreturns the result of previous comparison of key and relevant node's key
%% @end
%%------------------------------------------------------------------------------
-callback replay_comparison(state()) -> {integer(), state()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%   The digest consists of the label of the root node followed by its height,
%%   expressed as a single (unsigned) byte
%% @end
%%------------------------------------------------------------------------------
-spec digest(aead:tree_node(), state()) -> aead:digest().
digest(RootNode, #{root_node_height := RootNodeHeight}) ->
    Label = aead:label(RootNode),
    (RootNodeHeight >= 0 andalso RootNodeHeight < 255) orelse
        throw({error, {root_node_height_out_of_range, RootNodeHeight}}),
    <<Label/binary, RootNodeHeight:8>>.

%%------------------------------------------------------------------------------
%% @doc
%%    Assumes the conditions for the double left rotation have already been established
%%    and rightChild.left.visited = true
%%    neither child needs to be attached to currentRoot
%% @end
%%------------------------------------------------------------------------------
-spec double_left_rotate(aead:internal_node(), aead:tree_node(),
                         aead:internal_node()) -> aead:internal_node().
double_left_rotate(CurrentRoot, LeftChild, RightChild) ->
    NewRoot = aead:left(RightChild),
    {NewLeftBalance, NewRightBalance} =
        case aead:balance(NewRoot) of
            ?BALANCE_0 ->
                {?BALANCE_0, ?BALANCE_0};
            ?BALANCE_L ->
                {?BALANCE_0, ?BALANCE_R};
            ?BALANCE_R ->
                {?BALANCE_L, ?BALANCE_0}
        end,
    NewLeftChild = aead:new(CurrentRoot, #{left    => LeftChild,
                                           right   => aead:left(NewRoot),
                                           balance => NewLeftBalance}),
    NewRightChild = aead:new(RightChild, #{left    => aead:right(NewRoot),
                                           balance => NewRightBalance}),
    aead:new(NewRoot, #{left => NewLeftChild,
                        right => NewRightChild,
                        balance => ?BALANCE_0}).

%%------------------------------------------------------------------------------
%% @doc
%%    Assumes the conditions for the double right rotation have already been
%%    established and leftChild.right.visited = true
%%    neither child needs to be attached to currentRoot
%% @end
%%------------------------------------------------------------------------------
-spec double_right_rotate(aead:internal_node(), aead:internal_node(),
                          aead:tree_node()) -> aead:internal_node().
double_right_rotate(CurrentRoot, LeftChild, RightChild) ->
    NewRoot = aead:right(LeftChild),
    {NewLeftBalance, NewRightBalance} =
        case aead:balance(NewRoot) of
            ?BALANCE_0 ->
                {?BALANCE_0, ?BALANCE_0};
            ?BALANCE_L ->
                {?BALANCE_0, ?BALANCE_R};
            ?BALANCE_R ->
                {?BALANCE_L, ?BALANCE_0}
        end,
    NewRightChild = aead:new(CurrentRoot, #{right   => RightChild,
                                            left    => aead:right(NewRoot),
                                            balance => NewRightBalance}),
    NewLeftChild = aead:new(LeftChild, #{right   => aead:left(NewRoot),
                                         balance => NewLeftBalance}),
    aead:new(NewRoot, #{left => NewLeftChild,
                        right => NewRightChild,
                        balance => ?BALANCE_0}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec perform(aead_operation:operation(), aead:tree_node(), state()) ->
                               {ok, aead:tree_node(), aead:maybe(aead:value()),
                                state()} |{error, term()}.
perform(Operation, RootNode, #{callback_module := Mod} = State) ->
    try
        Key = aead_operation:key(Operation),

        %% TODO:
        %% require(ByteArray.compare(key, NegativeInfinityKey) > 0, s"Key ${Base58.encode(key)} is less than -inf")
        %% require(ByteArray.compare(key, PositiveInfinityKey) < 0, s"Key ${Base58.encode(key)} is more than +inf")
        %% require(key.length == keyLength)
        State2 = State#{saved_node => none},

        %%    returns the new root, an indicator whether tree has been modified at r or below,
        %%    an indicator whether the height has increased,
        %%    an indicator whether we need to go delete the leaf that was just reached,
        %%    and the old value associated with key
        %%
        %%    Handles binary tree search and AVL rebalancing
        %%
        %%    Deletions are not handled here in order not to complicate the code even more --
        %%    in case of deletion,
        %%    we don't change the tree, but simply return toDelete = true.
        %%    We then go in and delete using deleteHelper

        {NewRootNode, _, HeightIncreased, ToDelete, OldValue, State3} =
            modify_helper(Mod, RootNode, Key, Operation, State2),

        case ToDelete of
            true ->
                {PostDeleteRootNode, HeightDecreased, State4} =
                    delete_helper(Mod, NewRootNode, false, State3),
                State5 = case HeightDecreased of
                             true ->
                                 H = maps:get(root_node_height, State4),
                                 State4#{root_node_height => H - 1};
                             false ->
                                 State4
                         end,
                {ok, PostDeleteRootNode, OldValue, State5};
            false ->
                State4 = case HeightIncreased of
                             true ->
                                 H = maps:get(root_node_height, State3),
                                 State3#{root_node_height => H + 1};
                             false ->
                                 State3
                         end,
                {ok, NewRootNode, OldValue, State4}
        end
    catch
        throw : Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%%  returns the new root, an indicator whether tree has been modified at r or
%%  below, an indicator whether the height has increased,
%%  an indicator whether we need to go delete the leaf that was just reached,
%%  and the old value associated with key
%%
%%  Handles binary tree search and AVL rebalancing
%%
%%  Deletions are not handled here in order not to complicate the code even more -- 
%%  in case of deletion, we don't change the tree, but simply return toDelete = true.
%%  We then go in and delete using deleteHelper
%% @end
%%------------------------------------------------------------------------------
-spec modify_helper(module(), aead:tree_node(), aead:key(), aead_operation:operation(),
                    state()) ->
                           {aead:tree_node(), ChangeHappened :: boolean(),
                            HeightIncreased :: boolean(), ToDelete :: boolean(), 
                            aead:value(), state()}.
modify_helper(Mod, #{value := Value} = RootNode, Key, Operation, State) ->
    %% leaf node

    %% Do not set the visited flag on the way down -- set it only after you
    %% know the operation did not fail, because if the operation failed,
    %% there is no need to put nodes in the proof.
    RootNodeVisited = RootNode#{visited => true},
    case {Mod:key_matches_leaf(Key, RootNode, State), Operation} of
        {{true, State2}, {modification, _Key, Upd}} ->
            case Upd({some, Value}) of
                {ok, none} ->
                    %% delete key
                    {RootNodeVisited, false, false, true, {some, Value}, State2};
                {ok, {some, NewValue}} ->
                    %% update value
                    %% TODO: check if value length has not changed
                    %% valueLengthOpt.foreach(vl => require(v.length == vl, s"Value length is fixed and should be $vl"))
                    NewRoot = aead:new(RootNodeVisited, #{value => NewValue}),
                    {NewRoot, true, false, false, Value, State2};
                {error, _} = Error ->
                    %% update function doesnt like the value we found
                    throw(Error)
            end;
        {{false, State2}, {modification, _Key, Upd}} ->
            case Upd(none) of
                {ok, none} ->
                    %% don't change anything, just lookup
                    {RootNodeVisited, false, false, false, none, State2};
                {ok, {some, NewValue}} ->
                    %% insert some new value
                    %% TODO: check if value length has not changed
                    %% valueLengthOpt.foreach(vl => require(v.length == vl, s"Value length is fixed and should be $vl"))
                    {Mod:add_node(RootNodeVisited, Key, NewValue),
                     true, true, false, none, State2};
                {error, _} = Error ->
                    %% update function doesn't like that we found nothing
                    throw(Error)
            end;
        {{true, State2}, {lookup, _Key, _Upd}} ->
            {RootNodeVisited, false, false, false, {some, Value}, State2};
        {{false, State2}, {lookup, _Key, _Upd}} ->
            {RootNodeVisited, false, false, false, none, State2}
    end;
modify_helper(aead_verifier, ?LABEL_ONLY_NODE = RootNode, _Key, Operation, _State) ->
    lager:error("Incorrect proof: trying to traverse a label-only node: ~p, operation: ~p~n",
                [RootNode, Operation]),
    throw({error, incorrect_proof_label_only_node});
modify_helper(Mod, RootNode, Key, Operation, State) ->
    %% Internal node

    %% Go recursively in the correct direction
    %% Get a new node
    %% See if a single or double rotation is needed for AVL tree balancing
    case Mod:next_direction_is_left(Key, RootNode, State) of
        {true, State2} ->
            {NewLeft, ChangeHappened, ChildHeightIncreased, ToDelete, OldValue, State3} =
                modify_helper(Mod, aead:left(RootNode), Key, Operation, State2),
            RootNodeVisited = RootNode#{visited => true},

            %% balance = ?BALANCE_L (-1) if left higher, ?BALANCE_R (+1) if left lower
            case ChangeHappened of
                true ->
                    case {ChildHeightIncreased, aead:balance(RootNodeVisited)} of
                        {true, ?BALANCE_L} ->
                            %% need to rotate
                            %% at this point we know newLeftM must be an internal node
                            %% and not a leaf -- because height increased
                            case aead:balance(NewLeft) of
                                ?BALANCE_L ->
                                    %% single right rotate
                                    NewRight = aead:new(RootNodeVisited,
                                                        #{left    => aead:right(NewLeft),
                                                          balance => ?BALANCE_0}),
                                    {aead:new(NewLeft, #{right   => NewRight,
                                                         balance => ?BALANCE_0}),
                                     true, false, false, OldValue, State3};
                                _ ->
                                    {double_right_rotate(RootNodeVisited, NewLeft,
                                                         aead:right(RootNodeVisited)),
                                     true, false, false, OldValue, State3}
                            end;
                        {true, ?BALANCE_0} ->
                            %% no need to rotate
                            {aead:new(RootNodeVisited, #{left    => NewLeft,
                                                         balance => ?BALANCE_L}),
                             true, true, false, OldValue, State3};
                        {true, ?BALANCE_R} ->
                            %% no need to rotate
                            {aead:new(RootNodeVisited, #{left    => NewLeft,
                                                         balance => ?BALANCE_0}),
                             true, false, false, OldValue, State3};
                        {false, B} ->
                            {aead:new(RootNodeVisited, #{left    => NewLeft,
                                                         balance => B}),
                             true, false, false, OldValue, State3}
                    end;
                false ->
                    %% no change happened
                    {RootNodeVisited, false, false, ToDelete, OldValue, State3}
            end;
        {false, State2} ->
            %% next direction is right
            {NewRight, ChangeHappened, ChildHeightIncreased, ToDelete, OldValue, State3} =
                modify_helper(Mod, aead:right(RootNode), Key, Operation, State2),
            RootNodeVisited = RootNode#{visited => true},

            %% balance = ?BALANCE_L (-1) if left higher, ?BALANCE_R (+1) if left lower
            case ChangeHappened of
                true ->
                    case {ChildHeightIncreased, aead:balance(RootNodeVisited)} of
                        {true, ?BALANCE_R} ->
                            %% need to rotate
                            %% at this point we know NewRight must be an internal node
                            %%  and not a leaf -- because height increased
                            %% TODO: ?? val newRight = newRightM.asInstanceOf[InternalNode[D]]

                            case aead:balance(NewRight) of
                                ?BALANCE_R ->
                                    %% single left rotate
                                    NewRight2 = aead:new(RootNodeVisited,
                                                         #{right   => aead:left(NewRight),
                                                           balance => ?BALANCE_0}),
                                    {aead:new(NewRight, #{left    => NewRight2,
                                                          balance => ?BALANCE_0}),
                                     true, false, false, OldValue, State3};
                                _ ->
                                    %% double left rotate
                                    {double_left_rotate(RootNodeVisited,
                                                        aead:left(RootNodeVisited), NewRight),
                                     true, false, false, OldValue, State3}
                            end;
                        {true, ?BALANCE_0} ->
                            %% no need to rotate
                            {aead:new(RootNodeVisited, #{right   => NewRight,
                                                         balance => ?BALANCE_R}),
                             true, true, false, OldValue, State3};
                        {true, ?BALANCE_L} ->
                            %% no need to rotate
                            {aead:new(RootNodeVisited, #{right   => NewRight,
                                                         balance => ?BALANCE_0}),
                             true, false, false, OldValue, State3};
                        {false, B} ->
                            {aead:new(RootNodeVisited, #{right   => NewRight,
                                                         balance => B}),
                             true, false, false, OldValue, State3}
                    end;
                false ->
                    %% no change happened
                    {RootNodeVisited, false, false, ToDelete, OldValue, State3}
            end
    end.
%% TODO: label-only nodes!

%%------------------------------------------------------------------------------
%% @doc
%%   Deletes the node in the subtree rooted at r and its corresponding leaf
%%   as indicated by replay_comparison or deleteMax. Performs AVL balancing.
%%
%%   If deleteMax == false: deletes the first node for which replay_comparison
%%   returns 0 and the leaf that is the leftmost descendant of this node's child
%%
%%   If deleteMax == true: deletes the right leaf and its parent, replacing the
%%   parent with the parent's left child
%%
%%   Returns the new root and an indicator whether the tree height decreased
%% @end
%%------------------------------------------------------------------------------
-spec delete_helper(module(), aead:internal_node(), boolean(), state()) ->
                           {aead:tree_node(), boolean(), state()}.
delete_helper(Mod, RootNode, DeleteMax, State) ->
    %% Overall strategy: if key is found in the node that has only a leaf as either
    %% of the two children, we can just delete the node. If it has a leaf as the
    %% right child, we can also delete the right child, update the nextLeafKey in
    %% the rightmost leaf of the left subtree, and we are done. Else, it has a leaf
    %% as the left child, so we copy the information from this left child leaf to
    %% the leftmost leaf in the right subtree, and delete the left child.
    %%
    %% Things get more complicated key is found in a node that has two non-leaf
    %% children.  In that case, we perform a deleteMax operation on the left subtree
    %% (recursively call ourselves on the left child with deleteMax = true), and
    %% copy the information from that deleted leaf into the node where the
    %% key was found and into the leftmost leaf of its right subtree
    {Direction, State2} =
        if
            DeleteMax -> {1, State};
            true      -> Mod:replay_comparison(State)
        end,
    case {Direction, aead:left(RootNode)} of
        {_D, ?LEAF_NODE} when _D < 0 ->
            %% If Direction < 0, this means we are not in deleteMax mode and we still
            %% haven't found the value we are trying to delete
            %% If the next step -- which is to the left -- is a leaf, then the value
            %% we are looking for is not a key of any internal node in the tree,
            %% which is impossible
            throw({error, failed_to_delete});
        _ ->
            ok
    end,

    LeftChild = aead:left(RootNode),
    RightChild = aead:right(RootNode),
    case {Direction, LeftChild, RightChild} of
        {D, _, ?LEAF_NODE} when D >= 0 ->
            %% we delete this node and its right child (leaf)
            %% we return the left subtree
            case DeleteMax of
                true ->
                    %% If we are in deleteMax mode,
                    %% we should save the info of leaf we are deleting,
                    %% because it will be copied over to its successor
                    State3 = State2#{saved_node => {some, aead:visit(RightChild)}},
                    {LeftChild, true, State3};
                false ->
                    %% Otherwise, we really are deleting the leaf, and therefore
                    %% we need to change the nextLeafKey of its predecessor
                    Direction == 0 orelse throw({error, unexpected_direction}),
                    {change_next_leaf_key_of_max_node(LeftChild,
                                                      aead:next_leaf_key(RightChild)),
                     true, State2}
            end;
        {0, ?LEAF_NODE, _} ->
            {change_key_and_value_of_min_node(RightChild,
                                              aead:key(LeftChild),
                                              aead:value(LeftChild)), true, State2};
        _ ->
            %% Potential hard deletion cases:
            case Direction of
                D when D =< 0 ->
                    %% going left; know left child is not a leaf;
                    %% deleteMax if and only if direction == 0
                    {NewLeft, ChildHeightDecreased, State3} =
                        delete_helper(Mod, LeftChild, Direction == 0, State2),
                    {NewRoot, State4} =
                        case Direction of
                            0 ->
                                %% this is the case where we needed to delete the min
                                %% of the right subtree, but, because we had two non-leaf
                                %% children, we instead deleted the node that was the max
                                %% of the left subtree and are copying its info
                                {some, S} = maps:get(saved_node, State3),
                                SKey = aead:key(S),
                                RootWithChangedKey = aead:set_key(SKey, RootNode),
                                {aead:new(RootWithChangedKey,
                                          #{right => change_key_and_value_of_min_node(
                                                       aead:right(RootWithChangedKey),
                                                       SKey, aead:value(S))}),
                                 State3#{saved_node => none}};
                            _ ->
                                {RootNode, State3}
                        end,

                    case {ChildHeightDecreased, aead:balance(NewRoot)} of
                        {true, ?BALANCE_R} ->
                            %% new to rotate because my left subtree is shorter than my right
                            %% I know my right child is not a leaf, because it is taller
                            %% than my left
                            RightChild = aead:right(NewRoot),
                            case aead:balance(RightChild) of
                                ?BALANCE_L ->
                                    %% double left rotate
                                    %% I know rightChild.left is not a leaf, because rightChild
                                    %% has a higher subtree on the left
                                    {double_left_rotate(aead:visit(NewRoot), NewLeft,
                                                        aead:visit(RightChild)), true, State4};
                                B ->
                                    %% single left rotate
                                    {NewLBalance, NewRBalance} =
                                        case B of
                                            ?BALANCE_0 -> {?BALANCE_R, ?BALANCE_L};
                                            ?BALANCE_R -> {?BALANCE_0, ?BALANCE_0}
                                        end,
                                    NewLeftChild = aead:new(NewRoot,
                                                            #{left    => NewLeft,
                                                              right   => aead:left(RightChild),
                                                              balance => NewLBalance}),
                                    {aead:new(RightChild, #{left    => NewLeftChild,
                                                            balance => NewRBalance}),
                                      NewRBalance == ?BALANCE_0, State4}
                            end;
                        {true, ?BALANCE_0} ->
                            {aead:new(NewRoot, #{left => NewLeft,
                                                 balance => ?BALANCE_R}), false, State4};
                        {true, ?BALANCE_L} ->
                            {aead:new(NewRoot, #{left => NewLeft,
                                                 balance => ?BALANCE_0}), true, State4};
                        {false, B} ->
                            {aead:new(NewRoot, #{left => NewLeft,
                                                 balance => B}), B == ?BALANCE_0, State4}
                    end;
                D when D > 0 ->
                    %% going right; know right child is not a leaf
                    {NewRight, ChildHeightDecreased, State3} =
                        delete_helper(Mod, aead:right(RootNode), DeleteMax, State2),
                    RootNodeVisited = aead:visit(RootNode),
                    case {ChildHeightDecreased, aead:balance(RootNode)} of
                        {true, ?BALANCE_L} ->
                            %% new to rotate because my right subtree is shorter than my left
                            %% TODO r.left.visited = true
                            %% I know my left child is not a leaf, because it is taller
                            %% than my right
                            LeftChild = aead:left(RootNode),
                            case aead:balance(LeftChild) of
                                ?BALANCE_R ->
                                    %% double right rotate
                                    %% I know LeftChild.right is not a leaf,
                                    %% because LeftChild has a higher subtree on the right
                                    %% TODO leftChild.right.visited = true
                                    RightOfLeftChild = aead:visit(aead:right(LeftChild)),
                                    NewLeftChild = aead:new(LeftChild,
                                                            #{right => RightOfLeftChild}),
                                    {double_right_rotate(RootNode,
                                                         aead:visit(NewLeftChild), NewRight),
                                     true, State3};
                                ?BALANCE_0 ->
                                    %% single right rotate
                                    NewRightChild = aead:new(aead:visit(RootNode),
                                                             #{left    => aead:right(LeftChild),
                                                               right   => NewRight,
                                                               balance => ?BALANCE_L}),
                                    {aead:new(LeftChild, #{right   => NewRightChild,
                                                           balance => ?BALANCE_R}),
                                     false, State3};
                                ?BALANCE_L ->
                                    %% single right rotate
                                    NewRightChild = aead:new(aead:visit(RootNode),
                                                             #{left    => aead:right(LeftChild),
                                                               right   => NewRight,
                                                               balance => ?BALANCE_0}),
                                    {aead:new(LeftChild, #{right   => NewRightChild,
                                                           balance => ?BALANCE_0}),
                                     true, State3}
                            end;
                        {true, ?BALANCE_0} ->
                            {aead:new(RootNodeVisited,
                                      #{right   => NewRight,
                                        balance => ?BALANCE_L}), false, State3};
                        {true, ?BALANCE_R} ->
                            {aead:new(RootNodeVisited,
                                      #{right   => NewRight,
                                        balance => ?BALANCE_0}), true, State3};
                        {false, B} ->
                            {aead:new(RootNodeVisited,
                                      #{right   => NewRight,
                                        balance => B}), B == ?BALANCE_0, State3}
                    end
            end
    end.

-spec change_next_leaf_key_of_max_node(aead:tree_node(), aead:key()) ->
                                              aead:tree_node().
change_next_leaf_key_of_max_node(#{value := _} = LeafNode, NextLeafKey) ->
    aead:new(LeafNode, #{visited   => true,
                         next_leaf => NextLeafKey});
change_next_leaf_key_of_max_node(#{balance := _} = InternalNode, NextLeafKey) ->
    NewRightNode = change_next_leaf_key_of_max_node(
                     aed_ad:right(InternalNode), NextLeafKey),
    aead:new(InternalNode, #{visited => true,
                             right   => NewRightNode}).

-spec change_key_and_value_of_min_node(aead:tree_node(), aead:key(),
                                       aead:value()) -> aead:tree_node().
change_key_and_value_of_min_node(#{value := _} = LeafNode, NewKey, NewValue) ->
    aead:new(LeafNode, #{visited => true,
                         key     => NewKey,
                         value   => NewValue});
change_key_and_value_of_min_node(#{balance := _} = InternalNode, NewKey, NewValue) ->
    NewLeftNode = change_key_and_value_of_min_node(aead:left(InternalNode),
                                                   NewKey, NewValue),
    aead:new(InternalNode, #{visited => true,
                             left    => NewLeftNode}).
