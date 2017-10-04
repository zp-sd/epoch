%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aead).

-export([new/2,
         left/1,
         right/1,
         balance/1,
         label/1,
         key/1,
         set_key/2,
         next_leaf_key/1,
         value/1,
         visit/1,
         unvisit/1,
         visited/1,
         reset_new_visited/1,
         compare_keys/2,
         label_only_node/1,
         initial_node/0,
         to_string/2]).

-include("ad.hrl").


-type maybe(T) :: 'none' | {'some', T}.
-type balance() :: ?BALANCE_0 | ?BALANCE_L | ?BALANCE_R.
-type key() :: binary().
-type label() :: binary().
-type value() :: term().
-type tree_node() :: internal_node() | leaf() | label_only_node().
-type internal_node() :: prover_internal_node() | verifier_internal_node().
-type leaf() :: prover_leaf() | verifier_leaf().
-type prover_node() :: prover_internal_node() | prover_leaf().
-type verifier_node() :: verifier_internal_node() | verifier_leaf() | label_only_node().
-type prover_internal_node() :: #{'key' := key(),
                                  'visited' := boolean(), %% = false
                                  'is_new' := boolean(), %% = true
                                  'balance' := balance(),
                                  'left' := tree_node(),
                                  'right' := tree_node()}.
-type verifier_internal_node() :: #{'visited' := boolean(), %% = false
                                    'balance' := balance(),
                                    'left' := tree_node(),
                                    'right' := tree_node()}.
-type prover_leaf() :: #{'key' := key(),
                         'visited' := boolean(), %% = false
                         'is_new' := boolean(), %% = true
                         'next_leaf' := key(),
                         'value' := value()}.
-type verifier_leaf() :: #{'key' := key(),
                           'visited' := boolean(), %% = false
                           'next_leaf' := key(),
                           'value' := value()}.
-type label_only_node() :: #{'visited' := boolean(), %% = false
                             'label' := key()}.
-type digest() :: binary().

-export_type([maybe/1,
              key/0,
              value/0,
              digest/0,
              balance/0,
              tree_node/0,
              internal_node/0,
              leaf/0,
              prover_node/0,
              verifier_node/0,
              verifier_internal_node/0,
              prover_internal_node/0,
              verifier_leaf/0,
              prover_leaf/0]).

%%%=============================================================================
%%% API
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new(tree_node(), #{}) -> tree_node().
new(#{} = Node, #{} = Args) ->
     N0 = maps:merge(Node, Args),
     set_default(visited, false, N0).

-spec left(internal_node()) -> tree_node().
left(#{left := L}) -> L.

-spec right(internal_node()) ->tree_node().
right(#{right := R}) -> R.

-spec balance(internal_node()) -> balance().
balance(#{balance := B}) -> B.

-spec label(tree_node()) -> binary().
label(#{balance := B, left := #{} = L, right := #{} = R}) ->
    LabelL = label(L),
    LabelR = label(R),
    ?HASH(<<1:8, B:8, LabelL/binary, LabelR/binary>>);
label(#{key := K, value := V, next_leaf := NK}) ->
    ValueBits = case ?VALUE_BITS of
                    none -> 64;
                    {some, L} -> L
                end,
    ?HASH(<<0:8, K/binary, V:ValueBits, NK/binary>>);
label(#{label := Label}) ->
    Label.

-spec key(leaf() | prover_internal_node()) -> key().
key(#{key := Key}) ->
    Key.

set_key(NewKey, #{key := _} = Node) ->
    Node#{key => NewKey};
set_key(_NewKey, Node) ->
    %% Internal verifier nodes do not have key: ignore
    Node.

-spec next_leaf_key(leaf()) -> key().
next_leaf_key(#{next_leaf := NL}) ->
    NL.

-spec value(leaf()) -> value().
value(#{value := Value}) ->
    Value.

-spec visit(tree_node()) -> tree_node().
visit(#{} = Node) ->
    Node#{visited => true}.

-spec unvisit(tree_node()) -> tree_node().
unvisit(#{} = Node) ->
    Node#{visited => false}.

-spec visited(tree_node()) -> boolean().
visited(#{visited := Visited}) ->
    Visited.

-spec compare_keys(key(), key()) -> -1..1.
compare_keys(Key1, Key2) ->
    case Key1 of
        Key2 ->
            0;
        _K when _K > Key2 ->
            1;
        _ -> -1
    end.

-spec reset_new_visited(prover_node()) -> prover_node().
reset_new_visited(#{is_new := _} = Node) ->
    case Node of
        ?PROVER_INTERNAL_NODE ->
            Node#{left  => reset_new_visited(aead:left(Node)),
                  right => reset_new_visited(aead:left(Node)),
                  visited => false,
                  is_new => false};
        ?PROVER_LEAF ->
            Node#{visited => false,
                  is_new => false}
    end.

-spec label_only_node(label()) -> label_only_node().
label_only_node(Label) ->
    #{visited => false,
      label   => Label}.

%%------------------------------------------------------------------------------
%% @doc
%%    Initial node: a prover leaf
%% @end
%%------------------------------------------------------------------------------
-spec initial_node() -> aead:tree_node().
initial_node() ->
    #{key       => ?NEG_INFINITY_KEY,
      visited   => false,
      is_new    => true,
      next_leaf => ?POS_INFINITY_KEY,
      value     => 0}.

%%------------------------------------------------------------------------------
%% @doc
%%    For debugging
%% @end
%%------------------------------------------------------------------------------
-spec to_string(aead:verifier_node(), integer()) -> string().
to_string(Node, Depth) ->
    lists:duplicate(2 + Depth, 32) ++
        case Node of
            ?PROVER_LEAF ->
                io_lib:format(
                  "At leaf label = ~s key = ~s  nextLeafKey = ~s value = ~p~n",
                  [base64:encode(aead:label(Node)), base64:encode(aead:key(Node)),
                   base64:encode(aead:next_leaf_key(Node)), aead:value(Node)]);
            ?PROVER_INTERNAL_NODE ->
                io_lib:format(
                  "Internal node label = ~s balance = ~p~n",
                  [base64:encode(aead:label(Node)), balance_to_string(aead:balance(Node))]) ++
                    to_string(aead:left(Node), Depth + 1) ++
                    to_string(aead:right(Node), Depth + 1);
            ?VERIFIER_LEAF ->
                io_lib:format(
                  "At leaf label = ~s key = ~s  nextLeafKey = ~s value = ~p~n",
                  [base64:encode(aead:label(Node)), base64:encode(aead:key(Node)),
                   base64:encode(aead:next_leaf_key(Node)), aead:value(Node)]);
            ?VERIFIER_INTERNAL_NODE ->
                io_lib:format(
                  "Internal node label = ~s balance = ~p~n",
                  [base64:encode(aead:label(Node)), balance_to_string(aead:balance(Node))]) ++
                    to_string(aead:left(Node), Depth + 1) ++
                    to_string(aead:right(Node), Depth + 1);
            ?LABEL_ONLY_NODE ->
                io_lib:format(
                  "Label-only node label = ~s~n", [aead:label(Node)])
        end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

set_default(Key, Value, M) ->
    case M of
        #{Key := _} -> M;
        _           -> M#{Key => Value}
    end.

balance_to_string(?BALANCE_0) -> "0";
balance_to_string(?BALANCE_L) -> "L";
balance_to_string(?BALANCE_R) -> "R".
