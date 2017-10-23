%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of the chain service
%%% @end
%%%-------------------------------------------------------------------

-module(aec_chain_state).

-export([ insert_block/2
        , insert_header/2
        , get_block/2
        , get_header/2
        , new/0
        , top_header/1
        , top_block/1
        ]).

%% -opaque(state() :: #{}).

-define(match_state(___S___), #{type := aec_chain_state, ___S___}).
-define(assert_state(), #{type := aec_chain_state}).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    #{ type => aec_chain_state
     , blocks_db => db_new()
     , top_header_hash => undefined
     , top_block_hash  => undefined
     , alt_tops => []
     , state_trees => #{}
     , dangling_prevs => orddict:new()
     }.

top_header(?match_state(top_header_hash := X) = State) ->
    export_header(blocks_db_get(X, State)).

top_block(?match_state(top_block_hash := X) = State) ->
    export_block(blocks_db_get(X, State), State).

insert_block(Block, ?assert_state() = State) ->
    internal_insert(wrap_block(Block), State).

insert_header(Block, ?assert_state() = State) ->
    internal_insert(wrap_header(Block), State).

get_block(Hash, ?assert_state() = State) ->
    case blocks_db_find(Hash, State) of
        {ok, Internal} -> {ok, export_block(Internal, State)};
        error -> error
    end.

get_header(Hash, ?assert_state() = State) ->
    case blocks_db_find(Hash, State) of
        {ok, Internal} -> {ok, export_header(Internal)};
        error -> error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_top_header_hash(#{top_header_hash := H}) -> H.
set_top_header_hash(H, State) -> State#{top_header_hash => H}.

get_top_block_hash(#{top_block_hash := H}) -> H.
set_top_block_hash(H, State) -> State#{top_block_hash => H}.

%%%-------------------------------------------------------------------
%%% Internal ADT for differing between blocks and headers
%%%-------------------------------------------------------------------

-record(node, { type    :: 'block' | 'header'
              , content :: any() %% aec_block | aec_header | aec_trees
              , hash    :: binary()
              }).

prev_hash(#node{type = header, content = X}) -> aec_headers:prev_hash(X);
prev_hash(#node{type = block , content = X}) -> aec_blocks:prev_hash(X).

node_height(#node{type = header, content = X}) -> aec_headers:height(X);
node_height(#node{type = block , content = X}) -> aec_blocks:height(X).

wrap_block(Block) ->
    #node{ type = block
         , content = Block
         , hash = aec_headers:hash_header(aec_blocks:to_header(Block))
         }.

wrap_header(Header) ->
    #node{ type = header
         , content = Header
         , hash = aec_headers:hash_header(Header)
         }.

export_header(#node{type = header, content = H}) -> H;
export_header(#node{type = block, content = H}) -> aec_blocks:to_header(H).

export_block(#node{type = block, hash = H, content = B}, State) ->
    case add_state_tree_to_block(B, H, State) of
        {ok, ExportBlock} -> ExportBlock;
        error -> B
    end.

%%%-------------------------------------------------------------------
%%% Handling the state trees
%%%-------------------------------------------------------------------

add_state_tree_to_block(Block, Hash, State) ->
    case state_db_find(Hash, State) of
        {ok, Trees} -> {ok, aec_blocks:set_trees(Block, Trees)};
        error -> error
    end.

%%%-------------------------------------------------------------------
%%% Chain operations
%%%-------------------------------------------------------------------

internal_insert(Node, State) ->
    case blocks_db_find(Node#node.hash, State) of
        error -> check_update_after_insert(Node, blocks_db_put(Node, State));
        {ok, Node} -> State;
        {ok, Old} when Node#node.type =:= block, Old#node.type =:= header ->
            check_update_after_insert(Node, blocks_db_put(Node, State));
        {ok, Old} when Node#node.type =:= Node#node.type ->
            error({same_key_different_content, Node, Old})
    end.

check_update_after_insert(Node, State) ->
    PrevHash = prev_hash(Node),
    TopBlockHash  = get_top_block_hash(State),
    State1 = case determine_chain_relation(Node, State) of
                 off_chain -> State;
                 in_chain -> State;
                 new_top -> set_top_header_hash(Node#node.hash, State);
                 missing_link -> find_new_header_top(Node, State)
             end,
    case TopBlockHash =:= PrevHash of
        true -> update_state_tree(PrevHash, Node, State1);
        false -> State
    end.

determine_chain_relation(_Node,_State) ->
    %% TODO: This.
    off_chain.

find_new_header_top(_Node, State) ->
    %% TODO: This.
    State.

%% Precondition: The header chain is assumed to have been updated
%%               before this function is called, i.e., the choice
%%               of main chain must have been made already.
update_state_tree(_PrevHash, #node{type = header} =_Node, State) ->
    State;
update_state_tree(PrevHash, #node{type = block} = Node, State) ->
    Hash = Node#node.hash,
    Tree = state_db_get(PrevHash, State),
    Txs = aec_blocks:txs(Node#node.content),
    Height = aec_blocks:height(Node#node.content),
    {ok, NewTrees} = aec_tx:apply_signed(Txs, Tree, Height),
    State1 = state_db_delete(PrevHash, State),
    State2 = state_db_put(Hash, NewTrees, State1),
    State3 = set_top_block_hash(Hash, State2),
    case find_next_node_at_height_from_top_header(Height + 1, State3) of
        {ok, NextNode} -> update_state_tree(Hash, NextNode, State3);
        not_found -> State3
    end.

find_next_node_at_height_from_top_header(AtHeight, State) ->
    TopHash = get_top_header_hash(State),
    TopHeader = blocks_db_get(TopHash, State),
    Height = node_height(TopHeader),
    find_next_node_at_height(AtHeight, Height, TopHeader, State).

find_next_node_at_height(AtHeight, AtHeight, Node,_State) -> Node;
find_next_node_at_height(AtHeight, Height, Node, State) when Height > AtHeight ->
    PrevHash = prev_hash(Node),
    Node = blocks_db_get(PrevHash, State),
    find_next_node_at_height(AtHeight, Height - 1, Node, State);
find_next_node_at_height(AtHeight, Height,_Node,_State) when Height < AtHeight ->
    not_found.

%%%-------------------------------------------------------------------
%%% Internal interface for the blocks_db
%%%-------------------------------------------------------------------

db_new() ->
    dict:new().

db_put(Key, Val, Store) ->
    dict:store(Key, Val, Store).

db_get(Key, Store) ->
    case dict:find(Key, Store) of
        {ok, Res} -> Res;
        error -> error({failed_get, Key})
    end.

db_find(Key, Store) ->
    case dict:find(Key, Store) of
        {ok, Res} -> {ok, Res};
        error -> error
    end.

db_delete(Key, Store) ->
    dict:erase(Key, Store).

blocks_db_put(#node{hash = Hash} = Node, #{blocks_db := DB} = State) ->
    State#{blocks_db => db_put(Hash, Node, DB)}.

blocks_db_find(Key, #{blocks_db := Store}) ->
    db_find(Key, Store).

blocks_db_get(Key, #{blocks_db := Store}) ->
    db_get(Key, Store).

state_db_put(Hash, Trees, #{state_db := DB} = State) ->
    State#{state_db => db_put(Hash, Trees, DB)}.

state_db_delete(Hash, #{state_db := DB} = State) ->
    State#{state_db => db_delete(Hash, DB)}.

state_db_find(Key, #{state_db := Store}) ->
    db_find(Key, Store).

state_db_get(Key, #{state_db := Store}) ->
    db_get(Key, Store).
