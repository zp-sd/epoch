%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%     Implements the batch AVL verifier from https://eprint.iacr.org/2016/994
%%
%%     @param keyLength        - length of keys in tree
%%     @param valueLengthOpt   - length of values in tree. None if it is not fixed
%%     @param maxNumOperations - option the maximum number of operations that this proof
%%                             can be for, to limit running time in case of malicious proofs.
%%                             If None, running time limits will not be enforced.
%%     @param maxDeletes       - at most, how many of maxNumOperations can be deletions;
%%                             for a tighter running time bound and better attack protection.
%%                             If None, defaults to maxNumOperations.
%%     @param hf               - hash function
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(aead_verifier).

-behaviour(gen_server).
-behaviour(aead_agent).

%%------------------------------------------------------------------------------
%% Verifier API
%%------------------------------------------------------------------------------
-export([start_link/2,
         stop/0,
         perform/1,
         digest/0,
         add_proof/1,

         extract_first_node/0,
         extract_nodes/0,
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
-define(LABEL_BITS, ?HASH_BITS).
-define(MAX_OPERATIONS, 256).

-type verifier_state() :: #{'top_node'         => aead:tree_node(),
                            'directions_index' => integer(),        %% = 0
                            'last_right_step'  => integer(),        %% = 0
                            'replay_index'     => integer(),        %% = 0
                            'proof'            => aead_agent:serialized_proof(),
                            'max_operations'   => aead:maybe(integer()),
                            'max_deletes'      => aead:maybe(integer()),
                            'root_node_height' => integer(),        %% = 0
                            'starting_digest'  => aead:digest(),
                            %% inherited from aead_agent:state():
                            'root_node_height' => integer(),        %% = 0
                            'saved_node'       => aead:maybe(aead:tree_node()), %% = none
                            'callback_module'  => module()}.

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
start_link(Digest, Proof) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Digest, Proof], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).


%%------------------------------------------------------------------------------
%% @doc
%%    Perform an opaeration on the tree
%% @end
%%------------------------------------------------------------------------------
-spec perform(aead_agent:operation()) -> aead:sucess(aead:maybe(aead:value())).
perform(Operation) ->
    gen_server:call(?SERVER, {perform, Operation}).

%%------------------------------------------------------------------------------
%% @doc
%%    Returns the current digest of the authenticated data structure,
%%    which contains the root hash and the root height
%% @end
%%------------------------------------------------------------------------------
-spec digest() -> aead:digest().
digest() ->
    gen_server:call(?SERVER, digest).

-spec add_proof(aead_agent:serialized_proof()) -> ok | {error, term()}.
add_proof(Proof) ->
    gen_server:call(?SERVER, {add_proof, Proof}).

-spec extract_first_node() -> aead:maybe(aead:verifer_node()).
extract_first_node() ->
    gen_server:call(?SERVER, extract_first_node).

-spec extract_nodes() -> list(aead:verifer_node()).
extract_nodes() ->
    gen_server:call(?SERVER, extract_nodes).

-spec top_node_to_string() -> string().
top_node_to_string() ->
    gen_server:call(?SERVER, top_node_to_string).

%%%=============================================================================
%%% aead_agent behaviour callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%   Determines if the leaf r contains the key or if r.key < r < r.nextLeafKey
%%   If neither of those holds, causes an exception.
%% @end
%%------------------------------------------------------------------------------
-spec key_matches_leaf(aead:key(), aead:leaf(), aed_agent:state()) ->
                              {boolean(), aed_agent:state()}.
key_matches_leaf(Key, Node, State) ->
    %% keyMatchesLeaf for the verifier is different than for the prover:
    %% since the verifier doesn't have keys in internal nodes, keyMatchesLeaf
    %% checks that the key is either equal to the leaf's key
    %% or is between the leaf's key and its nextLeafKey
    %% See https://eprint.iacr.org/2016/994 Appendix B paragraph "Our Algorithms"
    case aead:compare_keys(Key, aead:key(Node)) of
        -1 ->
            throw({error, key_missing_from_tree});
        0 ->
            {true, State};
        1 ->
            case aead:compare_keys(Key, aead:next_leaf_key(Node)) of
                -1 ->
                    throw({error, inconsistent_next_leaf_tree});
                _ ->
                    {false, State}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%%    Figures out whether to go left or right when from node r when searching
%%    for the key,
%%    using the appropriate bit in the directions bit string from the proof
%%
%%    return true if to go left, false if to go right in the search
%% @end
%%-------------------------------------------------------------------------------
-spec next_direction_is_left(aead:key(), aead:internal_node(), aed_agent:state()) ->
                                    {boolean(), aed_agent:state()}.
next_direction_is_left(_Key, _Node, #{directions_index := DirectionsIndex,
                                      proof            := Proof} = State) ->
    %% Decode bits of the proof as Booleans
    case binary:at(Proof, (DirectionsIndex bsr 3)) band
        (1 bsl (DirectionsIndex band 7)) of
        0 ->
            {false, State#{directions_index => DirectionsIndex + 1}};
        _ ->
            {true, State#{last_right_step => DirectionsIndex,
                          directions_index => DirectionsIndex + 1}}
    end.

%%-----------------------------------------------------------------------------
%% @doc
%%    return - A new verifier node with two leaves: Leaf on the left and a new
%%    leaf containing key and value on the right
%% @end
%%-----------------------------------------------------------------------------
-spec add_node(aead:leaf(), aead:key(), aead:value()) -> aead:internal_node().
add_node(Leaf, Key, Value) ->
    NK = aead:next_leaf_key(Leaf),
    #{visited => false,
      balance => 0,
      left    => Leaf#{next_leaf => NK},
      right   => #{key       => Key,
                   visited   => false,
                   is_new    => true,
                   next_leaf => Key,
                   value     => Value}}.

%%-----------------------------------------------------------------------------
%% @doc
%%   Deletions go down the tree twice -- once to find the leaf and realize
%%   that it needs to be deleted, and the second time to actually perform the
%%   deletion. This method will re-create comparison results using directions
%%   in the proof and lastRightStep variable. Each time it's called, it will
%%   give the next comparison result of key and node.key, where node starts at
%%   the root and progresses down the tree according to the comparison results.
%%
%%   Return result of previous comparison of key and relevant node's key
%% @end
%%-------------------------------------------------------------------------------
-spec replay_comparison(aead_agent:state()) -> {integer(), aead_agent:state()}.
replay_comparison(#{last_right_step := LastRightStep,
                    replay_index    := ReplayIndex,
                    proof           := Proof} = State) ->
    State2 = State#{replay_index => ReplayIndex + 1},
    case ReplayIndex of
        LastRightStep ->
            {0, State2};
        _ ->
            case {binary:at(Proof, ReplayIndex bsr 3) band (ReplayIndex band 7),
                  ReplayIndex < LastRightStep} of
                {0, true} ->
                    {1, State2};
                _ ->
                    {-1, State2}
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
-spec init(list(aead:digest() | aead:serialized_proof())) ->
                  {ok, verifier_state()} | {error, term()}.
init([Digest, Proof]) ->
    State0 = #{directions_index => 0,
               last_right_step  => 0,
               replay_index     => 0,
               directions       => [],
               proof            => Proof,
               max_operations   => {some, ?MAX_OPERATIONS},
               max_nodes        => none,
               max_deletes      => none,
               starting_digest  => Digest,
               %% inherited from aead_agent:state():
               root_node_height => 0,
               saved_node       => none,
               callback_module  => ?MODULE},
    reconstruct_tree(State0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), gen_server:server_ref(), verifier_state()) ->
                         {'reply', term(), verifier_state()}.
handle_call({perform, Operation}, _From, State) ->
    case perform(Operation, State) of
        {ok, {MaybeValue, State2}} ->
            {reply, MaybeValue, State2};
        {error, _} = Error ->
            lager:error("Operation ~p failed: ~p", [Operation, Error]),
            {reply, Error, State}
    end;
handle_call(digest, _From, State) ->
    {reply, digest(State), State};
handle_call({add_proof, Proof}, _From, State) ->
    case reconstruct_tree(State#{proof => Proof}) of
        {ok, State2} ->
            {reply, ok, State2};
        {error, _} = Error ->
            {reply, Error, State}
    end;
handle_call(extract_first_node, _From, State) ->
    {reply, extract_first_node(State), State};
handle_call(extract_nodes, _From, State) ->
    {reply, extract_nodes(State), State};
handle_call(top_node_to_string, _From, #{top_node := TopNode} = State) ->
    {reply, to_string(TopNode, 0), State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), verifier_state()) ->
                         {'noreply', verifier_state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), verifier_state()) ->
                         {'noreply', verifier_state()}.
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
-spec terminate(atom(), verifier_state()) -> 'ok'.
terminate(Reason, _State) ->
    lager:info("Verifier stopping: ~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%    Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), verifier_state(), term()) -> {'ok', verifier_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%    Will be None if the proof is not correct and thus a tree cannot be
%%    reconstructed
%% @end
%%------------------------------------------------------------------------------
-spec reconstruct_tree(verifier_state()) ->
                              {ok, verifier_state()} | {error, term()}.
reconstruct_tree(#{starting_digest := StartingDigest,
                   max_operations  := MaxOperations,
                   max_deletes     := MaxDeletes} = State) ->
    try
        case 8*size(StartingDigest) of
            ?LABEL_BITS + 8 ->
                ok;
            _ ->
                throw({error, incorrect_digest_size})
        end,
        LastByte = binary:last(StartingDigest),
        RootNodeHeight = LastByte band 16#ff,
        MaxNodes = get_max_nodes(MaxOperations, RootNodeHeight, MaxDeletes),

        %% Now reconstruct the tree from the proof, which has the post order
        %% traversal of the tree
        State2 = parse_proof(State#{max_nodes => {some, MaxNodes}}),
        {ok, State2}
    catch
        _ : Error ->
            Stack = erlang:get_stacktrace(),
            {Error, Stack}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%    Returns the current digest of the authenticated data structure,
%%    which contains the root hash and the root height
%% @end
%%------------------------------------------------------------------------------
-spec digest(verifier_state()) -> aead:digest().
digest(#{top_node := TopNode} = State) ->
    aead_agent:digest(TopNode, State).

%%------------------------------------------------------------------------------
%% @doc
%%    If operation.key exists in the tree and the operation succeeds,
%%    returns Success(Some(v)), where v is the value associated with
%%    operation.key before the operation.
%%    If operation.key does not exists in the tree and the operation succeeds,
%%    returns Success(None).
%%    Returns Failure if the operation fails or the proof does not verify.
%%    After one failure, all subsequent operations will fail and digest
%%    is None.
%%
%%    Returns: Success(Some(old value)), Success(None), or Failure
%% @end
%%------------------------------------------------------------------------------
-spec perform(aead_agent:operation(), verifier_state()) ->
                     aead:sucess(aead:maybe(aead:value()), verifier_state()).
perform(Operation, #{top_node := TopNode} = State) ->
    case aead_agent:perform(Operation, TopNode, State) of
        {ok, TopNode, MaybeValue, State2} ->
            {ok, {MaybeValue, State2#{top_node => TopNode}}};
        {error, _} = Error ->
            %% if topNode is None, the line above will fail and nothing will change
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%%    foldl over the leafs of a tree
%% @end
%%------------------------------------------------------------------------------
-spec traverse_tree(fun((aead:verifier_node(), term()) -> term()),
                term(), aead:verifier_node()) -> term().
traverse_tree(Fun, Acc, Node) ->
    case Node of
        ?VERIFIER_LEAF ->
            Fun(Node, Acc);
        {?LABEL_ONLY_NODE, true} ->
            Fun(Node, Acc);
        {?VERIFIER_INTERNAL_NODE, _} ->
            traverse_tree(Fun,
                          traverse_tree(Fun, Acc, aead:left(Node)),
                          aead:right(Node))
    end.

%%------------------------------------------------------------------------------
%% @doc
%%    Extract the first leaf
%% @end
%%------------------------------------------------------------------------------
-spec extract_first_node(verifier_state()) -> aead:maybe(aead:verifer_node()).
extract_first_node(#{top_node := TopNode}) ->
    traverse_tree(fun(Node, none) -> {some, Node};
                     (_Node, Res) -> Res
                  end,
                  none,
                  TopNode).

%%------------------------------------------------------------------------------
%% @doc
%%    Extract all the leafs
%% @end
%%------------------------------------------------------------------------------
-spec extract_nodes(verifier_state()) -> list(aead:verifer_node()).
extract_nodes(#{top_node := TopNode}) ->
    traverse_tree(fun(Node, Acc) ->
                          Acc ++ [Node]
                  end,
                  [],
                  TopNode).

-spec parse_proof(verifier_state()) ->
                         verifier_state().
parse_proof(#{proof := Proof} = State) ->
    {{Nodes, _}, State2} =
        parse_proof(Proof, {[], none}, State),

    %% check result
    case Nodes of
        [TopNode] ->
            Label = aead:label(TopNode),
            SDLen = 8*size(Label),
            case SDLen of
                ?LABEL_BITS ->
                    State3 = State2#{top_node => TopNode},
                    Digest = digest(State3),
                    State3#{starting_digest => Digest};
                LL ->
                    throw({error, {incorrect_root_label, LL}})
            end;
        [] ->
            lager:error("Proof did not contain changes"),
            State2;
        [_ | _] = What ->
            throw({error, {unused_nodes_in_proof, What}})
    end.

-spec parse_proof(aead_agent:serialized_proof(),
                  {list(aead:verifier_node()), aead:verifier_node()}, verifier_state()) ->
                         {{list(aead:verifier_node()), aead:verifier_node()}, verifier_state()}.
parse_proof(<<?END_OF_TREE_IN_PACKAGED_PROOF, Directions/binary>>, Acc,
            #{proof := Proof} = State) ->
    ProofSize = size(Proof),
    {Acc, State#{directions_index := ProofSize - size(Directions)}};
parse_proof(<<?LABEL_IN_PACKAGED_PROOF, Rest/binary>>, {Nodes, PreviousLeaf}, State) ->
    check_new_nodes_allowed(Nodes, State),
    <<Label:?LABEL_BITS, Rest2/binary>> = Rest,
    parse_proof(Rest2, {[aead:label_only_node(<<Label:?LABEL_BITS>>) | Nodes], none}, State);
parse_proof(<<?LEAF_IN_PACKAGED_PROOF, Rest/binary>>, {Nodes, PreviousLeaf}, State) ->
    check_new_nodes_allowed(Nodes, State),
    {Key, Rest2} =
        case PreviousLeaf of
            {some, PrevLeaf} ->
                %% take key from previous leaf
                {aead:next_leaf_key(PrevLeaf), Rest};
            none ->
                %% read key from proof
                <<K:?KEY_BITS, Rest20/binary>> = Rest,
                {<<K:?KEY_BITS>>, Rest20}
        end,
    <<NextLeafKey:?KEY_BITS, Rest3/binary>> = Rest2,
    {Value, Rest4} =
        case ?VALUE_BITS of
            {some, L} ->
                <<V:L, Rest40/binary>> = Rest3,
                {V, Rest40};
            none ->
                <<VLength:32, Rest40/binary>> = Rest3,
                VLengthBits = 8*VLength,
                <<V:VLengthBits, Rest41/binary>> = Rest40,
                {V, Rest41}
        end,
    Leaf = #{key       => Key,
             visited   => false,
             next_leaf => <<NextLeafKey:?KEY_BITS>>,
             value     => Value},
    parse_proof(Rest4, {[Leaf | Nodes], {some, Leaf}}, State);
parse_proof(<<Balance:8, Rest/binary>>, {Nodes, PreviousLeaf}, State) ->
    %% Must be an internal node
    [NodeR, NodeL | Nodes2] = Nodes,
    INode = #{visited => false,
              balance => Balance,
              left    => NodeL,
              right   => NodeR},
    parse_proof(Rest, {[INode | Nodes2], PreviousLeaf}, State).


check_new_nodes_allowed(Nodes, #{max_nodes := MaxNodes}) ->
    case MaxNodes of
        none ->
            ok;
        {some, Max} ->
            case length(Nodes) of
                L when L >= Max ->
                    throw({error, proof_too_long});
                _ ->
                    ok
            end
    end.

get_max_nodes(none, _RootNodeHeight, _MaxDeletes) ->
    0;
get_max_nodes({some, MaxOperations}, RootNodeHeight, MaxDeletes) ->
    %% compute the maximum number of nodes the proof
    %% can contain according to
    %% https://eprint.iacr.org/2016/994 Appendix B last paragraph
    %%
    %% compute maximum height that the tree can be before an operation
    RealNumOperations = MaxOperations,
    LogNumOps = log_num_ops(MaxOperations),
    %% this will replace 1.4405 from the paper with 1.5 and
    %% will round down, which is safe, because hnew is an integer
    HNew = 3 * (1 + max(RootNodeHeight, LogNumOps))/2,
    RealMaxDeletes = case MaxDeletes of
                         none -> MaxOperations;
                         MD -> MD
                     end,
    (RealNumOperations + RealMaxDeletes) * (2 * RootNodeHeight + 1) +
        %% +1 needed in case numOperations == 0
        RealMaxDeletes * HNew + 1.

log_num_ops(MaxNumOperations) ->
    log_num_ops(MaxNumOperations, 1, 0).

log_num_ops(MaxNumOps, T, L) when T < MaxNumOps ->
    log_num_ops(MaxNumOps, 2* T, L + 1);
log_num_ops(_MaxNumOps, _T, L) ->
    L.

%%------------------------------------------------------------------------------
%% @doc
%%    For debugging
%% @end
%%------------------------------------------------------------------------------
-spec to_string(aead:verifier_node(), integer()) -> string().
to_string(none, _Depth) ->
    "none";
to_string({some, Node}, Depth) ->
    aead:to_string(Node, Depth).
