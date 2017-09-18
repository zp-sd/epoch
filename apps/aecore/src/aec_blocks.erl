-module(aec_blocks).

%% API
-export([prev_hash/1,
         height/1,
         trees/1,
         difficulty/1,
         set_nonce/2,
         new/3,
         to_header/1,
         serialize_to_binary/1,
         deserialize_from_binary/1,
         serialize_to_map/1,
         deserialize_from_map/1,
         hash/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-export_type([serialized_to_binary/0,
              serialized_to_map/0]).


-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(CURRENT_BLOCK_VERSION, 1).

-type serialized_to_binary() :: binary().
-type serialized_to_map() :: map().
-type block_deserialized() :: #block{trees :: DummyTrees::trees()}.

prev_hash(Block) ->
    Block#block.prev_hash.

height(Block) ->
    Block#block.height.

trees(Block) ->
    Block#block.trees.

difficulty(Block) ->
    Block#block.difficulty.

set_nonce(Block, Nonce) ->
    Block#block{nonce = Nonce}.

-spec new(block(), list(signed_tx()), trees()) -> {ok, block()} | {error, term()}.
new(LastBlock, Txs, Trees0) ->
    LastBlockHeight = height(LastBlock),
    {ok, LastBlockHeaderHash} = hash(LastBlock),
    Height = LastBlockHeight + 1,
    case aec_tx:apply_signed(Txs, Trees0, Height) of
        {ok, Trees} ->
            {ok, #block{height = Height,
                        prev_hash = LastBlockHeaderHash,
                        root_hash = aec_trees:all_trees_hash(Trees),
                        trees = Trees,
                        txs = Txs,
                        difficulty = difficulty(LastBlock),
                        time = aeu_time:now_in_msecs(),
                        version = ?CURRENT_BLOCK_VERSION}};
        {error, _Reason} = Error ->
            Error
    end.

-spec to_header(block()) -> header().
to_header(#block{height = Height,
                 prev_hash = PrevHash,
                 root_hash = RootHash,
                 difficulty = Difficulty,
                 nonce = Nonce,
                 time = Time,
                 version = Version}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            difficulty = Difficulty,
            nonce = Nonce,
            time = Time,
            version = Version}.

-spec serialize_to_map(block() | block_deserialized) -> {ok, serialized_to_map()}.
serialize_to_map(B) ->
    Serialized =
      #{<<"height">> => height(B),
        <<"prev-hash">> => base64:encode(prev_hash(B)),
        <<"root-hash">> => base64:encode(B#block.root_hash),
        <<"difficulty">> => B#block.difficulty,
        <<"nonce">> => B#block.nonce,
        <<"time">> => B#block.time,
        <<"version">> => B#block.version,
        <<"txs">> => [] %TODO txs serialization
      },
    {ok, Serialized}.

-spec deserialize_from_map(map()) -> {ok, block_deserialized()}.
deserialize_from_map(B) ->
    #{<<"height">> := Height,
      <<"prev-hash">> := PrevHash,
      <<"root-hash">> := RootHash,
      <<"difficulty">> := Difficulty,
      <<"nonce">> := Nonce,
      <<"time">> := Time,
      <<"version">> := Version,
      <<"txs">> := _Txs %TODO txs deserialization
      } = B,
    {ok, #block{height = Height,
                prev_hash = base64:decode(PrevHash),
                root_hash = base64:decode(RootHash),
                difficulty = Difficulty,
                nonce = Nonce,
                time = Time,
                version = Version}}.

-spec serialize_to_binary(block() | block_deserialized()) -> {ok, serialized_to_binary()}.
serialize_to_binary(B) ->
    {ok, Map} = serialize_to_map(B),
    {ok, jsx:encode(Map)}.

-spec deserialize_from_binary(serialized_to_binary()) -> {ok, block_deserialized()}.
deserialize_from_binary(B) ->
    deserialize_from_map(jsx:decode(B, [return_maps])).    

-spec hash(block()) -> {ok, block_header_hash()}.
hash(B) ->
    aec_headers:hash(to_header(B)).
