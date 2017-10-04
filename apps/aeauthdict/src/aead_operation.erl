%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    Operations on elements of an AD tree
%%% @end
%%%-------------------------------------------------------------------
-module(aead_operation).

-export([key/1,
         lookup/1,
         insert/2,
         update/2,
         insert_or_update/2,
         remove/1,
         remove_if_exists/1,
         update_long_by/2]).


-type success(T) :: {ok, T} | {error, term()}.

%%------------------------------------------------------------------------------
%% @doc
%%   Update functions takes Option[oldValue] and return Try[Option[newValue]]
%%   For example:
%%   Insert: None => Success(Some(newValue)), but Some(oldValue) => Failure()
%%   Update: Some(oldValue) => Success(Some(newValue))
%%   Delete: Some(oldValue) => Success(None), but None => Failure()
%%   ConditionalUpdate: Some(oldValue) => Success(Some(newValue)) or Failure(),
%%   depending on whether oldValue satisfied some desired conditions
%% @end
%%------------------------------------------------------------------------------
-type operation_type() :: lookup | modification.
-type update_fun() :: fun((aead:maybe(aead:value())) ->
                           success(aead:maybe(aead:value()))).
-type operation() :: {operation_type(),
                      aead:key(),
                      update_fun()}.

-export_type([success/1,
              operation/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec key(operation()) -> aead:key().
key({_, Key, _}) ->
    Key.

-spec lookup(aead:key()) -> operation().
lookup(Key) ->
    {lookup, Key,
     fun({some, V}) -> {ok, {some, V}};
        (none)      -> {error, not_found}
     end}.

-spec insert(aead:key(), aead:value()) -> operation().
insert(Key, Value) ->
    {modification, Key,
     fun(none)      -> {ok, {some, Value}};
        ({some, _}) -> {error, already_present}
     end}.

-spec update(aead:key(), aead:value()) -> operation().
update(Key, NewValue) ->
    {modification, Key,
     fun({some, _OldVlaue}) -> {ok, {some, NewValue}};
        (none)              -> {error, not_found}
     end}.

-spec insert_or_update(aead:key(), aead:value()) -> operation().
insert_or_update(Key, Value) ->
    {modification, Key,
     fun(_) -> {ok, {some, Value}} end}.

-spec remove(aead:key()) -> operation().
remove(Key) ->
    {modification, Key,
     fun({some, _V}) -> {ok, none};
        (none)       -> {error, not_found}
     end}.

-spec remove_if_exists(aead:key()) -> operation().
remove_if_exists(Key) ->
    {modification, Key,
     fun(_) -> {ok, none} end}.

%%------------------------------------------------------------------------------
%% @doc
%%   If the key exists in the tree, add delta to its value, fail if
%%   the result is negative, and remove the key if the result is equal to 0.
%%   If the key does not exist in the tree, treat it as if its value is 0:
%%   insert the key with value delta if delta is positive,
%%   fail if delta is negative, and do nothing if delta is 0.
%% @end
%%------------------------------------------------------------------------------
-spec update_long_by(aead:key(), integer()) -> operation().
update_long_by(Key, 0) ->
    {modification, Key,
     fun(MaybeValue) -> {ok, MaybeValue} end};
update_long_by(Key, Delta) when Delta > 0 ->
    {modification, Key,
     fun(none)      -> {ok, {some, aead:from_integer(Delta)}};
        ({some, V}) ->
             case aead:to_integer(V) + Delta of
                 0 ->
                     {ok, none};
                 NewValue when NewValue > 0 ->
                     {ok, {some, aead:from_integer(NewValue)}};
                 _Negative ->
                     {error, new_value_is_negative}
             end
     end}.

