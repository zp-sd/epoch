-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([broadcast/1]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    gproc:reg({p, l, {?MODULE, broadcast}}),
    {ok, Req, undefined_state}.

websocket_handle({text, MsgBin}, Req, State) ->
    #{<<"action">> := Action, <<"payload">> := Payload}
        = jsx:decode(MsgBin, [return_maps]),
    Response0 = ws_int_dispatch:execute(Action, Payload),
    Response =
        case Response0 of
            {error, ErrMsg} ->
                [{status, error}, {message, ErrMsg}];
            {ok, R} when is_list(R) ->
                [{status, ok}, {result, R}]
        end,
    {reply, {text, jsx:encode(Response)}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

broadcast(Msg) ->
    gproc:send({p, l, {?MODULE, broadcast}}, {send, Msg}).

