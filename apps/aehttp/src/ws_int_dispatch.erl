-module(ws_int_dispatch).

-export([execute/2]).

-spec execute(Action :: binary(), Payload :: list()) -> {ok, list()} |
                                                        {error, binary()}.
execute(Action, Payload) ->
    case is_valid(Action, Payload) of
        {false, Reason} ->
            {error, Reason};
        true ->
            case do_execute(Action, Payload) of
                {ok, _} = OK ->
                    OK;
                {error, _} = Err ->
                    Err
            end
    end.

is_valid(<<"invalid_message">>, _SomePayload) ->
    {false, <<"some reason">>};
is_valid(_, _) ->
    true.

do_execute(<<"silence_dialyzer">>, _) ->
    {error, <<"oh, no">>};
do_execute(<<"hello">>, _) ->
    {ok, [{resp, <<"world">>}]}.

