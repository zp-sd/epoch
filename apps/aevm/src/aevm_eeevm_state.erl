-module(aevm_eeevm_state).
%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle the machine state for the EEEVM.
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-export([ caller/1
	, code/1
	, cp/1
	, data/1
	, gas/1
	, init/1
	, mem/1
	, set_call/2
	, set_code/2
	, set_cp/2
	, set_gas/2
	, set_mem/2
	, set_out/2
	, set_selfdestruct/2
	, set_stack/2
	, set_storage/2
	, stack/1
	, storage/1
	, trace_format/3
	]).

init(Spec) ->
    Exec = maps:get(exec, Spec),
    Opts = maps:get(opts, Spec),
    Code = maps:get(code, Exec),
    Data = maps:get(data, Exec),
    Caller = maps:get(caller, Exec),
    Gas = maps:get(gas, Exec),
    Trace = maps:get(trace, Opts, false),
    TraceFun = maps:get(trace_fun, Opts, fun(S,A) -> io:format(S,A) end),

    #{ stack     => []
     , memory    => #{}
     , storage   => #{}   %% For now. Should be permanent.
     , code      => Code
     , data      => Data
     , caller    => Caller
     , gas       => Gas
     , out       => <<>>
     , cp        => 0
     , do_trace  => Trace
     , trace_fun => TraceFun
     , trace     => []
     }.


caller(State)    -> maps:get(caller, State).
cp(State)        -> maps:get(cp, State).
code(State)      -> maps:get(code, State).
data(State)      -> maps:get(data, State).
stack(State)     -> maps:get(stack, State).
mem(State)       -> maps:get(memory, State).
gas(State)       -> maps:get(gas, State).
storage(State)   -> maps:get(storage, State).

do_trace(State)  -> maps:get(do_trace, State).
trace(State)     -> maps:get(trace, State).
trace_fun(State) -> maps:get(trace_fun, State).


set_call(Value, State)    -> maps:put(call, Value, State).
set_cp(Value, State)      -> maps:put(cp, Value, State).
set_code(Value, State)    -> maps:put(code, Value, State).
set_stack(Value, State)   -> maps:put(stack, Value, State).
set_mem(Value, State)     -> maps:put(memory, Value, State).
set_out(Value, State)     -> maps:put(out, Value, State).
set_gas(Value, State)     -> maps:put(gas, Value, State).
set_storage(Value, State) -> maps:put(storage, Value, State).
set_selfdestruct(Value, State) -> maps:put(selfdestruct, Value, State).
  

add_trace(T, State) ->
    Trace = maps:get(trace, State),
    maps:put(trace, Trace ++ [T], State).

trace_format(String, Argument, State) ->
    CP   = aevm_eeevm_state:cp(State),
    Code = aevm_eeevm_state:code(State),
    OP   = aevm_eeevm:code_get_op(CP, Code),
    case do_trace(State) of
	true ->
	    F = trace_fun(State),
	    F("~8.16.0B : ~2.16.0B", [CP, OP]),
	    F(" ~w", [stack(State)]),
	    F(String, Argument),
	    add_trace({CP, OP}, State);
	false ->
	    State
    end.