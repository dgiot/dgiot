%% File    : mini.erl
%% Author  : Henning Diedrich
%% File    : luerl/examples/minibench/minibench.erl
%% Purpose : Benchmark for frequent calls to small Luerl scripts
%% Author  : Henning Diedrich
%% Use     $ cd ./examples/minibench 
%%         $ erlc minibench.erl 
%%         $ erl -pa ../../ebin -s minibench run -s init stop -noshell
%% Or      $ make minibench

-module(minibench).
-export([run/0]).

run() ->

    io:format("----------------------------------------------------------~n"),
    io:format("This is a benchmark of frequent fast calls into Luerl.~n"),

    % I. eval and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, parse and execute '1 + 1'~n"),
    I1 = 100000,
    {T1,_State} = timer:tc(fun() -> do_loop(I1, "return 1 + 1") end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T1,I1]),
    io:format("Per call: ~p microseconds.~n", [T1/I1]),
    

    % II. eval once, then only execute
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, and execute pre-parsed '1 + 1'~n"),
    I2 = 100000,
    {ok, Chunk2, _St} = luerl:load("return 1 + 1"),
    
    {T2,_State21} = timer:tc(fun() -> do_loop(I2, Chunk2) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T2,I2]),
    io:format("Per call: ~p microseconds.~n", [T2/I2]),
    

    % III. eval once, then only execute
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parse execute '1 + 1', re-using same state~n"),
    I3 = 100000,
    State3 = luerl:init(),
    {ok, Chunk3, State31} = luerl:load("return 1 + 1", State3),
    
    {T3,_State31} = timer:tc(fun() -> do_loop_state(I3, Chunk3, State31) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T3,I3]),
    io:format("Per call: ~p microseconds.~n", [T3/I3]),


    % IV. measure but state initialization
    io:format("----------------------------------------------------------~n"),
    io:format("Pure initialization of Lua state~n"),
    I4 = 100000,
    
    {T4,_State41} = timer:tc(fun() -> [luerl:init() || _ <- lists:seq(1,I4)] end),

    io:format("Adding Up: ~p microseconds for ~p x initializing Lua state.~n", [T4,I4]),
    io:format("Per call: ~p microseconds.~n", [T4/I4]),


    % V. eval once, then only execute, re-use previous state
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parsed '1 + 1', re-using state from last result~n"),
    I5 = 100000,
    State5 = luerl:init(),
    {ok, Chunk5, State51} = luerl:load("return 1 + 1", State5),
    
    {T5,_State51} = timer:tc(fun() -> do_loop_chain(I5, Chunk5, State51) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T5,I5]),
    io:format("Per call: ~p microseconds.~n", [T5/I5]),


    % VI. measure but parsing
    io:format("----------------------------------------------------------~n"),
    io:format("Pure parsing~n"),
    I6 = 100000,
    {T6,_State61} = timer:tc(fun() -> [luerl:load("return 1 + 1") || _ <- lists:seq(1,I6)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T6,I6]),
    io:format("Per call: ~p microseconds.~n", [T6/I6]),


    % VII. Parse and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Parse and execute '1 + 1', re-using state~n"),
    I7 = 100000,
    State7 = luerl:init(),
    {T7,_State71} = timer:tc(fun() -> do_loop_state(I7, "return 1 + 1", State7) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T7,I7]),
    io:format("Per call: ~p microseconds.~n", [T7/I7]),


    done.

% helper
selffeed(State, _Chunk, 0) -> State;
selffeed(State, Chunk, I) -> 
    {ok,[2.0],State1} = luerl:do(Chunk, State),
    selffeed(State1, Chunk, I-1).

do_loop(N, Chunk) when N > 0 ->
    luerl:do(Chunk),
    do_loop(N-1, Chunk);
do_loop(0, _) -> ok.

do_loop_state(N, Chunk, State) when N > 0 ->
    luerl:do(Chunk, State),
    do_loop_state(N-1, Chunk, State);
do_loop_state(0, _, _) -> ok.

do_loop_chain(N, Chunk, State0) when N > 0 ->
    {_,State1} = luerl:do(Chunk, State0),
    do_loop_chain(N-1, Chunk, State1);
do_loop_chain(0, _, _) -> ok.
