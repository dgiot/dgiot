-module(poolboy_eqc).
-compile([export_all]).

-ifdef(TEST).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include_lib("eunit/include/eunit.hrl").

poolboy_test_() ->
    {timeout, 20,
        fun() ->
                ?assert(eqc:quickcheck(eqc:testing_time(4,
                            poolboy_eqc:prop_sequential()))),
                ?assert(eqc:quickcheck(eqc:testing_time(4,
                            poolboy_eqc:prop_parallel())))
        end
    }.

-record(state,
    {
        pid,
        size,
        max_overflow,
        checked_out = []
    }).

initial_state() ->
    #state{}.

command(S) ->
    oneof(
        [{call, ?MODULE, start_poolboy, make_args(S, nat(), nat())} || S#state.pid == undefined] ++
            [{call, ?MODULE, stop_poolboy, [S#state.pid]} || S#state.pid /= undefined] ++
            [{call, ?MODULE, checkout_nonblock, [S#state.pid]} || S#state.pid /= undefined] ++
            %% checkout shrinks to checkout_nonblock so we can simplify counterexamples
            [{call, ?MODULE, ?SHRINK(checkout_block, [checkout_nonblock]), [S#state.pid]} || S#state.pid /= undefined] ++
            [{call, ?MODULE, checkin, [S#state.pid, fault({call, ?MODULE, spawn_process, []}, elements(S#state.checked_out))]} || S#state.pid /= undefined, S#state.checked_out /= []] ++
            [{call, ?MODULE, kill_worker, [elements(S#state.checked_out)]} || S#state.pid /= undefined, S#state.checked_out /= []] ++
            [{call, ?MODULE, kill_idle_worker, [S#state.pid]} || S#state.pid /= undefined] ++
            [{call, ?MODULE, spurious_exit, [S#state.pid]} || S#state.pid /= undefined]
    ).

make_args(_S, Size, Overflow) ->
    [[{size, Size}, {max_overflow, Overflow}, {worker_module, poolboy_test_worker}, {name, {local, poolboy_eqc}}]].

spawn_process() ->
    {spawn(fun() ->
                timer:sleep(5000)
        end), self()}.

spawn_linked_process(Pool) ->
    Parent = self(),
    Pid = spawn(fun() ->
                    link(Pool),
                    Parent ! {linked, self()},
                    timer:sleep(5000)
            end),
    receive
        {linked, Pid} ->
            Pid
    end.

start_poolboy(Args) ->
    {ok, Pid} = poolboy:start_link(Args),
    Pid.

stop_poolboy(Pid) ->
    gen_server:call(Pid, stop),
    timer:sleep(1).

checkout_nonblock(Pool) ->
    {poolboy:checkout(Pool, false), self()}.

checkout_block(Pool) ->
    {catch(poolboy:checkout(Pool, true, 100)), self()}.

checkin(Pool, {Worker, _}) ->
    Res = poolboy:checkin(Pool, Worker),
    gen_server:call(Pool, get_avail_workers),
    Res.

kill_worker({Worker, _}) ->
    exit(Worker, kill),
    timer:sleep(1),
    Worker.

kill_idle_worker(Pool) ->
    Pid = poolboy:checkout(Pool, false),
    case Pid of
        _ when is_pid(Pid) ->
            poolboy:checkin(Pool, Pid),
            kill_worker({Pid, self()});
        _ ->
            timer:sleep(1),
            kill_idle_worker(Pool)
    end.

spurious_exit(Pool) ->
    Pid = spawn_linked_process(Pool),
    exit(Pid, kill).

precondition(S,{call,_,start_poolboy,_}) ->
    %% only start new pool when old one is stopped
  S#state.pid == undefined;
precondition(S,_) when S#state.pid == undefined ->
    %% all other states need a running pool
    false;
precondition(S, {call, _, kill_worker, [Pid]}) ->
    lists:member(Pid, S#state.checked_out);
precondition(S,{call,_,kill_idle_worker,[_Pool]}) ->
    length(S#state.checked_out) < S#state.size;
precondition(S,{call,_,checkin,[_Pool, Pid]}) ->
    lists:member(Pid, S#state.checked_out);
precondition(_S,{call,_,_,_}) ->
    true.

%% check model state against internal state, only used in sequential tests
invariant(S = #state{pid=Pid},_) when Pid /= undefined ->
    State = if length(S#state.checked_out) == S#state.size + S#state.max_overflow ->
            full;
        length(S#state.checked_out) >= S#state.size ->
            overflow;
        true ->
            ready
    end,

    Workers = max(0, S#state.size - length(S#state.checked_out)),
    OverFlow = max(0, length(S#state.checked_out) - S#state.size),
    Monitors = length(S#state.checked_out),

    RealStatus = gen_server:call(Pid, status),
    case RealStatus == {State, Workers, OverFlow, Monitors} of
        true ->
            true;
        _ ->
            {wrong_state, RealStatus, {State, Workers, OverFlow, Monitors}}
    end;
invariant(_,_) ->
    true.

%% what states block
blocking(S, {call, _, checkout_block, _}) ->
    %% blocking checkout can block if we expect a checkout to fail
    not checkout_ok(S);
blocking(_, _) ->
    false.

postcondition(S,{call,_,checkout_block,[_Pool]},R) ->
    case R of
        {{'EXIT', {timeout, _}}, _} ->
            case length(S#state.checked_out) >= S#state.size + S#state.max_overflow of
                true ->
                    true;
                _ ->
                    {checkout_block, R}
            end;
        _ ->
            case length(S#state.checked_out) < S#state.size + S#state.max_overflow of
                true ->
                    true;
                _ ->
                    {checkout_block, R}
            end
    end;
postcondition(S,{call,_,checkout_nonblock,[_Pool]},R) ->
    case R of
        {full, _} ->
            case length(S#state.checked_out) >= S#state.size + S#state.max_overflow of
                true ->
                    true;
                _ ->
                    {checkout_nonblock, R}
            end;
        _ ->
            case length(S#state.checked_out) < S#state.size + S#state.max_overflow of
                true ->
                    true;
                _ ->
                    {checkout_block, R}
            end
    end;
postcondition(_S, {call,_,checkin,_}, R) ->
    case R of
        ok ->
            true;
        _ ->
            {checkin, R}
    end;
postcondition(_S,{call,_,_,_},_R) ->
    true.

next_state(S,V,{call,_,start_poolboy, [Args]}) ->
    S#state{pid=V,
        size=proplists:get_value(size, Args),
        max_overflow=proplists:get_value(max_overflow, Args)
    };
next_state(S,_V,{call,_,stop_poolboy, [_Args]}) ->
    S#state{pid=undefined, checked_out=[]};
next_state(S,V,{call,_,checkout_block,_}) ->
    %% if the model says the checkout worked, store the result
    case checkout_ok(S) of
        false ->
            S;
        _ ->
            S#state{checked_out=S#state.checked_out++[V]}
    end;
next_state(S,V,{call,_,checkout_nonblock,_}) ->
    %% if the model says the checkout worked, store the result
    case checkout_ok(S) of
        false ->
            S;
        _ ->
            S#state{checked_out=S#state.checked_out++[V]}
    end;
next_state(S,_V,{call, _, checkin, [_Pool, Worker]}) ->
    S#state{checked_out=S#state.checked_out -- [Worker]};
next_state(S,_V,{call, _, kill_worker, [Worker]}) ->
    S#state{checked_out=S#state.checked_out -- [Worker]};
next_state(S,_V,{call, _, kill_idle_worker, [_Pool]}) ->
    S;
next_state(S,_V,{call, _, spurious_exit, [_Pool]}) ->
    S;
next_state(S,V,{call, erlang, self, []}) ->
    %% added after test generation, values are never symbolic
    S#state{checked_out=[{Worker, Pid} || {Worker, Pid} <- S#state.checked_out, Pid /= V]}.


prop_sequential() ->
    fault_rate(1, 10,
        ?FORALL(Cmds,commands(?MODULE),
            ?TRAPEXIT(
                aggregate(command_names(Cmds),
                    begin
                            {H,S,Res} = run_commands(?MODULE,Cmds),
                            catch(stop_poolboy(whereis(poolboy_eqc))),
                            ?WHENFAIL(io:format("History: ~p\nState: ~p\nRes: ~p\n~p\n",
                                    [H,S,Res, zip(Cmds, [Y || {_, Y} <- H])]),
                                Res == ok)
                    end)))).

prop_parallel() ->
    fault_rate(1, 10,
        ?FORALL(Cmds={Seq,Par},parallel_commands(?MODULE),
            ?TRAPEXIT(
                aggregate(command_names(Cmds),
                    begin
                            NewPar = [P ++ [{set, {var, 0}, {call, erlang, self, []}}] || P <- Par],
                            {H,S,Res} = run_parallel_commands(?MODULE,{Seq,NewPar}),
                            catch(stop_poolboy(whereis(poolboy_eqc))),
                            ?WHENFAIL(io:format("History: ~p\nState: ~p\nRes: ~p\n",
                                    [H,S,Res]),
                                Res == ok)
                    end)))).


checkout_ok(S) ->
    length(S#state.checked_out) < S#state.size + S#state.max_overflow.

-endif.
-endif.
