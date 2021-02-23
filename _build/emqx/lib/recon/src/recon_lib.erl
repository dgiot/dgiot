%%% @author Fred Hebert <mononcqc@ferd.ca>
%%%  [http://ferd.ca/]
%%% @doc Regroups useful functionality used by recon when dealing with data
%%% from the node. The functions in this module allow quick runtime access
%%% to fancier behaviour than what would be done using recon module itself.
%%% @end
-module(recon_lib).
-export([sliding_window/2, sample/2, count/1,
         port_list/1, port_list/2,
         proc_attrs/1, proc_attrs/2,
         inet_attrs/1, inet_attrs/2,
         triple_to_pid/3, term_to_pid/1,
         term_to_port/1,
         time_map/5, time_fold/6,
         scheduler_usage_diff/2,
         sublist_top_n_attrs/2]).
%% private exports
-export([binary_memory/1]).

-type diff() :: [recon:proc_attrs() | recon:inet_attrs()].

%% @doc Compare two samples and return a list based on some key. The type mentioned
%% for the structure is `diff()' (`{Key,Val,Other}'), which is compatible with
%% the {@link recon:proc_attrs()} type.
-spec sliding_window(First::diff(), Last::diff()) -> diff().
sliding_window(First, Last) ->
    Dict = lists:foldl(
        fun({Key, {Current, Other}}, Acc) ->
            dict:update(Key,
                        fun({Old,_Other}) -> {Current-Old, Other} end,
                        {Current, Other},
                        Acc)
        end,
        dict:from_list([{K,{V,O}} || {K,V,O} <- First]),
        [{K,{V,O}} || {K,V,O} <- Last]
    ),
    [{K,V,O} || {K,{V,O}} <- dict:to_list(Dict)].

%% @doc Runs a fun once, waits `Ms', runs the fun again,
%% and returns both results.
-spec sample(Ms::non_neg_integer(), fun(() -> term())) ->
      {First::term(), Second::term()}.
sample(Delay, Fun) ->
    First = Fun(),
    timer:sleep(Delay),
    Second = Fun(),
    {First, Second}.

%% @doc Takes a list of terms, and counts how often each of
%% them appears in the list. The list returned is in no
%% particular order.
-spec count([term()]) -> [{term(), Count::integer()}].
count(Terms) ->
    Dict = lists:foldl(
        fun(Val, Acc) ->  dict:update_counter(Val, 1, Acc) end,
        dict:new(),
        Terms
    ),
    dict:to_list(Dict).

%% @doc Returns a list of all the open ports in the VM, coupled with
%% one of the properties desired from `erlang:port_info/1-2'.
-spec port_list(Attr::atom()) -> [{port(), term()}].
port_list(Attr) ->
    [{Port,Val} || Port <- erlang:ports(),
                   {_, Val} <- [erlang:port_info(Port, Attr)]].

%% @doc Returns a list of all the open ports in the VM, but only
%% if the `Attr''s resulting value matches `Val'. `Attr' must be
%% a property accepted by `erlang:port_info/2'.
-spec port_list(Attr::atom(), term()) -> [port()].
port_list(Attr, Val) ->
    [Port || Port <- erlang:ports(),
             {Attr, Val} =:= erlang:port_info(Port, Attr)].

%% @doc Returns the attributes ({@link recon:proc_attrs()}) of
%% all processes of the node, except the caller.
-spec proc_attrs(term()) -> [recon:proc_attrs()].
proc_attrs(AttrName) ->
    Self = self(),
    [Attrs || Pid <- processes(),
	      Pid =/= Self,
              {ok, Attrs} <- [proc_attrs(AttrName, Pid)]
	].

%% @doc Returns the attributes of a given process. This form of attributes
%% is standard for most comparison functions for processes in recon.
%%
%% A special attribute is `binary_memory', which will reduce the memory used
%% by the process for binary data on the global heap.
-spec proc_attrs(term(), pid()) -> {ok, recon:proc_attrs()} | {error, term()}.
proc_attrs(binary_memory, Pid) ->
    case process_info(Pid, [binary, registered_name,
                            current_function, initial_call]) of
        [{_, Bins}, {registered_name,Name}, Init, Cur] ->
            {ok, {Pid, binary_memory(Bins), [Name || is_atom(Name)]++[Init, Cur]}};
        undefined ->
            {error, undefined}
    end;
proc_attrs(AttrName, Pid) ->
    case process_info(Pid, [AttrName, registered_name,
                            current_function, initial_call]) of
        [{_, Attr}, {registered_name,Name}, Init, Cur] ->
            {ok, {Pid, Attr, [Name || is_atom(Name)]++[Init, Cur]}};
        undefined ->
            {error, undefined}
    end.

%% @doc Returns the attributes ({@link recon:inet_attrs()}) of
%% all inet ports (UDP, SCTP, TCP) of the node.
-spec inet_attrs(term()) -> [recon:inet_attrs()].
inet_attrs(AttrName) ->
    Ports = [Port || Port <- erlang:ports(),
                     {_, Name} <- [erlang:port_info(Port, name)],
                     Name =:= "tcp_inet" orelse
                     Name =:= "udp_inet" orelse
                     Name =:= "sctp_inet"],
    [Attrs || Port <- Ports,
              {ok, Attrs} <- [inet_attrs(AttrName, Port)]].

%% @doc Returns the attributes required for a given inet port (UDP,
%% SCTP, TCP). This form of attributes is standard for most comparison
%% functions for processes in recon.
-spec inet_attrs(AttributeName, port()) -> {ok,recon:inet_attrs()}
                                         | {error,term()} when
      AttributeName :: 'recv_cnt' | 'recv_oct' | 'send_cnt' | 'send_oct'
                     | 'cnt' | 'oct'.
inet_attrs(Attr, Port) ->
    Attrs = case Attr of
        cnt -> [recv_cnt, send_cnt];
        oct -> [recv_oct, send_oct];
        _ -> [Attr]
    end,
    case inet:getstat(Port, Attrs) of
        {ok, Props} ->
            ValSum = lists:foldl(fun({_,X},Y) -> X+Y end, 0, Props),
            {ok, {Port,ValSum,Props}};
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Equivalent of `pid(X,Y,Z)' in the Erlang shell.
-spec triple_to_pid(N,N,N) -> pid() when
    N :: non_neg_integer().
triple_to_pid(X, Y, Z) ->
    list_to_pid("<" ++ integer_to_list(X) ++ "." ++
                       integer_to_list(Y) ++ "." ++
                       integer_to_list(Z) ++ ">").

%% @doc Transforms a given term to a pid.
-spec term_to_pid(recon:pid_term()) -> pid().
term_to_pid(Pid) when is_pid(Pid) -> Pid;
term_to_pid(Name) when is_atom(Name) -> whereis(Name);
term_to_pid(List = "<0."++_) -> list_to_pid(List);
term_to_pid(Binary = <<"<0.", _/binary>>) -> list_to_pid(binary_to_list(Binary));
term_to_pid({global, Name}) -> global:whereis_name(Name);
term_to_pid({via, Module, Name}) -> Module:whereis_name(Name);
term_to_pid({X,Y,Z}) when is_integer(X), is_integer(Y), is_integer(Z) ->
    triple_to_pid(X,Y,Z).

%% @doc Transforms a given term to a port
-spec term_to_port(recon:port_term()) -> port().
term_to_port(Port) when is_port(Port) -> Port;
term_to_port(Name) when is_atom(Name) -> whereis(Name);
term_to_port("#Port<0."++Id) ->
    N = list_to_integer(lists:sublist(Id, length(Id)-1)), % drop trailing '>'
    term_to_port(N);
term_to_port(N) when is_integer(N) ->
    %% We rebuild the term from the int received:
    %% http://www.erlang.org/doc/apps/erts/erl_ext_dist.html#id86892
    Name = iolist_to_binary(atom_to_list(node())),
    NameLen = iolist_size(Name),
    Vsn = binary:last(term_to_binary(self())),
    Bin = <<131, % term encoding value
            102, % port tag
            100, % atom ext tag, used for node name
            NameLen:2/unit:8,
            Name:NameLen/binary,
            N:4/unit:8, % actual counter value
            Vsn:8>>, % version
    binary_to_term(Bin).

%% @doc Calls a given function every `Interval' milliseconds and supports
%% a map-like interface (each result is modified and returned)
-spec time_map(N, Interval, Fun, State, MapFun) -> [term()] when
    N :: non_neg_integer(),
    Interval :: pos_integer(),
    Fun :: fun((State) -> {term(), State}),
    State :: term(),
    MapFun :: fun((_) -> term()).
time_map(0, _, _, _, _) ->
    [];
time_map(N, Interval, Fun, State, MapFun) ->
    {Res, NewState} = Fun(State),
    timer:sleep(Interval),
    [MapFun(Res) | time_map(N-1,Interval,Fun,NewState,MapFun)].

%% @doc Calls a given function every `Interval' milliseconds and supports
%% a fold-like interface (each result is modified and accumulated)
-spec time_fold(N, Interval, Fun, State, FoldFun, Init) -> [term()] when
    N :: non_neg_integer(),
    Interval :: pos_integer(),
    Fun :: fun((State) -> {term(), State}),
    State :: term(),
    FoldFun :: fun((term(), Init) -> Init),
    Init :: term().
time_fold(0, _, _, _, _, Acc) ->
    Acc;
time_fold(N, Interval, Fun, State, FoldFun, Init) ->
    timer:sleep(Interval),
    {Res, NewState} = Fun(State),
    Acc = FoldFun(Res,Init),
    time_fold(N-1,Interval,Fun,NewState,FoldFun,Acc).

%% @doc Diffs two runs of erlang:statistics(scheduler_wall_time) and
%% returns usage metrics in terms of cores and 0..1 percentages.
-spec scheduler_usage_diff(SchedTime, SchedTime) -> undefined | [{SchedulerId, Usage}] when
    SchedTime :: [{SchedulerId, ActiveTime, TotalTime}],
    SchedulerId :: pos_integer(),
    Usage :: number(),
    ActiveTime :: non_neg_integer(),
    TotalTime :: non_neg_integer().
scheduler_usage_diff(First, Last) when First =:= undefined orelse Last =:= undefined ->
    undefined;
scheduler_usage_diff(First, Last) ->
    lists:map(
        fun ({{I, _A0, T}, {I, _A1, T}}) -> {I, 0.0}; % Avoid divide by zero
            ({{I, A0, T0}, {I, A1, T1}}) -> {I, (A1 - A0)/(T1 - T0)}
        end,
        lists:zip(lists:sort(First), lists:sort(Last))
    ).

%% @doc Returns the top n element of a list of process or inet attributes
-spec sublist_top_n_attrs([Attrs], pos_integer()) -> [Attrs]
    when Attrs :: recon:proc_attrs() | recon:inet_attrs().
sublist_top_n_attrs(_, 0) ->
    %% matching lists:sublist/2 behaviour
    [];
sublist_top_n_attrs(List, Len) ->
    pheap_fill(List, Len, []).

%% @private crush binaries from process_info into their amount of place
%% taken in memory.
binary_memory(Bins) ->
    lists:foldl(fun({_,Mem,_}, Tot) -> Mem+Tot end, 0, Bins).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
pheap_fill(List, 0, Heap) ->
    pheap_full(List, Heap);
pheap_fill([], _, Heap) ->
    pheap_to_list(Heap, []);
pheap_fill([{Y, X, _} = H|T], N, Heap) ->
    pheap_fill(T, N-1, insert({{X, Y}, H}, Heap)).

pheap_full([], Heap) ->
    pheap_to_list(Heap, []);
pheap_full([{Y, X, _} = H|T], [{K, _}|HeapT] = Heap) ->
    case {X, Y} of
        N when N > K ->
            pheap_full(T, insert({N, H}, merge_pairs(HeapT)));
        _ ->
            pheap_full(T, Heap)
    end.

pheap_to_list([], Acc) -> Acc;
pheap_to_list([{_, H}|T], Acc) ->
    pheap_to_list(merge_pairs(T), [H|Acc]).

-compile({inline, [insert/2, merge/2]}).
insert(E, []) -> [E];        %% merge([E], H)
insert(E, [E2|_] = H) when E =< E2 -> [E, H];
insert(E, [E2|H]) -> [E2, [E]|H].

merge(H1, []) -> H1;
merge([E1|H1], [E2|_]=H2) when E1 =< E2 -> [E1, H2|H1];
merge(H1, [E2|H2]) -> [E2, H1|H2].

merge_pairs([]) -> [];
merge_pairs([H]) -> H;
merge_pairs([A, B|T]) -> merge(merge(A, B), merge_pairs(T)).


