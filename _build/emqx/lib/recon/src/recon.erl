%%% @author Fred Hebert <mononcqc@ferd.ca>
%%%  [http://ferd.ca/]
%%% @doc Recon, as a module, provides access to the high-level functionality
%%% contained in the Recon application.
%%%
%%% It has functions in five main categories:
%%%
%%% <dl>
%%%     <dt>1. State information</dt>
%%%     <dd>Process information is everything that has to do with the
%%%         general state of the node. Functions such as {@link info/1}
%%%         and {@link info/3} are wrappers to provide more details than
%%%         `erlang:process_info/1', while providing it in a production-safe
%%%         manner. They have equivalents to `erlang:process_info/2' in
%%%         the functions {@link info/2} and {@link info/4}, respectively.</dd>
%%%     <dd>{@link proc_count/2} and {@link proc_window/3} are to be used
%%%         when you require information about processes in a larger sense:
%%%         biggest consumers of given process information (say memory or
%%%         reductions), either absolutely or over a sliding time window,
%%%         respectively.</dd>
%%%     <dd>{@link bin_leak/1} is a function that can be used to try and
%%%         see if your Erlang node is leaking refc binaries. See the function
%%%         itself for more details.</dd>
%%%     <dd>Functions to access node statistics, in a manner somewhat similar
%%%         to what <a href="https://github.com/ferd/vmstats">vmstats</a>
%%%         provides as a library. There are 3 of them:
%%%         {@link node_stats_print/2}, which displays them,
%%%         {@link node_stats_list/2}, which returns them in a list, and
%%%         {@link node_stats/4}, which provides a fold-like interface
%%%         for stats gathering. For CPU usage specifically, see
%%%         {@link scheduler_usage/1}.</dd>
%%%
%%%     <dt>2. OTP tools</dt>
%%%     <dd>This category provides tools to interact with pieces of OTP
%%%         more easily. At this point, the only function included is
%%%         {@link get_state/1}, which works as a wrapper around
%%%         {@link get_state/2}, which works as a wrapper around
%%%         `sys:get_state/1' in R16B01, and provides the required
%%%         functionality for older versions of Erlang.</dd>
%%%
%%%     <dt>3. Code Handling</dt>
%%%     <dd>Specific functions are in `recon' for the sole purpose
%%%         of interacting with source and compiled code.
%%%         {@link remote_load/1} and {@link remote_load/2} will allow
%%%         to take a local module, and load it remotely (in a diskless
%%%         manner) on another Erlang node you're connected to.</dd>
%%%     <dd>{@link source/1} allows to print the source of a loaded module,
%%%         in case it's not available in the currently running node.</dd>
%%%
%%%     <dt>4. Ports and Sockets</dt>
%%%     <dd>To make it simpler to debug some network-related issues,
%%%         recon contains functions to deal with Erlang ports (raw, file
%%%         handles, or inet). Functions {@link tcp/0}, {@link udp/0},
%%%         {@link sctp/0}, {@link files/0}, and {@link port_types/0} will
%%%         list all the Erlang ports of a given type. The latter function
%%%         prints counts of all individual types.</dd>
%%%     <dd>Port state information can be useful to figure out why certain
%%%         parts of the system misbehave. Functions such as 
%%%         {@link port_info/1} and {@link port_info/2} are wrappers to provide
%%%         more similar or more details than `erlang:port_info/1-2', and, for
%%%         inet ports, statistics and options for each socket.</dd>
%%%     <dd>Finally, the functions {@link inet_count/2} and {@link inet_window/3}
%%%         provide the absolute or sliding window functionality of
%%%         {@link proc_count/2} and {@link proc_count/3} to inet ports
%%%         and connections currently on the node.</dd>
%%%
%%%     <dt>5. RPC</dt>
%%%     <dd>These are wrappers to make RPC work simpler with clusters of
%%%         Erlang nodes. Default RPC mechanisms (from the `rpc' module)
%%%         make it somewhat painful to call shell-defined funs over node
%%%         boundaries. The functions {@link rpc/1}, {@link rpc/2}, and
%%%         {@link rpc/3} will do it with a simpler interface.</dd>
%%%     <dd>Additionally, when you're running diagnostic code on remote
%%%         nodes and want to know which node evaluated what result, using
%%%         {@link named_rpc/1}, {@link named_rpc/2}, and {@link named_rpc/3}
%%%         will wrap the results in a tuple that tells you which node it's
%%%         coming from, making it easier to identify bad nodes.</dd>
%%% </dl>
%%% @end
-module(recon).
-export([info/1, info/2, info/3, info/4,
         proc_count/2, proc_window/3,
         bin_leak/1,
         node_stats_print/2, node_stats_list/2, node_stats/4,
         scheduler_usage/1]).
-export([get_state/1, get_state/2]).
-export([remote_load/1, remote_load/2,
         source/1]).
-export([tcp/0, udp/0, sctp/0, files/0, port_types/0,
         inet_count/2, inet_window/3,
         port_info/1, port_info/2]).
-export([rpc/1, rpc/2, rpc/3,
         named_rpc/1, named_rpc/2, named_rpc/3]).

%%%%%%%%%%%%%
%%% TYPES %%%
%%%%%%%%%%%%%
-type proc_attrs() :: {pid(),
                       Attr::_,
                       [Name::atom()
                       |{current_function, mfa()}
                       |{initial_call, mfa()}, ...]}.

-type inet_attrs() :: {port(),
                       Attr::_,
                       [{atom(), term()}]}.

-type pid_term() :: pid() | atom() | string()
                  | {global, term()} | {via, module(), term()}
                  | {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-type info_type() :: meta | signals | location | memory_used | work.

-type info_meta_key() :: registered_name | dictionary | group_leader | status.
-type info_signals_key() :: links | monitors | monitored_by | trap_exit.
-type info_location_key() :: initial_call | current_stacktrace.
-type info_memory_key() :: memory | message_queue_len | heap_size
                         | total_heap_size | garbage_collection.
-type info_work_key() :: reductions.

-type info_key() :: info_meta_key() | info_signals_key() | info_location_key()
                  | info_memory_key() | info_work_key().

-type port_term() :: port() | string() | atom() | pos_integer().

-type port_info_type() :: meta | signals | io | memory_used | specific.

-type port_info_meta_key() :: registered_name | id | name | os_pid.
-type port_info_signals_key() :: connected | links | monitors.
-type port_info_io_key() :: input | output.
-type port_info_memory_key() :: memory | queue_size.
-type port_info_specific_key() :: atom().

-type port_info_key() :: port_info_meta_key() | port_info_signals_key()
                       | port_info_io_key() | port_info_memory_key()
                       | port_info_specific_key().

-export_type([proc_attrs/0, inet_attrs/0, pid_term/0, port_term/0]).
-export_type([info_type/0, info_key/0,
              info_meta_key/0, info_signals_key/0, info_location_key/0,
              info_memory_key/0, info_work_key/0]).
-export_type([port_info_type/0, port_info_key/0,
              port_info_meta_key/0, port_info_signals_key/0, port_info_io_key/0,
              port_info_memory_key/0, port_info_specific_key/0]).

%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%

%%% Process Info %%%

%% @doc Equivalent to `info(<A.B.C>)' where `A', `B', and `C' are integers part
%% of a pid
-spec info(N,N,N) -> [{info_type(), [{info_key(),term()}]},...] when
      N :: non_neg_integer().
info(A,B,C) -> info(recon_lib:triple_to_pid(A,B,C)).

%% @doc Equivalent to `info(<A.B.C>, Key)' where `A', `B', and `C' are integers part
%% of a pid
-spec info(N,N,N, Key) -> term() when
      N :: non_neg_integer(),
      Key :: info_type() | [atom()] | atom().
info(A,B,C, Key) -> info(recon_lib:triple_to_pid(A,B,C), Key).


%% @doc Allows to be similar to `erlang:process_info/1', but excludes fields
%% such as the mailbox, which have a tendency to grow and be unsafe when called
%% in production systems. Also includes a few more fields than what is usually
%% given (`monitors', `monitored_by', etc.), and separates the fields in a more
%% readable format based on the type of information contained.
%%
%% Moreover, it will fetch and read information on local processes that were
%% registered locally (an atom), globally (`{global, Name}'), or through
%% another registry supported in the `{via, Module, Name}' syntax (must have a
%% `Module:whereis_name/1' function). Pids can also be passed in as a string
%% (`"<0.39.0>"') or a triple (`{0,39,0}') and will be converted to be used.
-spec info(pid_term()) -> [{info_type(), [{info_key(), Value}]},...] when
      Value :: term().
info(PidTerm) ->
    Pid = recon_lib:term_to_pid(PidTerm),
    [info(Pid, Type) || Type <- [meta, signals, location, memory_used, work]].

%% @doc Allows to be similar to `erlang:process_info/2', but allows to
%% sort fields by safe categories and pre-selections, avoiding items such
%% as the mailbox, which may have a tendency to grow and be unsafe when
%% called in production systems.
%%
%% Moreover, it will fetch and read information on local processes that were
%% registered locally (an atom), globally (`{global, Name}'), or through
%% another registry supported in the `{via, Module, Name}' syntax (must have a
%% `Module:whereis_name/1' function). Pids can also be passed in as a string
%% (`"<0.39.0>"') or a triple (`{0,39,0}') and will be converted to be used.
%%
%% Although the type signature doesn't show it in generated documentation,
%% a list of arguments or individual arguments accepted by
%% `erlang:process_info/2' and return them as that function would.
%%
%% A fake attribute `binary_memory' is also available to return the
%% amount of memory used by refc binaries for a process.
-spec info(pid_term(), info_type()) -> {info_type(), [{info_key(), term()}]}
    ;     (pid_term(), [atom()]) -> [{atom(), term()}]
    ;     (pid_term(), atom()) -> {atom(), term()}.
info(PidTerm, meta) ->
    info_type(PidTerm, meta, [registered_name, dictionary, group_leader,
                              status]);
info(PidTerm, signals) ->
    info_type(PidTerm, signals, [links, monitors, monitored_by, trap_exit]);
info(PidTerm, location) ->
    info_type(PidTerm, location, [initial_call, current_stacktrace]);
info(PidTerm, memory_used) ->
    info_type(PidTerm, memory_used, [memory, message_queue_len, heap_size,
                                     total_heap_size, garbage_collection]);
info(PidTerm, work) ->
    info_type(PidTerm, work, [reductions]);
info(PidTerm, Keys) ->
    proc_info(recon_lib:term_to_pid(PidTerm), Keys).

%% @private makes access to `info_type()' calls simpler.
-spec info_type(pid_term(), info_type(), [info_key()]) ->
    {info_type(), [{info_key(), term()}]}.
info_type(PidTerm, Type, Keys) ->
    Pid = recon_lib:term_to_pid(PidTerm),
    {Type, proc_info(Pid, Keys)}.

%% @private wrapper around `erlang:process_info/2' that allows special
%% attribute handling for items like `binary_memory'.
proc_info(Pid, binary_memory) ->
    {binary, Bins} = erlang:process_info(Pid, binary),
    {binary_memory, recon_lib:binary_memory(Bins)};
proc_info(Pid, Term) when is_atom(Term) ->
    erlang:process_info(Pid, Term);
proc_info(Pid, List) when is_list(List) ->
    case lists:member(binary_memory, List) of
        false ->
            erlang:process_info(Pid, List);
        true ->
            Res = erlang:process_info(Pid, replace(binary_memory, binary, List)),
            proc_fake(List, Res)
    end.

%% @private Replace keys around
replace(_, _, []) -> [];
replace(H, Val, [H|T]) -> [Val | replace(H, Val, T)];
replace(R, Val, [H|T]) -> [H | replace(R, Val, T)].

proc_fake([], []) ->
    [];
proc_fake([binary_memory|T1], [{binary,Bins}|T2]) ->
    [{binary_memory, recon_lib:binary_memory(Bins)}
     | proc_fake(T1,T2)];
proc_fake([_|T1], [H|T2]) ->
    [H | proc_fake(T1,T2)].

%% @doc Fetches a given attribute from all processes (except the
%% caller) and returns the biggest `Num' consumers.
-spec proc_count(AttributeName, Num) -> [proc_attrs()] when
      AttributeName :: atom(),
      Num :: non_neg_integer().
proc_count(AttrName, Num) ->
    recon_lib:sublist_top_n_attrs(recon_lib:proc_attrs(AttrName), Num).

%% @doc Fetches a given attribute from all processes (except the
%% caller) and returns the biggest entries, over a sliding time window.
%%
%% This function is particularly useful when processes on the node
%% are mostly short-lived, usually too short to inspect through other
%% tools, in order to figure out what kind of processes are eating
%% through a lot resources on a given node.
%%
%% It is important to see this function as a snapshot over a sliding
%% window. A program's timeline during sampling might look like this:
%%
%%  `--w---- [Sample1] ---x-------------y----- [Sample2] ---z--->'
%%
%% Some processes will live between `w' and die at `x', some between `y' and
%% `z', and some between `x' and `y'. These samples will not be too significant
%% as they're incomplete. If the majority of your processes run between a time
%% interval `x'...`y' (in absolute terms), you should make sure that your
%% sampling time is smaller than this so that for many processes, their
%% lifetime spans the equivalent of `w' and `z'. Not doing this can skew the
%% results: long-lived processes, that have 10 times the time to accumulate
%% data (say reductions) will look like bottlenecks when they're not one.
%%
%% Warning: this function depends on data gathered at two snapshots, and then
%% building a dictionary with entries to differentiate them. This can take a
%% heavy toll on memory when you have many dozens of thousands of processes.
-spec proc_window(AttributeName, Num, Milliseconds) -> [proc_attrs()] when
      AttributeName :: atom(),
      Num :: non_neg_integer(),
      Milliseconds :: pos_integer().
proc_window(AttrName, Num, Time) ->
    Sample = fun() -> recon_lib:proc_attrs(AttrName) end,
    {First,Last} = recon_lib:sample(Time, Sample),
    recon_lib:sublist_top_n_attrs(recon_lib:sliding_window(First, Last), Num).

%% @doc Refc binaries can be leaking when barely-busy processes route them
%% around and do little else, or when extremely busy processes reach a stable
%% amount of memory allocated and do the vast majority of their work with refc
%% binaries. When this happens, it may take a very long while before references
%% get deallocated and refc binaries get to be garbage collected, leading to
%% Out Of Memory crashes.
%% This function fetches the number of refc binary references in each process
%% of the node, garbage collects them, and compares the resulting number of
%% references in each of them. The function then returns the `N' processes
%% that freed the biggest amount of binaries, potentially highlighting leaks.
%% 
%% See <a href="http://www.erlang.org/doc/efficiency_guide/binaryhandling.html#id65722">The efficiency guide</a>
%% for more details on refc binaries
-spec bin_leak(pos_integer()) -> [proc_attrs()].
bin_leak(N) ->
    Procs = recon_lib:sublist_top_n_attrs([
        try
            {ok, {_,Pre,Id}} = recon_lib:proc_attrs(binary, Pid),
            erlang:garbage_collect(Pid),
            {ok, {_,Post,_}} = recon_lib:proc_attrs(binary, Pid),
            {Pid, length(Pre) - length(Post), Id}
        catch
            _:_ -> {Pid, 0, []}
        end || Pid <- processes()
    ], N),
    [{Pid, -Val, Id} ||{Pid, Val, Id} <-Procs].

%% @doc Shorthand for `node_stats(N, Interval, fun(X,_) -> io:format("~p~n",[X]) end, nostate)'.
-spec node_stats_print(Repeat, Interval) -> term() when
      Repeat :: non_neg_integer(),
      Interval :: pos_integer().
node_stats_print(N, Interval) ->
    node_stats(N, Interval, fun(X, _) -> io:format("~p~n", [X]) end, ok).

%% @doc Because Erlang CPU usage as reported from `top' isn't the most
%% reliable value (due to schedulers doing idle spinning to avoid going
%% to sleep and impacting latency), a metric exists that is based on
%% scheduler wall time.
%%
%% For any time interval, Scheduler wall time can be used as a measure
%% of how 'busy' a scheduler is. A scheduler is busy when:
%%
%% <ul>
%%    <li>executing process code</li>
%%    <li>executing driver code</li>
%%    <li>executing NIF code</li>
%%    <li>executing BIFs</li>
%%    <li>garbage collecting</li>
%%    <li>doing memory management</li>
%% </ul>
%%
%% A scheduler isn't busy when doing anything else.
-spec scheduler_usage(Millisecs) -> undefined | [{SchedulerId, Usage}] when
    Millisecs :: non_neg_integer(),
    SchedulerId :: pos_integer(),
    Usage :: number().
scheduler_usage(Interval) when is_integer(Interval) ->
    %% We start and stop the scheduler_wall_time system flag if
    %% it wasn't in place already. Usually setting the flag should
    %% have a CPU impact (making it higher) only when under low usage.
    FormerFlag = erlang:system_flag(scheduler_wall_time, true),
    First = erlang:statistics(scheduler_wall_time),
    timer:sleep(Interval),
    Last = erlang:statistics(scheduler_wall_time),
    erlang:system_flag(scheduler_wall_time, FormerFlag),
    recon_lib:scheduler_usage_diff(First, Last).

%% @doc Shorthand for `node_stats(N, Interval, fun(X,Acc) -> [X|Acc] end, [])'
%% with the results reversed to be in the right temporal order.
-spec node_stats_list(Repeat, Interval) -> [Stats] when
      Repeat :: non_neg_integer(),
      Interval :: pos_integer(),
      Stats :: {[Absolutes::{atom(),term()}],
                [Increments::{atom(),term()}]}.
node_stats_list(N, Interval) ->
    lists:reverse(node_stats(N, Interval, fun(X, Acc) -> [X|Acc] end, [])).

%% @doc Gathers statistics `N' time, waiting `Interval' milliseconds between
%% each run, and accumulates results using a folding function `FoldFun'.
%% The function will gather statistics in two forms: Absolutes and Increments.
%%
%% Absolutes are values that keep changing with time, and are useful to know
%% about as a datapoint: process count, size of the run queue, error_logger
%% queue length in versions before OTP-21 or those thar run it explicitely,
%% and the memory of the node (total, processes, atoms, binaries,
%% and ets tables).
%%
%% Increments are values that are mostly useful when compared to a previous
%% one to have an idea what they're doing, because otherwise they'd never
%% stop increasing: bytes in and out of the node, number of garbage colelctor
%% runs, words of memory that were garbage collected, and the global reductions
%% count for the node.
-spec node_stats(N, Interval, FoldFun, Acc) -> Acc when
      N :: non_neg_integer(),
      Interval :: pos_integer(),
      FoldFun :: fun((Stats, Acc) -> Acc),
      Acc :: term(),
      Stats :: {[Absolutes::{atom(),term()}],
                [Increments::{atom(),term()}]}.
node_stats(N, Interval, FoldFun, Init) ->
    Logger = case whereis(error_logger) of
        undefined -> logger;
        _ -> error_logger
    end,
    %% Turn on scheduler wall time if it wasn't there already
    FormerFlag = erlang:system_flag(scheduler_wall_time, true),
    %% Stats is an ugly fun, but it does its thing.
    Stats = fun({{OldIn,OldOut},{OldGCs,OldWords,_}, SchedWall}) ->
        %% Absolutes
        ProcC = erlang:system_info(process_count),
        RunQ = erlang:statistics(run_queue),
        LogQ = case Logger of
            error_logger ->
                {_,LogQLen} = process_info(whereis(error_logger),
                                           message_queue_len),
                LogQLen;
            _ ->
                undefined
        end,
        %% Mem (Absolutes)
        Mem = erlang:memory(),
        Tot = proplists:get_value(total, Mem),
        ProcM = proplists:get_value(processes_used,Mem),
        Atom = proplists:get_value(atom_used,Mem),
        Bin = proplists:get_value(binary, Mem),
        Ets = proplists:get_value(ets, Mem),
        %% Incremental
        {{input,In},{output,Out}} = erlang:statistics(io),
        GC={GCs,Words,_} = erlang:statistics(garbage_collection),
        BytesIn = In-OldIn,
        BytesOut = Out-OldOut,
        GCCount = GCs-OldGCs,
        GCWords = Words-OldWords,
        {_, Reds} = erlang:statistics(reductions),
        SchedWallNew = erlang:statistics(scheduler_wall_time),
        SchedUsage = recon_lib:scheduler_usage_diff(SchedWall, SchedWallNew),
         %% Stats Results
        {{[{process_count,ProcC}, {run_queue,RunQ}] ++
          [{error_logger_queue_len,LogQ} || LogQ =/= undefined] ++
          [{memory_total,Tot},
           {memory_procs,ProcM}, {memory_atoms,Atom},
           {memory_bin,Bin}, {memory_ets,Ets}],
          [{bytes_in,BytesIn}, {bytes_out,BytesOut},
           {gc_count,GCCount}, {gc_words_reclaimed,GCWords},
           {reductions,Reds}, {scheduler_usage, SchedUsage}]},
         %% New State
         {{In,Out}, GC, SchedWallNew}}
    end,
    {{input,In},{output,Out}} = erlang:statistics(io),
    Gc = erlang:statistics(garbage_collection),
    SchedWall = erlang:statistics(scheduler_wall_time), 
    Result = recon_lib:time_fold(
            N, Interval, Stats,
            {{In,Out}, Gc, SchedWall},
            FoldFun, Init),
    %% Set scheduler wall time back to what it was
    erlang:system_flag(scheduler_wall_time, FormerFlag),
    Result.

%%% OTP & Manipulations %%%


%% @doc Shorthand call to `recon:get_state(PidTerm, 5000)'
-spec get_state(pid_term()) -> term().
get_state(PidTerm) -> get_state(PidTerm, 5000).

%% @doc Fetch the internal state of an OTP process.
%% Calls `sys:get_state/2' directly in R16B01+, and fetches
%% it dynamically on older versions of OTP.
-spec get_state(pid_term(), Ms::non_neg_integer() | 'infinity') -> term().
get_state(PidTerm, Timeout) ->
    Proc = recon_lib:term_to_pid(PidTerm),
    try
        sys:get_state(Proc, Timeout)
    catch
        error:undef ->
            case sys:get_status(Proc, Timeout) of
                {status,_Pid,{module,gen_server},Data} ->
                    {data, Props} = lists:last(lists:nth(5, Data)),
                    proplists:get_value("State", Props);
                {status,_Pod,{module,gen_fsm},Data} ->
                    {data, Props} = lists:last(lists:nth(5, Data)),
                    proplists:get_value("StateData", Props)
            end
    end.

%%% Code & Stuff %%%

%% @equiv remote_load(nodes(), Mod)
-spec remote_load(module()) -> term().
remote_load(Mod) -> remote_load(nodes(), Mod).

%% @doc Loads one or more modules remotely, in a diskless manner.  Allows to
%% share code loaded locally with a remote node that doesn't have it
-spec remote_load(Nodes, module()) -> term() when
      Nodes :: [node(),...] | node().
remote_load(Nodes=[_|_], Mod) when is_atom(Mod) ->
    {Mod, Bin, File} = code:get_object_code(Mod),
    rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]);
remote_load(Nodes=[_|_], Modules) when is_list(Modules) ->
    [remote_load(Nodes, Mod) || Mod <- Modules];
remote_load(Node, Mod) ->
    remote_load([Node], Mod).

%% @doc Obtain the source code of a module compiled with `debug_info'.
%% The returned list sadly does not allow to format the types and typed
%% records the way they look in the original module, but instead goes to
%% an intermediary form used in the AST. They will still be placed
%% in the right module attributes, however.
%% @todo Figure out a way to pretty-print typespecs and records.
-spec source(module()) -> iolist().
source(Module) ->
    Path = code:which(Module),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Path, [abstract_code]),
    erl_prettypr:format(erl_syntax:form_list(AC)).

%%% Ports Info %%%

%% @doc returns a list of all TCP ports (the data type) open on the node.
-spec tcp() -> [port()].
tcp() -> recon_lib:port_list(name, "tcp_inet").

%% @doc returns a list of all UDP ports (the data type) open on the node.
-spec udp() -> [port()].
udp() -> recon_lib:port_list(name, "udp_inet").

%% @doc returns a list of all SCTP ports (the data type) open on the node.
-spec sctp() -> [port()].
sctp() -> recon_lib:port_list(name, "sctp_inet").

%% @doc returns a list of all file handles open on the node.
%% @deprecated Starting with OTP-21, files are implemented as NIFs
%% and can no longer be listed. This function returns an empty list
%% in such a case.
-spec files() -> [port()].
files() -> recon_lib:port_list(name, "efile").

%% @doc Shows a list of all different ports on the node with their respective
%% types.
-spec port_types() -> [{Type::string(), Count::pos_integer()}].
port_types() ->
    lists:usort(
        %% sorts by biggest count, smallest type
        fun({KA,VA}, {KB,VB}) -> {VA,KB} > {VB,KA} end,
        recon_lib:count([Name || {_, Name} <- recon_lib:port_list(name)])
    ).

%% @doc Fetches a given attribute from all inet ports (TCP, UDP, SCTP)
%% and returns the biggest `Num' consumers.
%%
%% The values to be used can be the number of octets (bytes) sent, received,
%% or both (`send_oct', `recv_oct', `oct', respectively), or the number
%% of packets sent, received, or both (`send_cnt', `recv_cnt', `cnt',
%% respectively). Individual absolute values for each metric will be returned
%% in the 3rd position of the resulting tuple.
-spec inet_count(AttributeName, Num) -> [inet_attrs()] when
      AttributeName :: 'recv_cnt' | 'recv_oct' | 'send_cnt' | 'send_oct'
                     | 'cnt' | 'oct',
      Num :: non_neg_integer().
inet_count(Attr, Num) ->
    recon_lib:sublist_top_n_attrs(recon_lib:inet_attrs(Attr), Num).

%% @doc Fetches a given attribute from all inet ports (TCP, UDP, SCTP)
%% and returns the biggest entries, over a sliding time window.
%%
%% Warning: this function depends on data gathered at two snapshots, and then
%% building a dictionary with entries to differentiate them. This can take a
%% heavy toll on memory when you have many dozens of thousands of ports open.
%%
%% The values to be used can be the number of octets (bytes) sent, received,
%% or both (`send_oct', `recv_oct', `oct', respectively), or the number
%% of packets sent, received, or both (`send_cnt', `recv_cnt', `cnt',
%% respectively). Individual absolute values for each metric will be returned
%% in the 3rd position of the resulting tuple.
-spec inet_window(AttributeName, Num, Milliseconds) -> [inet_attrs()] when
      AttributeName :: 'recv_cnt' | 'recv_oct' | 'send_cnt' | 'send_oct'
                     | 'cnt' | 'oct',
      Num :: non_neg_integer(),
      Milliseconds :: pos_integer().
inet_window(Attr, Num, Time) when is_atom(Attr) ->
    Sample = fun() -> recon_lib:inet_attrs(Attr) end,
    {First,Last} = recon_lib:sample(Time, Sample),
    recon_lib:sublist_top_n_attrs(recon_lib:sliding_window(First, Last), Num).

%% @doc Allows to be similar to `erlang:port_info/1', but allows
%% more flexible port usage: usual ports, ports that were registered
%% locally (an atom), ports represented as strings (`"#Port<0.2013>"'),
%% or through an index lookup (`2013', for the same result as
%% `"#Port<0.2013>"').
%%
%% Moreover, the function will try to fetch implementation-specific
%% details based on the port type (only inet ports have this feature
%% so far). For example, TCP ports will include information about the
%% remote peer, transfer statistics, and socket options being used.
%%
%% The information-specific and the basic port info are sorted and 
%% categorized in broader categories ({@link port_info_type()}).
-spec port_info(port_term()) -> [{port_info_type(),
                                  [{port_info_key(), term()}]},...].
port_info(PortTerm) ->
    Port = recon_lib:term_to_port(PortTerm),
    [port_info(Port, Type) || Type <- [meta, signals, io, memory_used,
                                       specific]].

%% @doc Allows to be similar to `erlang:port_info/2', but allows
%% more flexible port usage: usual ports, ports that were registered
%% locally (an atom), ports represented as strings (`"#Port<0.2013>"'),
%% or through an index lookup (`2013', for the same result as
%% `"#Port<0.2013>"').
%%
%% Moreover, the function allows to to fetch information by category
%% as defined in {@link port_info_type()}, and although the type signature
%% doesn't show it in the generated documentation, individual items
%% accepted by `erlang:port_info/2' are accepted, and lists of them too.
-spec port_info(port_term(), port_info_type()) -> {port_info_type(),
                                                   [{port_info_key(), _}]}
    ;          (port_term(), [atom()]) -> [{atom(), term()}]
    ;          (port_term(), atom()) -> {atom(), term()}.
port_info(PortTerm, meta) ->
    {meta, List} = port_info_type(PortTerm, meta, [id, name, os_pid]),
    case port_info(PortTerm, registered_name) of
        [] -> {meta, List};
        Name -> {meta, [Name | List]}
    end;
port_info(PortTerm, signals) ->
    port_info_type(PortTerm, signals, [connected, links, monitors]);
port_info(PortTerm, io) ->
    port_info_type(PortTerm, io, [input, output]);
port_info(PortTerm, memory_used) ->
    port_info_type(PortTerm, memory_used, [memory, queue_size]);
port_info(PortTerm, specific) ->
    Port = recon_lib:term_to_port(PortTerm),
    Props = case erlang:port_info(Port, name) of
        {_,Type} when Type =:= "udp_inet";
                      Type =:= "tcp_inet";
                      Type =:= "sctp_inet" ->
            case inet:getstat(Port) of
                {ok, Stats} -> [{statistics, Stats}];
                _ -> []
            end ++
            case inet:peername(Port) of
                {ok, Peer} -> [{peername, Peer}];
                {error, _} ->  []
            end ++
            case inet:sockname(Port) of
                {ok, Local} -> [{sockname, Local}];
                {error, _} -> []
            end ++
            case inet:getopts(Port, [active, broadcast, buffer, delay_send,
                                     dontroute, exit_on_close, header,
                                     high_watermark, ipv6_v6only, keepalive,
                                     linger, low_watermark, mode, nodelay,
                                     packet, packet_size, priority,
                                     read_packets, recbuf, reuseaddr,
                                     send_timeout, sndbuf]) of
                {ok, Opts} -> [{options, Opts}];
                {error, _} -> []
            end;
        {_,"efile"} ->
            %% would be nice to support file-specific info, but things
            %% are too vague with the file_server and how it works in
            %% order to make this work efficiently
            [];
        _ ->
            []
    end,
    {type, Props};
port_info(PortTerm, Keys) when is_list(Keys) ->
    Port = recon_lib:term_to_port(PortTerm),
    [erlang:port_info(Port,Key) || Key <- Keys];
port_info(PortTerm, Key) when is_atom(Key) ->
    erlang:port_info(recon_lib:term_to_port(PortTerm), Key).

%% @private makes access to `port_info_type()' calls simpler.
%-spec port_info_type(pid_term(), port_info_type(), [port_info_key()]) ->
%    {port_info_type(), [{port_info_key(), term()}]}.
port_info_type(PortTerm, Type, Keys) ->
    Port = recon_lib:term_to_port(PortTerm),
    {Type, [erlang:port_info(Port,Key) || Key <- Keys]}.


%%% RPC Utils %%%

%% @doc Shorthand for `rpc([node()|nodes()], Fun)'.
-spec rpc(fun(() -> term())) -> {[Success::_],[Fail::_]}.
rpc(Fun) ->
    rpc([node()|nodes()], Fun).

%% @doc Shorthand for `rpc(Nodes, Fun, infinity)'.
-spec rpc(node()|[node(),...], fun(() -> term())) -> {[Success::_],[Fail::_]}.
rpc(Nodes, Fun) ->
    rpc(Nodes, Fun, infinity).

%% @doc Runs an arbitrary fun (of arity 0) over one or more nodes.
-spec rpc(node()|[node(),...], fun(() -> term()), timeout()) -> {[Success::_],[Fail::_]}.
rpc(Nodes=[_|_], Fun, Timeout) when is_function(Fun,0) ->
    rpc:multicall(Nodes, erlang, apply, [Fun,[]], Timeout);
rpc(Node, Fun, Timeout) when is_atom(Node) ->
    rpc([Node], Fun, Timeout).

%% @doc Shorthand for `named_rpc([node()|nodes()], Fun)'.
-spec named_rpc(fun(() -> term())) -> {[Success::_],[Fail::_]}.
named_rpc(Fun) ->
    named_rpc([node()|nodes()], Fun).

%% @doc Shorthand for `named_rpc(Nodes, Fun, infinity)'.
-spec named_rpc(node()|[node(),...], fun(() -> term())) -> {[Success::_],[Fail::_]}.
named_rpc(Nodes, Fun) ->
    named_rpc(Nodes, Fun, infinity).

%% @doc Runs an arbitrary fun (of arity 0) over one or more nodes, and returns the
%% name of the node that computed a given result along with it, in a tuple.
-spec named_rpc(node()|[node(),...], fun(() -> term()), timeout()) -> {[Success::_],[Fail::_]}.
named_rpc(Nodes=[_|_], Fun, Timeout) when is_function(Fun,0) ->
    rpc:multicall(Nodes, erlang, apply, [fun() -> {node(),Fun()} end,[]], Timeout);
named_rpc(Node, Fun, Timeout) when is_atom(Node) ->
    named_rpc([Node], Fun, Timeout).

