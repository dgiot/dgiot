%%% @author Fred Hebert <mononcqc@ferd.ca>
%%%  [http://ferd.ca/]
%%% @doc
%%% `recon_trace' is a module that handles tracing in a safe manner for single
%%% Erlang nodes, currently for function calls only. Functionality includes:
%%%
%%% <ul>
%%%     <li>Nicer to use interface (arguably) than `dbg' or trace BIFs.</li>
%%%     <li>Protection against dumb decisions (matching all calls on a node
%%%         being traced, for example)</li>
%%%     <li>Adding safe guards in terms of absolute trace count or
%%%         rate-limitting</li>
%%%     <li>Nicer formatting than default traces</li>
%%% </ul>
%%%
%%% == Tracing Erlang Code ==
%%%
%%% The Erlang Trace BIFs allow to trace any Erlang code at all. They work in
%%% two parts: pid specifications, and trace patterns.
%%%
%%% Pid specifications let you decide which processes to target. They can be
%%% specific pids, `all' pids, `existing' pids, or `new' pids (those not
%%% spawned at the time of the function call).
%%%
%%% The trace patterns represent functions. Functions can be specified in two
%%% parts: specifying the modules, functions, and arguments, and then with
%%% Erlang match specifications to add constraints to arguments (see
%%% {@link calls/3} for details).
%%%
%%% What defines whether you get traced or not is the intersection of both:
%%%
%%% ```
%%%         _,--------,_      _,--------,_
%%%      ,-'            `-,,-'            `-,
%%%   ,-'              ,-'  '-,              `-,
%%%  |   Matching    -'        '-   Matching    |
%%%  |     Pids     |  Getting   |    Trace     |
%%%  |              |   Traced   |  Patterns    |
%%%  |               -,        ,-               |
%%%   '-,              '-,  ,-'              ,-'
%%%      '-,_          _,-''-,_          _,-'
%%%          '--------'        '--------'
%%% '''
%%%
%%% If either the pid specification excludes a process or a trace pattern
%%% excludes a given call, no trace will be received.
%%%
%%% == Example Session ==
%%%
%%% First let's trace the `queue:new' functions in any process:
%%%
%%% ```
%%% 1> recon_trace:calls({queue, new, '_'}, 1).
%%% 1
%%% 13:14:34.086078 <0.44.0> queue:new()
%%% Recon tracer rate limit tripped.
%%% '''
%%%
%%% The limit was set to `1' trace message at most, and `recon' let us
%%% know when that limit was reached.
%%%
%%% Let's instead look for all the `queue:in/2' calls, to see what it is
%%% we're inserting in queues:
%%%
%%% ```
%%% 2> recon_trace:calls({queue, in, 2}, 1).
%%% 1
%%% 13:14:55.365157 <0.44.0> queue:in(a, {[],[]})
%%% Recon tracer rate limit tripped.
%%% '''
%%%
%%% In order to see the content we want, we should change the trace patterns
%%% to use a `fun' that matches on all arguments in a list (`_') and returns
%%% `return_trace()'. This last part will generate a second trace for each
%%% call that includes the return value:
%%%
%%% ```
%%% 3> recon_trace:calls({queue, in, fun(_) -> return_trace() end}, 3).
%%% 1
%%%
%%% 13:15:27.655132 <0.44.0> queue:in(a, {[],[]})
%%%
%%% 13:15:27.655467 <0.44.0> queue:in/2 --> {[a],[]}
%%%
%%% 13:15:27.757921 <0.44.0> queue:in(a, {[],[]})
%%% Recon tracer rate limit tripped.
%%% '''
%%%
%%% Matching on argument lists can be done in a more complex manner:
%%%
%%% ```
%%% 4> recon_trace:calls(
%%% 4>   {queue, '_', fun([A,_]) when is_list(A); is_integer(A) andalso A > 1 -> return_trace() end},
%%% 4>   {10,100}
%%% 4> ).
%%% 32
%%%
%%% 13:24:21.324309 <0.38.0> queue:in(3, {[],[]})
%%%
%%% 13:24:21.371473 <0.38.0> queue:in/2 --> {[3],[]}
%%%
%%% 13:25:14.694865 <0.53.0> queue:split(4, {[10,9,8,7],[1,2,3,4,5,6]})
%%%
%%% 13:25:14.695194 <0.53.0> queue:split/2 --> {{[4,3,2],[1]},{[10,9,8,7],[5,6]}}
%%%
%%% 5> recon_trace:clear().
%%% ok
%%% '''
%%%
%%% Note that in the pattern above, no specific function (<code>'_'</code>) was
%%% matched against. Instead, the `fun' used restricted functions to those
%%% having two arguments, the first of which is either a list or an integer
%%% greater than `1'.
%%%
%%% The limit was also set using `{10,100}' instead of an integer, making the
%%% rate-limitting at 10 messages per 100 milliseconds, instead of an absolute
%%% value.
%%%
%%% Any tracing can be manually interrupted by calling `recon_trace:clear()',
%%% or killing the shell process.
%%%
%%% Be aware that extremely broad patterns with lax rate-limitting (or very
%%% high absolute limits) may impact your node's stability in ways
%%% `recon_trace' cannot easily help you with.
%%%
%%% In doubt, start with the most restrictive tracing possible, with low
%%% limits, and progressively increase your scope.
%%%
%%% See {@link calls/3} for more details and tracing possibilities.
%%%
%%% == Structure ==
%%%
%%% This library is production-safe due to taking the following structure for
%%% tracing:
%%%
%%% ```
%%% [IO/Group leader] <---------------------,
%%%   |                                     |
%%% [shell] ---> [tracer process] ----> [formatter]
%%% '''
%%%
%%% The tracer process receives trace messages from the node, and enforces
%%% limits in absolute terms or trace rates, before forwarding the messages
%%% to the formatter. This is done so the tracer can do as little work as
%%% possible and never block while building up a large mailbox.
%%%
%%% The tracer process is linked to the shell, and the formatter to the
%%% tracer process. The formatter also traps exits to be able to handle
%%% all received trace messages until the tracer termination, but will then
%%% shut down as soon as possible.
%%%
%%% In case the operator is tracing from a remote shell which gets
%%% disconnected, the links between the shell and the tracer should make it
%%% so tracing is automatically turned off once you disconnect.
%%%
%%% If sending output to the Group Leader is not desired, you may specify
%%% a different pid() via the option `io_server' in the {@link calls/3} function.
%%% For instance to write the traces to a file you can do something like
%%%
%%% ```
%%% 1> {ok, Dev} = file:open("/tmp/trace",[write]).
%%% 2> recon_trace:calls({queue, in, fun(_) -> return_trace() end}, 3, [{io_server, Dev}]).
%%% 1
%%% 3>
%%% Recon tracer rate limit tripped.
%%% 4> file:close(Dev).
%%% '''
%%%
%%% The only output still sent to the Group Leader is the rate limit being
%%% tripped, and any errors. The rest will be sent to the other IO
%%% server (see [http://erlang.org/doc/apps/stdlib/io_protocol.html]).
%%%
%%% == Record Printing ==
%%%
%%% Thanks to code contributed by Bartek Górny, record printing can be added
%%% to traces by first importing records in an active session with
%%% `recon_rec:import([Module, ...])', after which the records declared in
%%% the module list will be supported.
%%% @end
-module(recon_trace).

%% API
-export([clear/0, calls/2, calls/3]).

-export([format/1]).

%% Internal exports
-export([count_tracer/1, rate_tracer/2, formatter/5, format_trace_output/1, format_trace_output/2]).

-type matchspec()    :: [{[term()], [term()], [term()]}].
-type shellfun()     :: fun((_) -> term()).
-type formatterfun() :: fun((_) -> iodata()).
-type millisecs()    :: non_neg_integer().
-type pidspec()      :: all | existing | new | recon:pid_term().
-type max_traces()   :: non_neg_integer().
-type max_rate()     :: {max_traces(), millisecs()}.

                   %% trace options
-type options()      :: [ {pid, pidspec() | [pidspec(),...]} % default: all
                        | {timestamp, formatter | trace}     % default: formatter
                        | {args, args | arity}               % default: args
                        | {io_server, pid()}                 % default: group_leader()
                        | {formatter, formatterfun()}        % default: internal formatter
                        | return_to | {return_to, boolean()} % default: false
                   %% match pattern options
                        | {scope, global | local}            % default: global
                        ].

-type mod()          :: '_' | module().
-type fn()           :: '_' | atom().
-type args()         :: '_' | 0..255 | return_trace | matchspec() | shellfun().
-type tspec()        :: {mod(), fn(), args()}.
-type max()          :: max_traces() | max_rate().
-type num_matches()  :: non_neg_integer().

-export_type([mod/0, fn/0, args/0, tspec/0, num_matches/0, options/0,
              max_traces/0, max_rate/0]).

%%%%%%%%%%%%%%
%%% PUBLIC %%%
%%%%%%%%%%%%%%

%% @doc Stops all tracing at once.
-spec clear() -> ok.
clear() ->
    erlang:trace(all, false, [all]),
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({'_','_','_'}, false, []), % unsets global
    maybe_kill(recon_trace_tracer),
    maybe_kill(recon_trace_formatter),
    ok.

%% @equiv calls({Mod, Fun, Args}, Max, [])
-spec calls(tspec() | [tspec(),...], max()) -> num_matches().
calls({Mod, Fun, Args}, Max) ->
    calls([{Mod,Fun,Args}], Max, []);
calls(TSpecs = [_|_], Max) ->
    calls(TSpecs, Max, []).

%% @doc Allows to set trace patterns and pid specifications to trace
%% function calls.
%%
%% The basic calls take the trace patterns as tuples of the form
%% `{Module, Function, Args}' where:
%%
%% <ul>
%%  <li>`Module' is any atom representing a module</li>
%%  <li>`Function' is any atom representing a function, or the wildcard
%%      <code>'_'</code></li>
%%  <li>`Args' is either the arity of a function (`0..255'), a wildcard
%%      pattern (<code>'_'</code>), a
%%      <a href="http://learnyousomeerlang.com/ets#you-have-been-selected">match specification</a>,
%%      or a function from a shell session that can be transformed into
%%      a match specification</li>
%% </ul>
%%
%% There is also an argument specifying either a maximal count (a number)
%% of trace messages to be received, or a maximal frequency (`{Num, Millisecs}').
%%
%% Here are examples of things to trace:
%%
%% <ul>
%%  <li>All calls from the `queue' module, with 10 calls printed at most:
%%      ``recon_trace:calls({queue, '_', '_'}, 10)''</li>
%%  <li>All calls to `lists:seq(A,B)', with 100 calls printed at most:
%%      `recon_trace:calls({lists, seq, 2}, 100)'</li>
%%  <li>All calls to `lists:seq(A,B)', with 100 calls per second at most:
%%      `recon_trace:calls({lists, seq, 2}, {100, 1000})'</li>
%%  <li>All calls to `lists:seq(A,B,2)' (all sequences increasing by two)
%%      with 100 calls at most:
%%      `recon_trace:calls({lists, seq, fun([_,_,2]) -> ok end}, 100)'</li>
%%  <li>All calls to `iolist_to_binary/1' made with a binary as an argument
%%      already (kind of useless conversion!):
%%      `recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10)'</li>
%%  <li>Calls to the queue module only in a given process `Pid', at a rate
%%      of 50 per second at most:
%%      ``recon_trace:calls({queue, '_', '_'}, {50,1000}, [{pid, Pid}])''</li>
%%  <li>Print the traces with the function arity instead of literal arguments:
%%      `recon_trace:calls(TSpec, Max, [{args, arity}])'</li>
%%  <li>Matching the `filter/2' functions of both `dict' and `lists' modules,
%%      across new processes only:
%%      `recon_trace:calls([{dict,filter,2},{lists,filter,2}], 10, [{pid, new}])'</li>
%%  <li>Tracing the `handle_call/3' functions of a given module for all new processes,
%%      and those of an existing one registered with `gproc':
%%      `recon_trace:calls({Mod,handle_call,3}, {10,100}, [{pid, [{via, gproc, Name}, new]}'</li>
%%  <li>Show the result of a given function call:
%%      `recon_trace:calls({Mod,Fun,fun(_) -> return_trace() end}, Max, Opts)'
%%      or
%%      ``recon_trace:calls({Mod,Fun,[{'_', [], [{return_trace}]}]}, Max, Opts)'',
%%      the important bit being the `return_trace()' call or the
%%      `{return_trace}' match spec value.
%%      A short-hand version for this pattern of 'match anything, trace everything'
%%      for a function is `recon_trace:calls({Mod, Fun, return_trace})'. </li>
%% </ul>
%%
%% There's a few more combination possible, with multiple trace patterns per call, and more
%% options:
%%
%% <ul>
%%  <li>`{pid, PidSpec}': which processes to trace. Valid options is any of
%%      `all', `new', `existing', or a process descriptor (`{A,B,C}',
%%      `"<A.B.C>"', an atom representing a name, `{global, Name}',
%%      `{via, Registrar, Name}', or a pid). It's also possible to specify
%%      more than one by putting them in a list.</li>
%%  <li>`{timestamp, formatter | trace}': by default, the formatter process
%%      adds timestamps to messages received. If accurate timestamps are
%%      required, it's possible to force the usage of timestamps within
%%      trace messages by adding the option `{timestamp, trace}'.</li>
%%  <li>`{args, arity | args}': whether to print arity in function calls
%%      or their (by default) literal representation.</li>
%%  <li>`{scope, global | local}': by default, only 'global' (fully qualified
%%      function calls) are traced, not calls made internally. To force tracing
%%      of local calls, pass in `{scope, local}'. This is useful whenever
%%      you want to track the changes of code in a process that isn't called
%%      with `Module:Fun(Args)', but just `Fun(Args)'.</li>
%%  <li>`{formatter, fun(Term) -> io_data() end}': override the default
%%       formatting functionality provided by recon.</li>
%%  <li>`{io_server, pid() | atom()}': by default, recon logs to the current
%%      group leader, usually the shell. This option allows to redirect
%%      trace output to a different IO server (such as a file handle).</li>
%%  <li>`return_to': If this option is set (in conjunction with the match
%%      option `{scope, local}'), the function to which the value is returned
%%      is output in a trace. Note that this is distinct from giving the
%%      *caller* since exception handling or calls in tail position may
%%      hide the original caller.</li>
%% </ul>
%%
%% Also note that putting extremely large `Max' values (i.e. `99999999' or
%% `{10000,1}') will probably negate most of the safe-guarding this library
%% does and be dangerous to your node. Similarly, tracing extremely large
%% amounts of function calls (all of them, or all of `io' for example)
%% can be risky if more trace messages are generated than any process on
%% the node could ever handle, despite the precautions taken by this library.
%% @end
-spec calls(tspec() | [tspec(),...], max(), options()) -> num_matches().

calls({Mod, Fun, Args}, Max, Opts) ->
    calls([{Mod,Fun,Args}], Max, Opts);
calls(TSpecs = [_|_], {Max, Time}, Opts) ->
    Pid = setup(rate_tracer, [Max, Time],
                validate_formatter(Opts), validate_io_server(Opts)),
    trace_calls(TSpecs, Pid, Opts);
calls(TSpecs = [_|_], Max, Opts) ->
    Pid = setup(count_tracer, [Max],
                validate_formatter(Opts), validate_io_server(Opts)),
    trace_calls(TSpecs, Pid, Opts).

%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE EXPORTS %%%
%%%%%%%%%%%%%%%%%%%%%%%
%% @private Stops when N trace messages have been received
count_tracer(0) ->
    exit(normal);
count_tracer(N) ->
    receive
        Msg ->
            recon_trace_formatter ! Msg,
            count_tracer(N-1)
    end.

%% @private Stops whenever the trace message rates goes higher than
%% `Max' messages in `Time' milliseconds. Note that if the rate
%% proposed is higher than what the IO system of the formatter
%% can handle, this can still put a node at risk.
%%
%% It is recommended to try stricter rates to begin with.
rate_tracer(Max, Time) -> rate_tracer(Max, Time, 0, os:timestamp()).

rate_tracer(Max, Time, Count, Start) ->
    receive
        Msg ->
            recon_trace_formatter ! Msg,
            Now = os:timestamp(),
            Delay = timer:now_diff(Now, Start) div 1000,
            if Delay > Time -> rate_tracer(Max, Time, 0, Now)
             ; Max > Count -> rate_tracer(Max, Time, Count+1, Start)
             ; Max =:= Count -> exit(normal)
            end
    end.

%% @private Formats traces to be output
formatter(Tracer, Parent, Ref, FormatterFun, IOServer) ->
    process_flag(trap_exit, true),
    link(Tracer),
    Parent ! {Ref, linked},
    formatter(Tracer, IOServer, FormatterFun).

formatter(Tracer, IOServer, FormatterFun) ->
    receive
        {'EXIT', Tracer, normal} ->
            io:format("Recon tracer rate limit tripped.~n"),
            exit(normal);
        {'EXIT', Tracer, Reason} ->
            exit(Reason);
        TraceMsg ->
            io:format(IOServer, FormatterFun(TraceMsg), []),
            formatter(Tracer, IOServer, FormatterFun)
    end.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% starts the tracer and formatter processes, and
%% cleans them up before each call.
setup(TracerFun, TracerArgs, FormatterFun, IOServer) ->
    clear(),
    Ref = make_ref(),
    Tracer = spawn_link(?MODULE, TracerFun, TracerArgs),
    register(recon_trace_tracer, Tracer),
    Format = spawn(?MODULE, formatter, [Tracer, self(), Ref, FormatterFun, IOServer]),
    register(recon_trace_formatter, Format),
    receive
        {Ref, linked} -> Tracer
    after 5000 ->
        error(setup_failed)
    end.

%% Sets the traces in action
trace_calls(TSpecs, Pid, Opts) ->
    {PidSpecs, TraceOpts, MatchOpts} = validate_opts(Opts),
    Matches = [begin
                {Arity, Spec} = validate_tspec(Mod, Fun, Args),
                erlang:trace_pattern({Mod, Fun, Arity}, Spec, MatchOpts)
               end || {Mod, Fun, Args} <- TSpecs],
    [erlang:trace(PidSpec, true, [call, {tracer, Pid} | TraceOpts])
     || PidSpec <- PidSpecs],
    lists:sum(Matches).


%%%%%%%%%%%%%%%%%%
%%% VALIDATION %%%
%%%%%%%%%%%%%%%%%%

validate_opts(Opts) ->
    PidSpecs = validate_pid_specs(proplists:get_value(pid, Opts, all)),
    Scope = proplists:get_value(scope, Opts, global),
    TraceOpts = case proplists:get_value(timestamp, Opts, formatter) of
                    formatter -> [];
                    trace -> [timestamp]
                 end ++
                 case proplists:get_value(args, Opts, args) of
                    args -> [];
                    arity -> [arity]
                 end ++
                 case proplists:get_value(return_to, Opts, undefined) of
                     true when Scope =:= local ->
                         [return_to];
                     true when Scope =:= global ->
                         io:format("Option return_to only works with option {scope, local}~n"),
                         %% Set it anyway
                         [return_to];
                     _ ->
                         []
                 end,
    MatchOpts = [Scope],
    {PidSpecs, TraceOpts, MatchOpts}.

%% Support the regular specs, but also allow `recon:pid_term()' and lists
%% of further pid specs.
-spec validate_pid_specs(pidspec() | [pidspec(),...]) ->
    [all | new | existing | pid(), ...].
validate_pid_specs(all) -> [all];
validate_pid_specs(existing) -> [existing];
validate_pid_specs(new) -> [new];
validate_pid_specs([Spec]) -> validate_pid_specs(Spec);
validate_pid_specs(PidTerm = [Spec|Rest]) ->
    %% can be "<a.b.c>" or [pidspec()]
    try
        [recon_lib:term_to_pid(PidTerm)]
    catch
        error:function_clause ->
            validate_pid_specs(Spec) ++ validate_pid_specs(Rest)
    end;
validate_pid_specs(PidTerm) ->
    %% has to be `recon:pid_term()'.
    [recon_lib:term_to_pid(PidTerm)].

validate_tspec(Mod, Fun, Args) when is_function(Args) ->
    validate_tspec(Mod, Fun, fun_to_ms(Args));
%% helper to save typing for common actions
validate_tspec(Mod, Fun, return_trace) ->
    validate_tspec(Mod, Fun, [{'_', [], [{return_trace}]}]);
validate_tspec(Mod, Fun, Args) ->
    BannedMods = ['_', ?MODULE, io, lists],
    %% The banned mod check can be bypassed by using
    %% match specs if you really feel like being dumb.
    case {lists:member(Mod, BannedMods), Args} of
        {true, '_'} -> error({dangerous_combo, {Mod,Fun,Args}});
        {true, []} -> error({dangerous_combo, {Mod,Fun,Args}});
        _ -> ok
    end,
    case Args of
        '_' -> {'_', true};
        _ when is_list(Args) -> {'_', Args};
        _ when Args >= 0, Args =< 255 -> {Args, true}
    end.

validate_formatter(Opts) ->
    case proplists:get_value(formatter, Opts) of
        F when is_function(F, 1) -> F;
        _ -> fun format/1
    end.

validate_io_server(Opts) ->
    proplists:get_value(io_server, Opts, group_leader()).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% TRACE FORMATTING %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% Thanks Geoff Cant for the foundations for this.
format(TraceMsg) ->
    {Type, Pid, {Hour,Min,Sec}, TraceInfo} = extract_info(TraceMsg),
    {FormatStr, FormatArgs} = case {Type, TraceInfo} of
        %% {trace, Pid, 'receive', Msg}
        {'receive', [Msg]} ->
            {"< ~p", [Msg]};
        %% {trace, Pid, send, Msg, To}
        {send, [Msg, To]} ->
            {" > ~p: ~p", [To, Msg]};
        %% {trace, Pid, send_to_non_existing_process, Msg, To}
        {send_to_non_existing_process, [Msg, To]} ->
            {" > (non_existent) ~p: ~p", [To, Msg]};
        %% {trace, Pid, call, {M, F, Args}}
        {call, [{M,F,Args}]} ->
            {"~p:~p~s", [M,F,format_args(Args)]};
        %% {trace, Pid, return_to, {M, F, Arity}}
        {return_to, [{M,F,Arity}]} ->
            {" '--> ~p:~p/~p", [M,F,Arity]};
        %% {trace, Pid, return_from, {M, F, Arity}, ReturnValue}
        {return_from, [{M,F,Arity}, Return]} ->
            {"~p:~p/~p --> ~s", [M,F,Arity, format_trace_output(Return)]};
        %% {trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}
        {exception_from, [{M,F,Arity}, {Class,Val}]} ->
            {"~p:~p/~p ~p ~p", [M,F,Arity, Class, Val]};
        %% {trace, Pid, spawn, Spawned, {M, F, Args}}
        {spawn, [Spawned, {M,F,Args}]}  ->
            {"spawned ~p as ~p:~p~s", [Spawned, M, F, format_args(Args)]};
        %% {trace, Pid, exit, Reason}
        {exit, [Reason]} ->
            {"EXIT ~p", [Reason]};
        %% {trace, Pid, link, Pid2}
        {link, [Linked]} ->
            {"link(~p)", [Linked]};
        %% {trace, Pid, unlink, Pid2}
        {unlink, [Linked]} ->
            {"unlink(~p)", [Linked]};
        %% {trace, Pid, getting_linked, Pid2}
        {getting_linked, [Linker]} ->
            {"getting linked by ~p", [Linker]};
        %% {trace, Pid, getting_unlinked, Pid2}
        {getting_unlinked, [Unlinker]} ->
            {"getting unlinked by ~p", [Unlinker]};
        %% {trace, Pid, register, RegName}
        {register, [Name]} ->
            {"registered as ~p", [Name]};
        %% {trace, Pid, unregister, RegName}
        {unregister, [Name]} ->
            {"no longer registered as ~p", [Name]};
        %% {trace, Pid, in, {M, F, Arity} | 0}
        {in, [{M,F,Arity}]} ->
            {"scheduled in for ~p:~p/~p", [M,F,Arity]};
        {in, [0]} ->
            {"scheduled in", []};
        %% {trace, Pid, out, {M, F, Arity} | 0}
        {out, [{M,F,Arity}]} ->
            {"scheduled out from ~p:~p/~p", [M, F, Arity]};
        {out, [0]} ->
            {"scheduled out", []};
        %% {trace, Pid, gc_start, Info}
        {gc_start, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"gc beginning -- heap ~p bytes",
             [HeapSize + OldHeapSize + MbufSize]};
        %% {trace, Pid, gc_end, Info}
        {gc_end, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"gc finished -- heap ~p bytes",
             [HeapSize + OldHeapSize + MbufSize]};
        _ ->
            {"unknown trace type ~p -- ~p", [Type, TraceInfo]}
    end,
    io_lib:format("~n~p:~p:~9.6.0f ~p " ++ FormatStr ++ "~n",
                  [Hour, Min, Sec, Pid] ++ FormatArgs).

extract_info(TraceMsg) ->
    case tuple_to_list(TraceMsg) of
        [trace_ts, Pid, Type | Info] ->
            {TraceInfo, [Timestamp]} = lists:split(length(Info)-1, Info),
            {Type, Pid, to_hms(Timestamp), TraceInfo};
        [trace, Pid, Type | TraceInfo] ->
            {Type, Pid, to_hms(os:timestamp()), TraceInfo}
    end.

to_hms(Stamp = {_, _, Micro}) ->
    {_,{H, M, Secs}} = calendar:now_to_local_time(Stamp),
    Seconds = Secs rem 60 + (Micro / 1000000),
    {H,M,Seconds};
to_hms(_) ->
    {0,0,0}.

format_args(Arity) when is_integer(Arity) ->
    [$/, integer_to_list(Arity)];
format_args(Args) when is_list(Args) ->
    [$(, join(", ", [format_trace_output(Arg) || Arg <- Args]), $)].


%% @doc formats call arguments and return values - most types are just printed out, except for
%% tuples recognised as records, which mimic the source code syntax
%% @end
format_trace_output(Args) ->
    format_trace_output(recon_rec:is_active(), recon_map:is_active(), Args).

format_trace_output(Recs, Args) ->
    format_trace_output(Recs, recon_map:is_active(), Args).

format_trace_output(true, _, Args) when is_tuple(Args) ->
    recon_rec:format_tuple(Args);
format_trace_output(false, true, Args) when is_tuple(Args) ->
    format_tuple(false, true, Args);
format_trace_output(Recs, Maps, Args) when is_list(Args), Recs orelse Maps ->
    case io_lib:printable_list(Args) of
        true ->
            io_lib:format("~p", [Args]);
        false ->
            format_maybe_improper_list(Recs, Maps, Args)
    end;
format_trace_output(Recs, true, Args) when is_map(Args) ->
    {Label, Map} = case recon_map:process_map(Args) of
                       {L, M} -> {atom_to_list(L), M};
                       M -> {"", M}
                   end,
    ItemList = maps:to_list(Map),
    [Label,
     "#{",
        join(", ", [format_kv(Recs, true, Key, Val) || {Key, Val} <- ItemList]),
    "}"];
format_trace_output(Recs, false, Args) when is_map(Args) ->
    ItemList = maps:to_list(Args),
    ["#{",
        join(", ", [format_kv(Recs, false, Key, Val) || {Key, Val} <- ItemList]),
    "}"];
format_trace_output(_, _, Args) ->
    io_lib:format("~p", [Args]).

format_kv(Recs, Maps, Key, Val) ->
    [format_trace_output(Recs, Maps, Key), "=>", format_trace_output(Recs, Maps, Val)].


format_tuple(Recs, Maps, Tup) ->
    [${ | format_tuple_(Recs, Maps, tuple_to_list(Tup))].

format_tuple_(_Recs, _Maps, []) ->
    "}";
format_tuple_(Recs, Maps, [H|T]) ->
    [format_trace_output(Recs, Maps, H), $,,
     format_tuple_(Recs, Maps, T)].


format_maybe_improper_list(Recs, Maps, List) ->
    [$[ | format_maybe_improper_list_(Recs, Maps, List)].

format_maybe_improper_list_(_, _, []) ->
    "]";
format_maybe_improper_list_(Recs, Maps, [H|[]]) ->
    [format_trace_output(Recs, Maps, H), $]];
format_maybe_improper_list_(Recs, Maps, [H|T]) when is_list(T) ->
    [format_trace_output(Recs, Maps, H), $,,
     format_maybe_improper_list_(Recs, Maps, T)];
format_maybe_improper_list_(Recs, Maps, [H|T]) when not is_list(T) ->
    %% Handling improper lists
    [format_trace_output(Recs, Maps, H), $|,
     format_trace_output(Recs, Maps, T), $]].


%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

maybe_kill(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            exit(Pid, kill),
            wait_for_death(Pid, Name)
    end.

wait_for_death(Pid, Name) ->
    case is_process_alive(Pid) orelse whereis(Name) =:= Pid of
        true ->
            timer:sleep(10),
            wait_for_death(Pid, Name);
        false ->
            ok
    end.

%% Borrowed from dbg
fun_to_ms(ShellFun) when is_function(ShellFun) ->
    case erl_eval:fun_data(ShellFun) of
        {fun_data,ImportList,Clauses} ->
            case ms_transform:transform_from_shell(
                   dbg,Clauses,ImportList) of
                {error,[{_,[{_,_,Code}|_]}|_],_} ->
                    io:format("Error: ~s~n",
                              [ms_transform:format_error(Code)]),
                    {error,transform_error};
                Else ->
                    Else
            end;
        false ->
            exit(shell_funs_only)
    end.


-ifdef(OTP_RELEASE).
-spec join(term(), [term()]) -> [term()].
join(Sep, List) ->
    lists:join(Sep, List).
-else.
-spec join(string(), [string()]) -> string().
join(Sep, List) ->
    string:join(List, Sep).
-endif.
