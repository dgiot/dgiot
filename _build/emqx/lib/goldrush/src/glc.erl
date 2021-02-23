%% Copyright (c) 2012, Magnus Klaar <klaar@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


%% @doc Event filter implementation.
%%
%% An event query is constructed using the built in operators exported from
%% this module. The filtering operators are used to specify which events
%% should be included in the output of the query. The default output action
%% is to copy all events matching the input filters associated with a query
%% to the output. This makes it possible to construct and compose multiple
%% queries at runtime.
%%
%% === Examples of built in filters ===
%% ```
%% %% Select all events where 'a' exists and is greater than 0.
%% glc:gt(a, 0).
%% %% Select all events where 'a' exists and is equal to 0.
%% glc:eq(a, 0).
%% %% Select all events where 'a' exists and is not equal to 0.
%% glc:neq(a, 0).
%% %% Select all events where 'a' exists and is less than 0.
%% glc:lt(a, 0).
%% %% Select all events where 'a' exists and is anything.
%% glc:wc(a).
%%
%% %% Select no input events. Used as black hole query.
%% glc:null(false).
%% %% Select all input events. Used as passthrough query.
%% glc:null(true).
%% '''
%%
%% === Examples of combining filters ===
%% ```
%% %% Select all events where both 'a' and 'b' exists and are greater than 0.
%% glc:all([glc:gt(a, 0), glc:gt(b, 0)]).
%% %% Select all events where 'a' or 'b' exists and are greater than 0.
%% glc:any([glc:gt(a, 0), glc:gt(b, 0)]).
%% '''
%%
%% === Handling output events ===
%%
%% Once a query has been composed it is possible to override the output action
%% with an erlang function. The function will be applied to each output event
%% from the query. The return value from the function will be ignored.
%%
%% ```
%% %% Write all input events as info reports to the error logger.
%% glc:with(glc:null(true), fun(E) ->
%%     error_logger:info_report(gre:pairs(E)) end).
%% '''
%%
-module(glc).

-export([
    compile/2,
    compile/3,
    compile/4,
    handle/2,
    get/2,
    delete/1,
    reset_counters/1,
    reset_counters/2
]).

-export([
    lt/2, lte/2,
    eq/2, neq/2,
    gt/2, gte/2,
    wc/1,
    nf/1
]).

-export([
    all/1,
    any/1,
    null/1,
    with/2
]).

-export([
    input/1,
    output/1,
    filter/1,
    union/1
]).

-record(module, {
    'query' :: term(),
    tables :: [{atom(), atom()}],
    qtree :: term(),
    store :: term()
}).

-spec lt(atom(), term()) -> glc_ops:op().
lt(Key, Term) ->
    glc_ops:lt(Key, Term).

-spec lte(atom(), term()) -> glc_ops:op().
lte(Key, Term) ->
    glc_ops:lte(Key, Term).

-spec eq(atom(), term()) -> glc_ops:op().
eq(Key, Term) ->
    glc_ops:eq(Key, Term).

-spec neq(atom(), term()) -> glc_ops:op().
neq(Key, Term) ->
    glc_ops:neq(Key, Term).

-spec gt(atom(), term()) -> glc_ops:op().
gt(Key, Term) ->
    glc_ops:gt(Key, Term).

-spec gte(atom(), term()) -> glc_ops:op().
gte(Key, Term) ->
    glc_ops:gte(Key, Term).

-spec wc(atom()) -> glc_ops:op().
wc(Key) ->
    glc_ops:wc(Key).

-spec nf(atom()) -> glc_ops:op().
nf(Key) ->
    glc_ops:nf(Key).

%% @doc Filter the input using multiple filters.
%%
%% For an input to be considered valid output the all filters specified
%% in the list must hold for the input event. The list is expected to
%% be a non-empty list. If the list of filters is an empty list a `badarg'
%% error will be thrown.
-spec all([glc_ops:op()]) -> glc_ops:op().
all(Filters) ->
    glc_ops:all(Filters).


%% @doc Filter the input using one of multiple filters.
%%
%% For an input to be considered valid output on of the filters specified
%% in the list must hold for the input event. The list is expected to be
%% a non-empty list. If the list of filters is an empty list a `badarg'
%% error will be thrown.
-spec any([glc_ops:op()]) -> glc_ops:op().
any(Filters) ->
    glc_ops:any(Filters).


%% @doc Always return `true' or `false'.
-spec null(boolean()) -> glc_ops:op().
null(Result) ->
    glc_ops:null(Result).


%% @doc Apply a function to each output of a query.
%%
%% Updating the output action of a query finalizes it. Attempting
%% to use a finalized query to construct a new query will result
%% in a `badarg' error.
-spec with(glc_ops:op(), fun((gre:event()) -> term())) -> glc_ops:op().
with(Query, Action) ->
    glc_ops:with(Query, Action).


%% @doc Return a union of multiple queries.
%%
%% The union of multiple queries is the equivalent of executing multiple
%% queries separately on the same input event. The advantage is that filter
%% conditions that are common to all or some of the queries only need to
%% be tested once.
%%
%% All queries are expected to be valid and have an output action other
%% than the default which is `output'. If these expectations don't hold
%% a `badarg' error will be thrown.
-spec union([glc_ops:op()]) -> glc_ops:op().
union(Queries) ->
    glc_ops:union(Queries).


%% @doc Compile a query to a module.
%%
%% On success the module representing the query is returned. The module and
%% data associated with the query must be released using the {@link delete/1}
%% function. The name of the query module is expected to be unique.
%% The counters are reset by default, unless Reset is set to false 
-spec compile(atom(), glc_ops:op() | [glc_ops:op()]) -> {ok, atom()}.
compile(Module, Query) ->
    compile(Module, Query, undefined, true).

-spec compile(atom(), glc_ops:op() | [glc_ops:op()], boolean()) -> {ok, atom()}.
compile(Module, Query, Reset) when is_boolean(Reset) ->
    compile(Module, Query, undefined, Reset);
compile(Module, Query, undefined) ->
    compile(Module, Query, undefined, true);
compile(Module, Query, Store) when is_list(Store) ->
    compile(Module, Query, Store, true).

compile(Module, Query, Store, Reset) ->
    {ok, ModuleData} = module_data(Module, Query, Store),
    case glc_code:compile(Module, ModuleData) of
        {ok, Module} when Reset ->
            reset_counters(Module),
            {ok, Module};
        {ok, Module} ->
            {ok, Module}
    end.


%% @doc Handle an event using a compiled query.
%%
%% The input event is expected to have been returned from {@link gre:make/2}.
-spec handle(atom(), list({atom(), term()}) | gre:event()) -> ok.
handle(Module, Event) when is_list(Event) ->
    Module:handle(gre:make(Event, [list]));
handle(Module, Event) ->
    Module:handle(Event).

get(Module, Key) ->
    Module:get(Key).
%% @doc The number of input events for this query module.
-spec input(atom()) -> non_neg_integer().
input(Module) ->
    Module:info(input).

%% @doc The number of output events for this query module.
-spec output(atom()) -> non_neg_integer().
output(Module) ->
    Module:info(output).

%% @doc The number of filtered events for this query module.
-spec filter(atom()) -> non_neg_integer().
filter(Module) ->
    Module:info(filter).


%% @doc Release a compiled query.
%%
%% This releases all resources allocated by a compiled query. The query name
%% is expected to be associated with an existing query module. Calling this
%% function will shutdown all relevant processes and purge/delete the module.
-spec delete(atom()) -> ok.
delete(Module) ->
    Params = params_name(Module),
    Counts = counts_name(Module),
    ManageParams = manage_params_name(Module),
    ManageCounts = manage_counts_name(Module),

    _ = [ begin 
        ok = supervisor:terminate_child(Sup, Name),
        ok = supervisor:delete_child(Sup, Name)
      end || {Sup, Name} <- 
        [{gr_manager_sup, ManageParams}, {gr_manager_sup, ManageCounts},
         {gr_param_sup, Params}, {gr_counter_sup, Counts}]
    ],

    code:soft_purge(Module),
    code:delete(Module),
    ok.

%% @doc Reset all counters
%%
%% This resets all the counters associated with a module
-spec reset_counters(atom()) -> ok.
reset_counters(Module) ->
    Module:reset_counters(all).

%% @doc Reset a specific counter
%%
%% This resets a specific counter associated with a module
-spec reset_counters(atom(), atom()) -> ok.
reset_counters(Module, Counter) ->
    Module:reset_counters(Counter).

%% @private Map a query to a module data term.
-spec module_data(atom(), term(), term()) -> {ok, #module{}}.
module_data(Module, Query, Store) ->
    %% terms in the query which are not valid arguments to the
    %% erl_syntax:abstract/1 functions are stored in ETS.
    %% the terms are only looked up once they are necessary to
    %% continue evaluation of the query.

    %% query counters are stored in a shared ETS table. this should
    %% be an optional feature. enabled by defaults to simplify tests.
    %% the abstract_tables/1 function expects a list of name-atom pairs.
    %% tables are referred to by name in the generated code. the table/1
    %% function maps names to registered processes response for those tables.
    Tables = module_tables(Module),
    Query2 = glc_lib:reduce(Query),
    {ok, #module{'query'=Query, tables=Tables, qtree=Query2, store=Store}}.

%% @private Create a data managed supervised process for params, counter tables
module_tables(Module) ->
    Params = params_name(Module),
    Counts = counts_name(Module),
    ManageParams = manage_params_name(Module),
    ManageCounts = manage_counts_name(Module),
    Counters = [{input,0}, {filter,0}, {output,0}],

    _ = supervisor:start_child(gr_param_sup, 
        {Params, {gr_param, start_link, [Params]}, 
        transient, brutal_kill, worker, [Params]}),
    _ = supervisor:start_child(gr_counter_sup, 
        {Counts, {gr_counter, start_link, [Counts]}, 
        transient, brutal_kill, worker, [Counts]}),
    _ = supervisor:start_child(gr_manager_sup, 
        {ManageParams, {gr_manager, start_link, [ManageParams, Params, []]},
        transient, brutal_kill, worker, [ManageParams]}),
    _ = supervisor:start_child(gr_manager_sup, {ManageCounts, 
        {gr_manager, start_link, [ManageCounts, Counts, Counters]},
        transient, brutal_kill, worker, [ManageCounts]}),
    [{params,Params}, {counters, Counts}].

reg_name(Module, Name) ->
    list_to_atom("gr_" ++ atom_to_list(Module) ++ Name).

params_name(Module) -> reg_name(Module, "_params").
counts_name(Module) -> reg_name(Module, "_counters").
manage_params_name(Module) -> reg_name(Module, "_params_mgr").
manage_counts_name(Module) -> reg_name(Module, "_counters_mgr").



%% @todo Move comment.
%% @private Map a query to a simplified query tree term.
%%
%% The simplified query tree is used to combine multiple queries into one
%% query module. The goal of this is to reduce the filtering and dispatch
%% overhead when multiple concurrent queries are executed.
%%
%% A fixed selection condition may be used to specify a property that an event
%% must have in order to be considered part of the input stream for a query.
%%
%% For the sake of simplicity it is only possible to define selection
%% conditions using the fields present in the context and identifiers
%% of an event. The fields in the context are bound to the reserved
%% names:
%%
%% - '$n': node name
%% - '$a': application name
%% - '$p': process identifier
%% - '$t': timestamp
%% 
%%
%% If an event must be selected based on the runtime state of an event handler
%% this must be done in the body of the handler.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_query(Module, Query) ->
    setup_query(Module, Query, undefined).

setup_query(Module, Query, Store) ->
    ?assertNot(erlang:module_loaded(Module)),
    ?assertEqual({ok, Module}, case (catch compile(Module, Query, Store)) of
        {'EXIT',_}=Error -> ?debugFmt("~p", [Error]), Error; Else -> Else end),
    ?assert(erlang:function_exported(Module, table, 1)),
    ?assert(erlang:function_exported(Module, handle, 1)),
    {compiled, Module}.

events_test_() ->
    {foreach,
        fun() ->
                error_logger:tty(false),
                application:start(syntax_tools),
                application:start(compiler),
                application:start(goldrush)
        end,
        fun(_) ->
                application:stop(goldrush),
                application:stop(compiler),
                application:stop(syntax_tools),
                error_logger:tty(true)
        end,
        [
            {"null query compiles",
                fun() ->
                    {compiled, Mod} = setup_query(testmod1, glc:null(false)),
                    ?assertError(badarg, Mod:table(noexists))
                end
            },
            {"params table exists",
                fun() ->
                    {compiled, Mod} = setup_query(testmod2, glc:null(false)),
                    ?assert(is_atom(Mod:table(params))),
                    ?assertMatch([_|_], gr_param:info(Mod:table(params)))
                end
            },
            {"null query exists",
                fun() ->
                    {compiled, Mod} = setup_query(testmod3, glc:null(false)),
                    ?assert(erlang:function_exported(Mod, info, 1)),
                    ?assertError(badarg, Mod:info(invalid)),
                    ?assertEqual({null, false}, Mod:info('query'))
                end
            },
            {"init counters test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod4, glc:null(false)),
                    ?assertEqual(0, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    ?assertEqual(0, Mod:info(output))
                end
            },
            {"filtered events test",
                fun() ->
                    %% If no selection condition is specified no inputs can match.
                    {compiled, Mod} = setup_query(testmod5, glc:null(false)),
                    glc:handle(Mod, gre:make([], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(0, Mod:info(output))
                end
            },
            {"nomatch event test",
                fun() ->
                    %% If a selection condition but no body is specified the event
                    %% is expected to count as filtered out if the condition does
                    %% not hold.
                    {compiled, Mod} = setup_query(testmod6, glc:eq('$n', 'noexists@nohost')),
                    glc:handle(Mod, gre:make([{'$n', 'noexists2@nohost'}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(0, Mod:info(output))
                end
            },
            {"opfilter equal test",
                fun() ->
                    %% If a selection condition but no body is specified the event
                    %% counts as input to the query, but not as filtered out.
                    {compiled, Mod} = setup_query(testmod7a, glc:eq('$n', 'noexists@nohost')),
                    glc:handle(Mod, gre:make([{'$n', 'noexists@nohost'}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output))
                end
            },
            {"opfilter not equal test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod7b, glc:neq('$n', 'noexists@nohost')),
                    glc:handle(Mod, gre:make([{'$n', 'noexists@nohost'}], [list])),
                    glc:handle(Mod, gre:make([{'$n', 'notexists@nohost'}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output))
                end
            },
            {"opfilter wildcard test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod8, glc:wc(a)),
                    glc:handle(Mod, gre:make([{b, 2}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(0, Mod:info(output)),
                    glc:handle(Mod, gre:make([{a, 2}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output))
                end
            },
            {"opfilter notfound test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod9, glc:nf(a)),
                    glc:handle(Mod, gre:make([{a, 2}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(0, Mod:info(output)),
                    glc:handle(Mod, gre:make([{b, 2}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output))
                end
            },
            {"opfilter greater than test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod10a, glc:gt(a, 1)),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'a', 0}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output))
                end
            },
            {"opfilter greater than or equal to test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod10b, glc:gte(a, 1)),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'a', 1}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'a', 0}], [list])),
                    ?assertEqual(3, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(2, Mod:info(output))
                end
            },
            {"opfilter less than test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod11a, glc:lt(a, 1)),
                    glc:handle(Mod, gre:make([{'a', 0}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output)),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output))
                end
            },
            {"opfilter less than or equal to test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod11b, glc:lte(a, 1)),
                    glc:handle(Mod, gre:make([{'a', 0}], [list])),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output)),
                    glc:handle(Mod, gre:make([{'a', 1}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    ?assertEqual(2, Mod:info(output)),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    ?assertEqual(3, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(2, Mod:info(output))
                end
            },
            {"allholds op test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod12,
                        glc:all([glc:eq(a, 1), glc:eq(b, 2)])),
                    glc:handle(Mod, gre:make([{'a', 1}], [list])),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'b', 1}], [list])),
                    glc:handle(Mod, gre:make([{'b', 2}], [list])),
                    ?assertEqual(4, Mod:info(input)),
                    ?assertEqual(4, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'a', 1},{'b', 2}], [list])),
                    ?assertEqual(5, Mod:info(input)),
                    ?assertEqual(4, Mod:info(filter)),
                    ?assertEqual(1, Mod:info(output))
                end
            },
            {"anyholds op test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod13,
                        glc:any([glc:eq(a, 1), glc:eq(b, 2)])),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    glc:handle(Mod, gre:make([{'b', 1}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'a', 1}], [list])),
                    glc:handle(Mod, gre:make([{'b', 2}], [list])),
                    ?assertEqual(4, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter))
                end
            },
            {"with function test",
                fun() ->
                    Self = self(),
                    {compiled, Mod} = setup_query(testmod14,
                        glc:with(glc:eq(a, 1), fun(Event) -> Self ! gre:fetch(a, Event) end)),
                    glc:handle(Mod, gre:make([{a,1}], [list])),
                    ?assertEqual(1, Mod:info(output)),
                    ?assertEqual(1, receive Msg -> Msg after 0 -> notcalled end)
                end
            },
            {"with function storage test",
                fun() ->
                    Self = self(),
                    Store = [{stored, value}],
                    {compiled, Mod} = setup_query(testmod15,
                        glc:with(glc:eq(a, 1), fun(Event, EStore) -> 
                           Self ! {gre:fetch(a, Event), EStore} end),
                         Store),
                    glc:handle(Mod, gre:make([{a,1}], [list])),
                    ?assertEqual(1, Mod:info(output)),
                    ?assertEqual(1, receive {Msg, Store} -> Msg after 0 -> notcalled end)
                end
            },
            {"delete test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod16, glc:null(false)),
                    ?assert(is_atom(Mod:table(params))),
                    ?assertMatch([_|_], gr_param:info(Mod:table(params))),
                    ?assert(is_list(code:which(Mod))),
                    ?assert(is_pid(whereis(params_name(Mod)))),
                    ?assert(is_pid(whereis(counts_name(Mod)))),
                    ?assert(is_pid(whereis(manage_params_name(Mod)))),
                    ?assert(is_pid(whereis(manage_counts_name(Mod)))),

                    glc:delete(Mod),
                    
                    ?assertEqual(non_existing, code:which(Mod)),
                    ?assertEqual(undefined, whereis(params_name(Mod))),
                    ?assertEqual(undefined, whereis(counts_name(Mod))),
                    ?assertEqual(undefined, whereis(manage_params_name(Mod))),
                    ?assertEqual(undefined, whereis(manage_counts_name(Mod)))
                end
            },
            {"reset counters test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod17,
                        glc:any([glc:eq(a, 1), glc:eq(b, 2)])),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    glc:handle(Mod, gre:make([{'b', 1}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'a', 1}], [list])),
                    glc:handle(Mod, gre:make([{'b', 2}], [list])),
                    ?assertEqual(4, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter)),
                    ?assertEqual(2, Mod:info(output)),

                    glc:reset_counters(Mod, input),
                    ?assertEqual(0, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter)),
                    ?assertEqual(2, Mod:info(output)),
                    glc:reset_counters(Mod, filter),
                    ?assertEqual(0, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    ?assertEqual(2, Mod:info(output)),
                    glc:reset_counters(Mod),
                    ?assertEqual(0, Mod:info(input)),
                    ?assertEqual(0, Mod:info(filter)),
                    ?assertEqual(0, Mod:info(output))
                end
            },
            {"ets data recovery test",
                fun() ->
                    Self = self(),
                    {compiled, Mod} = setup_query(testmod18,
                        glc:with(glc:eq(a, 1), fun(Event) -> Self ! gre:fetch(a, Event) end)),
                    glc:handle(Mod, gre:make([{a,1}], [list])),
                    ?assertEqual(1, Mod:info(output)),
                    ?assertEqual(1, receive Msg -> Msg after 0 -> notcalled end),
                    ?assertEqual(1, length(gr_param:list(Mod:table(params)))),
                    ?assertEqual(3, length(gr_param:list(Mod:table(counters)))),
                    true = exit(whereis(Mod:table(params)), kill),
                    true = exit(whereis(Mod:table(counters)), kill),
                    ?assertEqual(1, Mod:info(input)),
                    glc:handle(Mod, gre:make([{'a', 1}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(2, Mod:info(output)),
                    ?assertEqual(1, length(gr_param:list(Mod:table(params)))),
                    ?assertEqual(3, length(gr_counter:list(Mod:table(counters))))
                end
            },
            {"variable storage test",
                fun() ->
                    {compiled, Mod} = setup_query(testmod19,
                        glc:eq(a, 2), [{stream, time}]),
                    glc:handle(Mod, gre:make([{'a', 2}], [list])),
                    glc:handle(Mod, gre:make([{'b', 1}], [list])),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{'b', 2}], [list])),
                    ?assertEqual(3, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter)),
                    ?assertEqual({ok, time}, glc:get(Mod, stream)),
                    ?assertEqual({error, undefined}, glc:get(Mod, beam))
                end
            },
            {"with multi function any test",
                fun() ->
                    Self = self(),
                    Store = [{stored, value}],

                    G1 = glc:with(glc:eq(a, 1), fun(_Event, EStore) -> 
                       Self ! {a, EStore} end),
                    G2 = glc:with(glc:eq(b, 2), fun(_Event, EStore) -> 
                       Self ! {b, EStore} end),

                    {compiled, Mod} = setup_query(testmod20, any([G1, G2]),
                         Store),
                    glc:handle(Mod, gre:make([{a,1}], [list])),
                    ?assertEqual(1, Mod:info(output)),
                    ?assertEqual(a, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(b, receive {Msg, _Store} -> Msg after 0 -> notcalled end)
                end
            },
            {"with multi function all test",
                fun() ->
                    Self = self(),
                    Store = [{stored, value}],

                    G1 = glc:with(glc:eq(a, 1), fun(_Event, EStore) -> 
                       Self ! {a, EStore} end),
                    G2 = glc:with(glc:eq(b, 2), fun(_Event, EStore) -> 
                       Self ! {b, EStore} end),
                    G3 = glc:with(glc:eq(c, 3), fun(_Event, EStore) -> 
                       Self ! {c, EStore} end),

                    {compiled, Mod} = setup_query(testmod21, all([G1, G2, G3]),
                         Store),
                    glc:handle(Mod, gre:make([{a,1}], [list])),
                    ?assertEqual(0, Mod:info(output)),
                    ?assertEqual(1, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{a,1}, {b, 2}], [list])),
                    ?assertEqual(0, Mod:info(output)),
                    ?assertEqual(2, Mod:info(filter)),
                    glc:handle(Mod, gre:make([{a,1}, {b, 2}, {c, 3}], [list])),
                    ?assertEqual(1, Mod:info(output)),
                    ?assertEqual(a, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(b, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(c, receive {Msg, _Store} -> Msg after 0 -> notcalled end)
                end
            },
            {"with multi-function output match test",
                fun() ->
                    Self = self(),
                    Store = [{stored, value}],

                    {compiled, Mod} = setup_query(testmod22,
                          [glc:with(glc:eq(a, 1), fun(Event, _EStore) -> 
                              Self ! {a, gre:fetch(a, Event)} end),
                           glc:with(glc:gt(b, 1), fun(Event, _EStore) -> 
                              Self ! {b, gre:fetch(b, Event)} end)],
                         Store),
                    glc:handle(Mod, gre:make([{a,1}, {b, 1}], [list])),
                    ?assertEqual(1, Mod:info(output)),
                    ?assertEqual(a, receive {a=Msg, _Store} -> Msg after 0 -> notcalled end)

                end
            },
            {"with multi-function output double-match test",
                fun() ->
                    Self = self(),
                    Store = [{stored, value}],
                    {compiled, Mod} = setup_query(testmod23,
                          [glc:with(glc:eq(a, 1), fun(Event, _EStore) -> 
                              Self ! {a, gre:fetch(a, Event)} end),
                           glc:with(glc:eq(b, 1), fun(Event, _EStore) -> 
                              Self ! {b, gre:fetch(b, Event)} end)],
                         Store),
                    glc:handle(Mod, gre:make([{a,1}, {b, 1}], [list])),
                    ?assertEqual(2, Mod:info(output)),
                    ?assertEqual(a, receive {a=Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(b, receive {b=Msg, _Store} -> Msg after 0 -> notcalled end)
                end
            },
            {"with multi function complex match test",
                fun() ->
                    Self = self(),
                    Store = [{stored, value}],

                    G1 = glc:with(glc:gt(r, 0.1), fun(_Event, EStore) -> 
                       Self ! {a, EStore} end),
                    G2 = glc:with(glc:all([glc:eq(a, 1), glc:gt(r, 0.5)]), fun(_Event, EStore) -> 
                       Self ! {b, EStore} end),
                    G3 = glc:with(glc:all([glc:eq(a, 1), glc:eq(b, 2), glc:gt(r, 0.6)]), fun(_Event, EStore) -> 
                       Self ! {c, EStore} end),

                    {compiled, Mod} = setup_query(testmod24, [G1, G2, G3],
                         Store),
                    glc:handle(Mod, gre:make([{a,1}, {r, 0.7}, {b, 3}], [list])),
                    ?assertEqual(2, Mod:info(output)),
                    ?assertEqual(1, Mod:info(input)),
                    ?assertEqual(1, Mod:info(filter)),
                    ?assertEqual(b, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(a, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    %
                    glc:handle(Mod, gre:make([{a,1}, {r, 0.6}], [list])),
                    ?assertEqual(4, Mod:info(output)),
                    ?assertEqual(2, Mod:info(input)),
                    ?assertEqual(2, Mod:info(filter)),
                    ?assertEqual(b, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(a, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    %
                    glc:handle(Mod, gre:make([{a,2}, {r, 0.7}, {b, 3}], [list])),
                    ?assertEqual(5, Mod:info(output)),
                    ?assertEqual(3, Mod:info(input)),
                    ?assertEqual(4, Mod:info(filter)),
                    ?assertEqual(a, receive {Msg, _Store} -> Msg after 0 -> notcalled end),

                    glc:handle(Mod, gre:make([{a,1}, {r, 0.7}, {b, 2}], [list])),
                    ?assertEqual(8, Mod:info(output)),
                    ?assertEqual(4, Mod:info(input)),
                    ?assertEqual(4, Mod:info(filter)),
                    ?assertEqual(c, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(b, receive {Msg, _Store} -> Msg after 0 -> notcalled end),
                    ?assertEqual(a, receive {Msg, _Store} -> Msg after 0 -> notcalled end)
                end
            }
        ]
    }.

union_error_test() ->
    ?assertError(badarg, glc:union([glc:eq(a, 1)])),
    done.

-endif.
