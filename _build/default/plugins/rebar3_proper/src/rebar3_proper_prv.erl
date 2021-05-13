-module(rebar3_proper_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, proper).
-define(DEPS, [compile]).
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).
-define(COUNTEREXAMPLE_FILE, "rebar3_proper-counterexamples.consult").
-define(REGRESSION_FILE, "proper-regressions.consult").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {profiles, [test]},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 proper"},   % How to use the plugin
            {opts, proper_opts()},        % list of options understood by the plugin
            {short_desc, "Run PropEr test suites"},
            {desc, "Run PropEr test suites"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    run_pre_hooks(State),
    {Opts, ProperOpts} = handle_opts(State),
    rebar_api:debug("{proper_opts,\n\t% general options:~n\t~p~n\t++~n"
                                "\t% proper-specific options:~n\t~p}",
                     [Opts, ProperOpts]),
    rebar_utils:update_code(rebar_state:code_paths(State, all_deps), [soft_purge]),
    maybe_cover_compile(State),
    %% needed in 3.2.0 and after -- this reloads the code paths required to
    %% compile everything *after* cover-compiling has cleaned up after itself
    %% (which incidentally clears up *our* environment too), but skips reloading
    %% the top level apps' own code paths since those would overwrite the cover-
    %% compiled code sitting in memory. The top app's path is readded to the
    %% code path but not pre-loaded in memory, though.
    TopAppsPaths = app_paths(State),
    rebar_utils:update_code(rebar_state:code_paths(State, all_deps)--TopAppsPaths, [soft_purge]),
    FlatPaths = TopAppsPaths ++ (code:get_path() -- TopAppsPaths),
    true = code:set_path(FlatPaths),

    ensure_proper(),
    SysConfigs = sys_config_list(ProperOpts, Opts),
    Configs = lists:flatmap(fun(Filename) ->
                               rebar_file_utils:consult_config(State, Filename)
                            end, SysConfigs),
    [application:load(Application) || Config <- SysConfigs, {Application, _} <- Config],
    rebar_utils:reread_config(Configs),


    Res = case run_type(Opts) of
        quickcheck -> do_quickcheck(State, Opts, ProperOpts);
        retry -> do_retry(State, Opts, ProperOpts);
        regressions -> do_regressions(State, Opts, ProperOpts);
        store -> do_store(State, Opts, ProperOpts)
    end,
    run_post_hooks(State, Res),
    Res.

run_type(Opts) ->
    case {proplists:get_value(retry, Opts, false),
          proplists:get_value(regressions, Opts, false),
          proplists:get_value(store, Opts, false)} of
        {true, _, _} -> retry;
        {_, true, _} -> regressions;
        {_, _, true} -> store;
        _ -> quickcheck
    end.

do_quickcheck(State, Opts, ProperOpts) ->
    try find_properties(State, Opts) of
        Props ->
            rebar_api:debug("properties: ~p", [Props]),
            Failed = [{Mod, Fun, Res, proper:counterexample()}
                      || {Mod, Fun} <- Props,
                         Res <- [catch check(Mod, Fun, ProperOpts)],
                         Res =/= true],
            rebar_api:debug("Failing Results: ~p", [Failed]),
            maybe_write_coverdata(State),
            rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
            case Failed of
                [] ->
                    Tot = length(Props),
                    rebar_api:info("~n~p/~p properties passed", [Tot, Tot]),
                    {ok, State};
                [_|_] ->
                    Tot = length(Props),
                    FailedCount = length(Failed),
                    Passed = Tot - FailedCount,
                    rebar_api:error("~n~p/~p properties passed, ~p failed", [Passed, Tot, FailedCount]),
                    store_counterexamples(State, Failed),
                    ?PRV_ERROR({failed, Failed})
            end
    catch
        throw:{module_not_found,_Mod,_Props}=Error -> ?PRV_ERROR(Error);
        throw:{property_not_found,_Prop,_Mods}=Error -> ?PRV_ERROR(Error)
    end.

do_retry(State, Opts, ProperOpts) ->
    Base = rebar_dir:base_dir(State),
    FilePath = filename:join([Base, ?COUNTEREXAMPLE_FILE]),
    case file:consult(FilePath) of
        {ok, Data} ->
            run_retries(State, Opts, ProperOpts, Data);
        {error, _} ->
            rebar_api:info("no counterexamples to run.", []),
            {ok, State}
    end.

do_regressions(State, Opts, ProperOpts) ->
    Dir = proplists:get_value(dir, Opts, "test"),
    FilePath = filename:join([Dir, ?REGRESSION_FILE]),
    case file:consult(FilePath) of
        {ok, Data} ->
            run_retries(State, Opts, ProperOpts, Data);
        {error, _} ->
            rebar_api:info("no regression tests to run.", []),
            {ok, State}
    end.

do_store(State, Opts, _ProperOpts) ->
    Base = rebar_dir:base_dir(State),
    FilePath = filename:join([Base, ?COUNTEREXAMPLE_FILE]),
    case file:consult(FilePath) of
        {ok, Data} ->
            Dir = proplists:get_value(dir, Opts, "test"),
            RegressionPath = filename:join([Dir, ?REGRESSION_FILE]),
            filelib:ensure_dir(RegressionPath),
            {ok, Io} = file:open(RegressionPath, [append, {encoding, utf8}]),
            rebar_api:debug("Storing counterexamples to ~s", [RegressionPath]),
            {ok, Prior} = file:consult(RegressionPath),
            [io:format(Io, "~n~p.~n", [{Mod,Fun,CounterEx}])
             || {Mod,Fun,CounterEx} <- Data,
                CounterEx =/= undefined,
                not lists:member({Mod,Fun,CounterEx}, Prior)], % dedupe
            file:close(Io),
            {ok, State};
        {error, ConsultErr} ->
            rebar_api:debug("counterexample file consult result: ~p", [ConsultErr]),
            rebar_api:info("no counterexamples to store.", []),
            {ok, State}
    end.


run_retries(State, Opts, ProperOpts, CounterExamples) ->
    Dir = proplists:get_value(dir, Opts, "test"),
    {Mods, Props} = lists:unzip([{atom_to_list(M), atom_to_list(F)}
                                   || {M, F, _Args} <- CounterExamples]),
    try find_properties(State, Dir, Mods, Props) of
        Found ->
            ExpectedLen = length(CounterExamples),
            FoundLen = length(Found),
            rebar_api:info("Running ~p counterexamples out of ~p properties",
                           [ExpectedLen, FoundLen]),
            Failed = [{M, F, Result, Args}
                      || {M,F,Args} <- CounterExamples,
                         lists:member({M,F}, Found),
                         Result <- [catch retry(M, F, Args, ProperOpts)],
                         Result =/= true],
            FailedCount = length(Failed),
            Passed = ExpectedLen - FailedCount,
            case Failed of
                [] ->
                    rebar_api:info("~n~p/~p counterexamples passed", [Passed, ExpectedLen]),
                    {ok, State};
                [_|_] ->
                    rebar_api:error("~n~p/~p counterexamples passed, ~p failed",
                                    [Passed, ExpectedLen, FailedCount]),
                    ?PRV_ERROR({failed, Failed})
            end
    catch
        throw:{module_not_found,_Mod,_Props}=Error -> ?PRV_ERROR(Error);
        throw:{property_not_found,_Prop,_Mods}=Error -> ?PRV_ERROR(Error)
    end.



-spec format_error(any()) ->  iolist().
format_error({failed, Failed}) ->
    ["Failed test cases:",
     [io_lib:format("~n~p:~p() -> ~p~ts",
                    [M,F,Res,format_doc(M,F)]) || {M,F,Res,_} <- Failed]];
format_error({module_not_found, Mod, any}) ->
    io_lib:format("Module ~p does not exist or exports no properties", [Mod]);
format_error({module_not_found, Mod, _}) ->
    io_lib:format("Module ~p does not exist", [Mod]);
format_error({property_not_found, Prop, []}) ->
    io_lib:format("Property ~p does not belong to any module", [Prop]);
format_error({property_not_found, Prop, Mods}) ->
    io_lib:format("Property ~p does not belong to any module in ~p", [Prop, Mods]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private
%% ===================================================================
maybe_cover_compile(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
                 true  -> rebar_state:set(State, cover_enabled, true);
                 false -> State
             end,
    rebar_prv_cover:maybe_cover_compile(State1).

maybe_write_coverdata(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
                 true  -> rebar_state:set(State, cover_enabled, true);
                 false -> State
             end,
    rebar_prv_cover:maybe_write_coverdata(State1, ?PROVIDER).

ensure_proper() ->
    try proper:module_info() of
        _ -> ok
    catch
        error:undef ->
            rebar_api:abort("PropEr not found. Add it as a dependency of "
                            "the application you are testing.", [])
    end.

check(Mod, Fun, Opts) ->
    rebar_api:info("Testing ~p:~p()", [Mod, Fun]),
    NewOpts = fetch_opts(Mod, Fun, Opts),
    proper:quickcheck(Mod:Fun(), NewOpts).

retry(Mod, Fun, Args, Opts) ->
    rebar_api:info("Retrying ~p:~p()", [Mod, Fun]),
    NewOpts = fetch_opts(Mod, Fun, Opts),
    proper:check(Mod:Fun(), Args, NewOpts).

fetch_opts(Mod, Fun, Opts) ->
    try Mod:Fun(opts) of
        TestOpts ->
            rebar_api:debug("Custom test options found for ~p:~p():~n\t~p",
                            [Mod, Fun, TestOpts]),
            TestOpts ++ Opts
    catch
        error:E when E == undef; E == function_clause ->
            rebar_api:debug("~p:~p(opts) not found; using predefined options",
                            [Mod, Fun]),
            Opts
    end.

store_counterexamples(State, Failed) ->
    Base = rebar_dir:base_dir(State),
    FilePath = filename:join([Base, ?COUNTEREXAMPLE_FILE]),
    {ok, Io} = file:open(FilePath, [write, {encoding, utf8}]),
    rebar_api:debug("Writing counterexamples to ~s", [FilePath]),
    %% Then run as proper:check(Mod:Fun(), CounterEx)
    [io:format(Io, "~p.~n", [{Mod,Fun,CounterEx}]) || {Mod,Fun,_,CounterEx} <- Failed,
                                                      CounterEx =/= undefined],
    file:close(Io),
    ok.

find_properties(State, Opts) ->
    Dir = proplists:get_value(dir, Opts, "test"),
    Mods = proplists:get_value(module, Opts, any),
    Props = proplists:get_value(properties, Opts, any),
    Found = find_properties(State, Dir, Mods, Props),
    rebar_api:debug("Found: ~p", [Found]),
    {ModsFound0, PropsFound0} = lists:unzip(Found),
    ModsFound = [atom_to_list(Mod) || Mod <- ModsFound0],
    PropsFound = [atom_to_list(Prop) || Prop <- PropsFound0],
    Props =/= any andalso
        [throw({property_not_found, Prop, Mods})
         || Prop <- Props, not lists:member(Prop, PropsFound)],
    Mods =/= any andalso
        [throw({module_not_found, Mod, Props})
         || Mod <- Mods, not lists:member(Mod, ModsFound)],
    Found.

find_properties(State, Dir, Mods, Props) ->
    %% Fetch directories and app configs
    RawDirs = [{{rebar_app_info:name(App),
                 filename:join([rebar_app_info:out_dir(App), Dir])},
                filename:join(rebar_app_info:dir(App), Dir)}
               || App <- rebar_state:project_apps(State),
                  not rebar_app_info:is_checkout(App)],
    %% Pick a root test directory for umbrella apps
    UmbrellaDir =
        [{{<<"root">>,
           filename:join(rebar_dir:base_dir(State), "prop_"++Dir)},
         P} || P <- [make_absolute_path(filename:join([".", Dir]))],
               not lists:member(P, [D || {_,D} <- RawDirs])],
    TestDirs = RawDirs ++ UmbrellaDir,
    rebar_api:debug("SearchDirs: ~p", [TestDirs]),
    %% Keep directories with properties in them
    Dirs = [{App, TestDir}
            || {App, TestDir} <- TestDirs,
               {ok, Files} <- [file:list_dir(TestDir)],
               lists:any(fun(File) -> prop_suite(Mods, File) end, Files)],
    [Prop || {_, TestDir} <- Dirs,
             {ok, Files} <- [file:list_dir(TestDir)],
             File <- Files,
             prop_suite(Mods, File),
             mod_compiled(module(File), TestDir),
             Prop <- properties(Props, module(File))].

prop_suite(Mods, File) ->
    Mod = filename:basename(File, ".erl"),
    filename:extension(File) =:= ".erl"
    andalso
    ((Mods =:= any andalso lists:prefix("prop_", Mod))
     orelse
     (Mods =/= any andalso lists:member(Mod, Mods))).

module(File) ->
    list_to_atom(filename:basename(File, ".erl")).

mod_compiled(Mod, TestDir) ->
    try Mod:module_info() of
        _ -> true
    catch
        error:undef when TestDir =:= "test" ->
            rebar_api:debug("Skipping module ~p since it was not compiled.",
                            [Mod]),
            false;
        error:undef ->
            rebar_api:debug("Skipping module ~p since it was not compiled. "
                            "Verify presence in extra_src_dirs", [Mod]),
            false
    end.

properties(any, Mod) ->
    [{Mod, Prop} || {Prop,0} <- Mod:module_info(exports),
                    prop_prefix(Prop)];
properties(Props, Mod) ->
    [{Mod, Prop} || {Prop,0} <- Mod:module_info(exports),
                    lists:member(atom_to_list(Prop), Props)].

prop_prefix(Atom) ->
    lists:prefix("prop_", atom_to_list(Atom)).

proper_opts() ->
    [{dir, $d, "dir", string,
      "directory where the property tests are located (defaults to \"test\"). "
      "The directory also needs to be declared in extra_src_dirs."},
     {module, $m, "module", string,
      "name of one or more modules to test (comma-separated)"},
     {properties, $p, "prop", string,
      "name of properties to test within a specified module (comma-separated)"},
     {numtests, $n, "numtests", integer,
      "number of tests to run when testing a given property"},
     {search_steps, $s, "search_steps", integer,
      "number of searches to run when testing a given targeted property"},
     {verbose, $v, "verbose", boolean,
      "each property tested shows its output or not (defaults to true)"},
     {cover, $c, "cover", {boolean, false},
      "generate cover data"},
     %% no short format for these buddies
     {retry, undefined, "retry", {boolean, false},
      "If failing test case counterexamples have been stored, "
      "they are retried"},
     {regressions, undefined, "regressions", {boolean, false},
      "replays the test cases stored in the regression file."},
     {store, undefined, "store", {boolean, false},
      "stores the last counterexample into the regression file."},
     {long_result, undefined, "long_result", boolean,
      "enables long-result mode, displaying counter-examples on failure "
      "rather than just false"},
     {start_size, undefined, "start_size", integer,
      "specifies the initial value of the size parameter"},
     {max_size, undefined, "max_size", integer,
      "specifies the maximum value of the size parameter"},
     {max_shrinks, undefined, "max_shrinks", integer,
      "specifies the maximum number of times a failing test case should be "
      "shrunk before returning"},
     {noshrink, undefined, "noshrink", boolean,
      "instructs PropEr to not attempt to shrink any failing test cases"},
     {constraint_tries, undefined, "constraint_tries", integer,
      "specifies the maximum number of tries before the generator subsystem "
      "gives up on producing an instance that satisfies a ?SUCHTHAT "
      "constraint"},
     {spec_timeout, undefined, "spec_timeout", integer,
      "duration, in milliseconds, after which PropEr considers an input "
      "to be failing"},
     {any_to_integer, undefined, "any_to_integer", boolean,
      "converts instances of the any() type to integers in order to speed "
      "up execution"},
     {on_output, undefined, "on_output", string,
      "specifies a binary function '{Mod,Fun}', similar to io:format/2, "
      "to be used for all output printing"},
     {sys_config, undefined, "sys_config", string,
      "config file to load before starting tests"}
    ].

handle_opts(State) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    ConfigOpts = rebar_state:get(State, proper_opts, []),
    {fill_defaults(rebar3_opts(merge_opts(ConfigOpts, CliOpts))),
     proper_opts(merge_opts(ConfigOpts, proper_opts(CliOpts)))}.

fill_defaults(Opts) ->
    [{dir, "test"} || proplists:get_value(dir, Opts) =:= undefined] ++
    [{mods, any} || proplists:get_value(mods, Opts) =:= undefined] ++
    [{properties, any} || proplists:get_value(properties, Opts) =:= undefined]
    ++ Opts.

rebar3_opts([]) ->
    [];
rebar3_opts([{dir, Dir} | T]) ->
    [{dir, Dir} | rebar3_opts(T)];
rebar3_opts([{module, Mods} | T]) ->
    [{module, maybe_parse_csv(Mods)} | rebar3_opts(T)];
rebar3_opts([{properties, Props} | T]) ->
    [{properties, maybe_parse_csv(Props)} | rebar3_opts(T)];
rebar3_opts([{retry, Retry} | T]) ->
    [{retry, Retry} | rebar3_opts(T)];
rebar3_opts([{regressions, Retry} | T]) ->
    [{regressions, Retry} | rebar3_opts(T)];
rebar3_opts([{sys_config, Config} | T]) ->
    [{sys_config, Config} | rebar3_opts(T)];
rebar3_opts([{store, Retry} | T]) ->
    [{store, Retry} | rebar3_opts(T)];
rebar3_opts([_ | T]) ->
    rebar3_opts(T).

proper_opts([]) -> [];
proper_opts([{verbose, true} | T]) -> [verbose | proper_opts(T)];
proper_opts([{verbose, false} | T]) -> [quiet | proper_opts(T)];
proper_opts([{long_result, true} | T]) -> [long_result | proper_opts(T)];
proper_opts([{long_result, false} | T]) -> proper_opts(T);
proper_opts([{noshrink, true} | T]) -> [noshrink | proper_opts(T)];
proper_opts([{noshrink, false} | T]) -> proper_opts(T);
proper_opts([{any_to_integer, true} | T]) -> [any_to_integer | proper_opts(T)];
proper_opts([{any_to_integer, false} | T]) -> proper_opts(T);
proper_opts([{on_output, {Mod, Fun}} | T]) ->
    [{on_output, fun Mod:Fun/2} | proper_opts(T)];
proper_opts([{on_output, MFStr} | T]) when is_list(MFStr) ->
    case on_output(MFStr) of
        undefined -> proper_opts(T);
        Fun       -> [{on_output, Fun} | proper_opts(T)]
    end;
%% those are rebar3-only options
proper_opts([{dir,_} | T]) -> proper_opts(T);
proper_opts([{module,_} | T]) -> proper_opts(T);
proper_opts([{properties,_} | T]) -> proper_opts(T);
proper_opts([{cover,_} | T]) -> proper_opts(T);
proper_opts([{retry,_} | T]) -> proper_opts(T);
proper_opts([{regressions,_} | T]) -> proper_opts(T);
proper_opts([{sys_config,_} | T]) -> proper_opts(T);
proper_opts([{store,_} | T]) -> proper_opts(T);
%% fall-through
proper_opts([H|T]) -> [H | proper_opts(T)].

merge_opts(Old, New) ->
    rebar_utils:tup_umerge(New, Old).

maybe_parse_csv(Data) ->
    case is_atom_list(Data) of
        true -> [atom_to_list(D) || D <- Data];
        false -> parse_csv(Data)
    end.

is_atom_list([]) -> true;
is_atom_list([H|T]) when is_atom(H) -> is_atom_list(T);
is_atom_list(_) -> false.

parse_csv(IoData) ->
    re:split(IoData, ", *", [{return, list}]).

-spec on_output(MFStr :: string()) -> 'undefined' | Fun when
      Fun :: fun((Format :: io:format(), Data :: [term()]) -> ok).
on_output(MFStr) ->
    case erl_scan:string(MFStr ++ ".") of
        {ok, Tokens, _EndLocation} ->
            case erl_parse:parse_term(Tokens) of
                {ok, {Mod, Fun}} -> fun Mod:Fun/2;
                _                -> undefined
            end;
        _ -> undefined
    end.

app_paths(State) ->
    Apps = rebar_state:project_apps(State),
    [rebar_app_info:ebin_dir(App) || App <- Apps,
                                     not rebar_app_info:is_checkout(App)].

sys_config_list(CmdOpts, CfgOpts) ->
    CmdSysConfigs = split_string(proplists:get_value(sys_config, CmdOpts, "")),
    case proplists:get_value(sys_config, CfgOpts, []) of
        [H | _]=Configs when is_list(H) ->
            Configs ++ CmdSysConfigs;
        [] ->
            CmdSysConfigs;
        Configs ->
            [Configs | CmdSysConfigs]
    end.

split_string(String) ->
    string:tokens(String, [$,]).

make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.

run_pre_hooks(State) ->
    Providers = rebar_state:providers(State),
    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_project_and_app_hooks(Cwd, pre, ?PROVIDER, Providers, State).

run_post_hooks(_, {ok, State}) -> run_post_hooks_(State);
run_post_hooks(State, _) -> run_post_hooks_(State).

run_post_hooks_(State) ->
    Providers = rebar_state:providers(State),
    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_project_and_app_hooks(Cwd, post, ?PROVIDER, Providers, State),
    %% reset code paths for the plugin if we want to handle our own errors
    %% since the rebar3 hooks drop them by default
    PluginDepsPaths = lists:usort(rebar_state:code_paths(State, all_plugin_deps)),
    code:add_pathsa(PluginDepsPaths),
    ok.

format_doc(Mod, Fun) ->
    try Mod:Fun(doc) of
        IoData -> [" (", IoData, $)]
    catch
        error:E when E == undef; E == function_clause ->
            rebar_api:debug("~p:~p(doc) not found; omitting docstring",
                            [Mod,Fun]),
            []
    end.

