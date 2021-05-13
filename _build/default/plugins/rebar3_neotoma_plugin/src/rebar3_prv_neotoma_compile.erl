-module(rebar3_prv_neotoma_compile).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name,       ?PROVIDER},
                                {module,     ?MODULE},
                                {namespace,  neotoma},
                                {bare,       false},
                                {deps,       ?DEPS},
                                {example,    "rebar3 neotoma compile"},
                                {short_desc, "compile peg files."},
                                {desc,       "compile peg files."},
                                {opts,       []}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running neotoma...", []),
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         CompileFun = fun(Source, Target, _Config) ->
                              OutDir = filename:dirname(Target),
                              neotoma:file(Source, [{output, OutDir}])
                      end,

         rebar_base_compiler:run(Opts, [], SourceDir, ".peg", SourceDir, ".erl", CompileFun, [{check_last_mod, true}])
     end || AppInfo <- Apps],

    {ok, State}.

format_error(Error) ->
    io_lib:format("~p", [Error]).
