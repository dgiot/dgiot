-module(providers).

%% API
-export([create/1,
         new/2,
         do/2,
         profiles/1,
         namespace/1,
         module/1,
         impl/1,
         opts/1,
         desc/1,
         process_deps/2,
         get_provider/2,
         get_provider/3,
         get_provider_by_module/2,
         get_providers_by_namespace/2,
         get_target_providers/2,
         get_target_providers/3,
         hooks/1,
         hooks/2,
         help/1,
         help/2,
         help/3,
         format_error/1,
         format_error/2,
         format/1]).

-export_type([t/0]).

-include("providers.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-record(provider,  { name              :: atom(),               % The 'user friendly' name of the task
                     module            :: module(),             % The module implementation of the task
                     hooks             :: {list(), list()},
                     bare              :: boolean(),            % Indicates whether task can be run by user
                     deps              :: [atom()],             % The list of dependencies
                     desc              :: string(),             % The description for the task
                     short_desc        :: string(),             % A one line short description of the task
                     example           :: string() | undefined, % An example of the task usage
                     opts              :: list(),               % The list of options that the task requires/understands
                     profiles          :: [atom()],               % Profile to use for provider
                     namespace=default :: atom()                % namespace the provider is registered in
                   }).

-type t() :: #provider{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new provider object from the specified module. The
%% module should implement the provider behaviour.
%%
%% @param ModuleName The module name.
%% @param State0 The current state of the system
-spec new(module(), any()) -> {ok, any()} | {error, string()}.
new(ModuleName, State) when is_atom(ModuleName) ->
    case code:which(ModuleName) of
        non_existing ->
            {error, io_lib:format("Module ~p does not exist.", [ModuleName])};
        _ ->
            ModuleName:init(State)
    end.

-spec create(list()) -> t().
create(Attrs) ->
    #provider{ name          = proplists:get_value(name, Attrs, undefined)
             , module        = proplists:get_value(module, Attrs, undefined)
             , hooks         = proplists:get_value(hooks, Attrs, {[], []})
             , bare          = proplists:get_value(bare, Attrs, true)
             , deps          = proplists:get_value(deps, Attrs, [])
             , desc          = proplists:get_value(desc, Attrs, "")
             , short_desc    = proplists:get_value(short_desc, Attrs, "")
             , example       = proplists:get_value(example, Attrs, "")
             , opts          = proplists:get_value(opts, Attrs, [])
             , profiles      = proplists:get_value(profiles, Attrs, [default])
             , namespace     = proplists:get_value(namespace, Attrs, default) }.

%% @doc Run provider and hooks.
%%
%% @param Provider the provider object
%% @param State the current state of the system
-spec do(t(), any()) -> {ok, any()} | {error, string()} | {error, {module(), any()}}.
do(Provider, State) ->
    (Provider#provider.module):do(State).

-spec profiles(t()) -> [atom()].
profiles(Provider) ->
    Provider#provider.profiles.

-spec namespace(t()) -> atom().
namespace(Provider) ->
    Provider#provider.namespace.

%%% @doc get the name of the module that implements the provider
-spec module(t()) -> module().
module(Provider) ->
    Provider#provider.module.

-spec impl(t()) -> atom().
impl(Provider) ->
    Provider#provider.name.

-spec opts(t()) -> list().
opts(Provider) ->
    Provider#provider.opts.

-spec desc(t()) -> string().
desc(Provider) ->
    Provider#provider.desc.

-spec hooks(t()) -> {[t()], [t()]}.
hooks(Provider) ->
    Provider#provider.hooks.

-spec hooks(t(), {[t()], [t()]}) -> t().
hooks(Provider, Hooks) ->
    Provider#provider{hooks=Hooks}.

help(Providers) when is_list(Providers) ->
    Dict = lists:foldl(
        fun(P, Dict) when P#provider.bare =:= true ->
            dict:append(P#provider.namespace,
                        {ec_cnv:to_list(P#provider.name),
                         P#provider.short_desc},
                        Dict)
        ;  (_, Dict) -> Dict
        end,
        dict:new(),
        Providers),
    Namespaces = [default |
                 lists:usort([NS || #provider{namespace=NS} <- Providers,
                                    NS =/= default])],
    namespace_help(Dict, Namespaces);
help(#provider{opts=Opts
              ,desc=Desc
              ,namespace=Namespace
              ,name=Name}) ->
    case Desc of
        Desc when length(Desc) > 0 ->
            io:format(Desc++"~n");
        _ ->
            ok
    end,

    StrNS = case Namespace of
        default -> "";
        _ -> atom_to_list(Namespace) ++ " "
    end,

    case Opts of
        [] ->
            io:format("Usage: rebar3 ~s~p~n", [StrNS, Name]);
        _ ->
            getopt:usage(Opts, "rebar3 " ++ StrNS ++ atom_to_list(Name), "", [])
    end.

help(Name, Providers) when is_list(Name) ->
    help(list_to_atom(Name), Providers, default);
help(Name, Providers) when is_atom(Name) ->
    help(Name, Providers, default).

help(Name, Providers, Namespace) when is_list(Name) ->
    help(list_to_atom(Name), Providers, Namespace);
help(Name, Providers, Namespace) when is_atom(Name) ->
    Provider = providers:get_provider(Name, Providers, Namespace),
    help(Provider).

format_error({provider_not_found, Namespace, ProviderName}) ->
    io_lib:format("Unable to resolve provider ~s in namespace ~s", [ProviderName, Namespace]).

%% @doc format an error produced from a provider.
-spec format_error(t(), Reason::term()) -> iolist().
format_error(#provider{module=Mod}, Error) ->
    Mod:format_error(Error).

%% @doc print the provider module name
%%
%% @param T - The provider
%% @return An iolist describing the provider
-spec format(t()) -> iolist().
format(#provider{name=Name}) ->
    atom_to_list(Name).

-spec get_target_providers({atom(),atom()} | atom(), list()) -> [{atom(), atom()}].
get_target_providers({Namespace, Target}, Providers) ->
    get_target_providers(Target, Providers, Namespace);
get_target_providers(Target, Providers) ->
    get_target_providers(Target, Providers, default).

get_target_providers(Target, Providers, Namespace) ->
    TargetProviders = lists:filter(fun(#provider{name=T, namespace=NS})
                                         when T =:= Target, NS =:= Namespace ->
                                           true;
                                      (_) ->
                                           false
                                   end, Providers),
    expand_hooks(process_deps(TargetProviders, Providers), [], Providers).

expand_hooks([], TargetProviders, _Providers) ->
    TargetProviders;
expand_hooks([Provider | Tail], TargetProviders, Providers) ->
    {PreHooks, PostHooks} = hooks(get_provider(Provider, Providers)),
    expand_hooks(Tail, TargetProviders++PreHooks++[Provider | PostHooks], Providers).

-spec get_provider(atom() | {atom(), atom()}, [t()]) -> t() | not_found.
get_provider({Namespace, ProviderName}, Providers) ->
    get_provider(ProviderName, Providers, Namespace);
get_provider(ProviderName, Providers) ->
    get_provider(ProviderName, Providers, default).

-spec get_provider(atom(), [t()], atom()) -> t() | not_found.
get_provider(ProviderName,
             [Provider = #provider{name = ProviderName, namespace=Namespace} | _],
             Namespace) ->
    Provider;
get_provider(ProviderName, [_ | Rest], Namespace) ->
    get_provider(ProviderName, Rest, Namespace);
get_provider(_, _, _) ->
    not_found.

-spec get_provider_by_module(atom(), [t()]) -> t() | not_found.
get_provider_by_module(ProviderModule, [Provider = #provider{module = ProviderModule} | _]) ->
    Provider;
get_provider_by_module(ProviderModule, [_ | Rest]) ->
    get_provider_by_module(ProviderModule, Rest);
get_provider_by_module(_ProviderModule, _) ->
    not_found.

-spec get_providers_by_namespace(atom(), [t()]) -> [t()].
get_providers_by_namespace(Namespace, [Provider = #provider{namespace = Namespace} | Rest]) ->
    [Provider | get_providers_by_namespace(Namespace, Rest)];
get_providers_by_namespace(Namespace, [_ | Rest]) ->
    get_providers_by_namespace(Namespace, Rest);
get_providers_by_namespace(_Namespace, []) ->
    [].

process_deps([], _Providers) ->
    [];
process_deps(TargetProviders, Providers) ->
    DepChain = lists:flatmap(fun(Provider) ->
                                     {DC, _, _} = process_deps(Provider, Providers, []),
                                     DC
                             end, TargetProviders),
    Providers1 = lists:flatten([{{none, none},
                               {P#provider.namespace, P#provider.name}} || P <- TargetProviders]
                             ++ DepChain),
    case reorder_providers(Providers1) of
        {error, _}=Error ->
            Error;
        [{none, none} | Rest] ->
            Rest
    end.

process_deps(Provider, Providers, Seen) ->
    case lists:member(Provider, Seen) of
        true ->
            {[], Providers, Seen};
        false ->
            Deps = Provider#provider.deps,
            Namespace = Provider#provider.namespace,
            DepList = lists:map(fun({NS, Dep}) ->
                                        {{NS, Dep}, {Namespace, Provider#provider.name}};
                                   (Dep) ->
                                        {{Namespace, Dep}, {Namespace, Provider#provider.name}}
                                end, Deps),
            {NewDeps, _, NewSeen} =
                lists:foldl(fun({NS, Arg}, Acc) ->
                                    process_dep({NS, Arg}, Acc);
                                (Arg, Acc) ->
                                    process_dep({Namespace, Arg}, Acc)
                            end,
                           {[], Providers, Seen}, Deps),
            {[DepList | NewDeps], Providers, NewSeen}
    end.

process_dep({Namespace, ProviderName}, {Deps, Providers, Seen}) ->
    case get_provider(ProviderName, Providers, Namespace) of
        not_found ->
            throw(?PRV_ERROR({provider_not_found, Namespace, ProviderName}));
        Provider ->
            {NewDeps, _, NewSeen} = process_deps(Provider#provider{namespace=Namespace}, Providers, [ProviderName | Seen]),
            {[Deps | NewDeps], Providers, NewSeen}
    end.

%% @doc Reorder the providers according to thier dependency set.
reorder_providers(OProviderList) ->
    case providers_topo:sort(OProviderList) of
        {ok, ProviderList} ->
            ProviderList;
        {error, _} ->
            {error, "There was a cycle in the provider list. Unable to complete build!"}
    end.

%% @doc Extract help values from a list on a per-namespace order
namespace_help(_, []) -> ok;
namespace_help(Dict, [NS|Namespaces]) ->
    Providers = case dict:find(NS, Dict) of
        {ok, Found} -> Found;
        error -> []
    end,
    Help = [case NS of
                default -> {Name, Desc};
                _ -> {"  "++Name, Desc}
            end || {Name, Desc} <- lists:sort(Providers)],
    if Help =:= [] ->
            no_public_providers;
       NS =/= default ->
            io:format("~n~p <task>:~n", [NS]),
            display_help(Help);
       NS =:= default ->
            display_help(Help)
    end,
    namespace_help(Dict, Namespaces).

display_help(Help) ->
    Longest = lists:max([length(X) || {X, _} <- Help]),
    lists:foreach(fun({Name, ShortDesc}) ->
                Length = length(Name),
                Spacing = lists:duplicate(Longest - Length + 8, " "),
                io:format("~s~s~s~n", [Name, Spacing, ShortDesc])
        end, Help).
