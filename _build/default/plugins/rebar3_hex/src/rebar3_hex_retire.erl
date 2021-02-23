%% @doc This provider allows the user to delete a package within one
%% hour of its publication.
%% @end
-module(rebar3_hex_retire).

-export([init/1,
         do/1,
         format_error/1]).

-export([retire/6]).

-include("rebar3_hex.hrl").

-define(PROVIDER, retire).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex retire some_pkg 0.3.0"},
                                 {short_desc, "Mark a package as deprecated."},
                                 {desc, ""},
                                 {opts, [{pkg, undefined, undefined, string, "Name of the package to retire."},
                                         {vsn, undefined, undefined, string, "Version of the package to retire."},
                                         {reason, undefined, undefined, string, "Reason to retire package."},
                                         {message, undefined, undefined, string, "Clarifying message for retirement"},
                                         rebar3_hex:repo_opt()]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar3_hex_config:repo(State) of
        {ok, Repo} ->
            handle_command(State, Repo);
        {error, Reason} ->
            ?PRV_ERROR(Reason)
    end.

handle_command(State, Repo) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Name = rebar3_hex:get_required(pkg, Args),
    PkgName = rebar_utils:to_binary(Name),
    Version = rebar3_hex:get_required(vsn, Args),
    Reason = rebar3_hex:get_required(reason, Args),
    Message = rebar3_hex:get_required(message, Args),
    retire(PkgName, rebar_utils:to_binary(Version), Repo,
           rebar_utils:to_binary(Reason),
           rebar_utils:to_binary(Message),
           State).

format_error({api_error, PkgName, Version, Reason}) ->
    io_lib:format("Unable to delete package ~ts ~ts: ~ts", [PkgName, Version, Reason]);
format_error({required, pkg}) ->
    "retire requires a package name argument to identify the package to delete";
format_error({required, vsn}) ->
    "retire requires a version number argument to identify the package to delete";
format_error({required, reason}) ->
    "retire requires a reason with value of either other, invalid, security, deprecated or renamed";
format_error({required, message}) ->
    "retire requires a message to clarify the reason for the retirement of the package";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

retire(PkgName, Version, Repo, Reason, Message, State) ->
    case rebar3_hex_config:hex_config_write(Repo) of
        {error, no_write_key} ->
            ?PRV_ERROR({no_write_key, maps:get(name, Repo)});

        {ok, HexConfig} ->

            Body = #{<<"reason">> => Reason,
                     <<"message">> => Message},

            case hex_api_release:retire(HexConfig, PkgName, Version, Body) of
                {ok, {Code, _Headers, _Body}} when Code =:= 204 ->
                    rebar_api:info("Successfully retired package ~ts ~ts", [PkgName, Version]),
                    {ok, State};
                {ok, {Code, _Headers, _Body}} ->
                    ?PRV_ERROR({api_error, PkgName, Version, rebar3_hex_client:pretty_print_status(Code)});
                {error, Reason} ->
                    ?PRV_ERROR({api_error, PkgName, Version, io_lib:format("~p", [Reason])})
            end
    end.
