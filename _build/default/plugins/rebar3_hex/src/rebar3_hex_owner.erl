-module(rebar3_hex_owner).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, owner).
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
                                 {example, "rebar3 hex owner"},
                                 {short_desc, "Add, remove or list package owners"},
                                 {desc, ""},
                                 {opts, [rebar3_hex:repo_opt(),
                                         {level, $l, "level", {string, "full"}, "Ownership level."},
                                         {transfer, $t, "transfer", {boolean, false}, "Transfer Package"}
                                        ]}]),

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
    case command_args(State) of
        {"add", Package, UsernameOrEmail, Level, Transfer} ->
            case valid_level(Level) of
                true ->
                    {ok, Config} = rebar3_hex_config:hex_config_write(Repo),
                    {ok, State} = add(Config, Package, UsernameOrEmail, Level, Transfer, State),
                    ok = rebar3_hex_io:say("Added ~ts to ~ts", [UsernameOrEmail, Package]),
                    {ok, State};
                false ->
                    {error, "level must be one of full or maintainer"}
            end;
        {"remove", Package, UsernameOrEmail} ->
            {ok, Config} = rebar3_hex_config:hex_config_write(Repo),
            {ok, State} = remove(Config, Package, UsernameOrEmail, State),
            ok = rebar3_hex_io:say("Removed ~ts to ~ts", [UsernameOrEmail, Package]),
            {ok, State};
        {"transfer", Package, UsernameOrEmail} ->
            {ok, Config} = rebar3_hex_config:hex_config_write(Repo),
            {ok, State} = add(Config, Package, UsernameOrEmail, <<"full">>, true, State),
            ok = rebar3_hex_io:say("Transfered ~ts to ~ts", [UsernameOrEmail, Package]),
            {ok, State};
        {"list", Package} ->
            {ok, Config} = rebar3_hex_config:hex_config_read(Repo),
            {ok, State} = list(Config, Package, State),
            {ok, State};
        _Command ->
            ?PRV_ERROR(bad_command)
    end.

command_args(State) ->
    case get_args(rebar_state:command_args(State)) of
        {"list", Package} ->
            {"list", rebar_utils:to_binary(Package)};

        {"add", Package, UserOrEmail} ->
            {AllArgs, _} = rebar_state:command_parsed_args(State),
            Level = proplists:get_value(level, AllArgs, "full"),
            Transfer = proplists:get_value(transfer, AllArgs, false),
            {"add", rebar_utils:to_binary(Package), rebar_utils:to_binary(UserOrEmail), rebar_utils:to_binary(Level), Transfer};

        {Command, Package, UserOrEmail} ->
            {Command, rebar_utils:to_binary(Package), rebar_utils:to_binary(UserOrEmail)};

        BadCommand ->
          BadCommand
     end.

get_args(["list", Package]) ->
    {"list", Package};
get_args([Task, Package, UserName | _Rest]) when Task =:= "add" orelse Task =:= "remove" ->
    {Task, Package, UserName};
get_args([Task, Package, UserName, "-r", _]) ->
    {Task, Package, UserName};
get_args(BadCommand) ->
    BadCommand.

-spec format_error(any()) -> iolist().
format_error(bad_command) ->
    "Command must be one of add, remove or list";
format_error({error, Package, Reason}) ->
    io_lib:format("Error listing owners of package ~ts: ~p", [Package, Reason]);
format_error({status, Status, Package}) ->
    io_lib:format("Error listing owners of package ~ts: ~ts",
                  [Package, rebar3_hex_client:pretty_print_status(Status)]);
format_error({error, Package, UsernameOrEmail, Reason}) ->
    io_lib:format("Error adding ~ts as owner of package ~ts: ~p", [UsernameOrEmail, Package, Reason]);
format_error({status, Status, Package, UsernameOrEmail}) ->
    io_lib:format("Error adding ~ts as owner of package ~ts: ~ts",
                  [UsernameOrEmail, Package, rebar3_hex_client:pretty_print_status(Status)]);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

valid_level(<<"full">>) -> true;
valid_level(<<"maintainer">>) -> true;
valid_level(_) -> false.

add(HexConfig, Package, UsernameOrEmail, Level, Transfer, State) ->
    case hex_api_package_owner:add(HexConfig, Package, UsernameOrEmail, Level, Transfer) of
        {ok, {Code, _Headers, _Body}} when Code =:= 204 orelse Code =:= 201->
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({Status, Package, UsernameOrEmail});
        {error, Reason} ->
            ?PRV_ERROR({error, Package, UsernameOrEmail, Reason})
    end.

remove(HexConfig, Package, UsernameOrEmail, State) ->
    case hex_api_package_owner:delete(HexConfig, Package, UsernameOrEmail) of
        {ok, {204, _Headers, _Body}} ->
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({status, Status, Package, UsernameOrEmail});
        {error, Reason} ->
            ?PRV_ERROR({error, Package, UsernameOrEmail, Reason})
    end.

list(HexConfig, Package, State) ->
    case hex_api_package_owner:list(HexConfig, Package) of
        {ok, {200, _Headers, List}} ->
            Owners = [binary_to_list(maps:get(<<"email">>, Owner, <<"">>)) || Owner <- List],
            OwnersString = rebar_string:join(Owners, "\n"),
            rebar3_hex_io:say("~s", [OwnersString]),
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({status, Status, Package});
        {error, Reason} ->
            ?PRV_ERROR({error, Package, Reason})
    end.
