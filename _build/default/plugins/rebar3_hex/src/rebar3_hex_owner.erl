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
                                 {short_desc, "Add, remove, transfer or list package owners"},
                                 {desc, support()},
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
                    case rebar3_hex_config:hex_config_write(Repo) of
                        {ok, Config}  ->
                            {ok, State} = add(Config, Package, UsernameOrEmail, Level, Transfer, State),
                            ok = rebar3_hex_io:say("Added ~ts to ~ts", [UsernameOrEmail, Package]),
                            {ok, State};
                        Err ->
                            ?PRV_ERROR(Err)
                    end;
                false ->
                    {error, "level must be one of full or maintainer"}
            end;
        {"remove", Package, UsernameOrEmail} ->
            case rebar3_hex_config:hex_config_write(Repo) of
                {ok, Config} ->
                    {ok, State} = remove(Config, Package, UsernameOrEmail, State),
                    ok = rebar3_hex_io:say("Removed ~ts to ~ts", [UsernameOrEmail, Package]),
                    {ok, State};
                Err ->
                    ?PRV_ERROR(Err)
            end;
        {"transfer", Package, UsernameOrEmail} ->
            case  rebar3_hex_config:hex_config_write(Repo) of
                {ok, Config} ->
                    {ok, State} = add(Config, Package, UsernameOrEmail, <<"full">>, true, State),
                    ok = rebar3_hex_io:say("Transfered ~ts to ~ts", [Package, UsernameOrEmail]),
                    {ok, State};
                Err ->
                    ?PRV_ERROR(Err)
            end;
        {"list", Package} ->
            case rebar3_hex_config:hex_config_read(Repo) of
                {ok, Config} ->
                    list(Config, Package, State);
                Err ->
                    ?PRV_ERROR(Err)
            end;
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
get_args(["list", Package| _Rest]) ->
    {"list", Package};
get_args([Task, Package, Username]) when Task =:= "transfer" ->
    {Task, Package, Username};
get_args([Task, Package, UserName | _Rest]) when Task =:= "add" orelse Task =:= "remove" ->
    {Task, Package, UserName};
get_args([Task, Package, UserName, "-r", _]) ->
    {Task, Package, UserName};
get_args(BadCommand) ->
    BadCommand.

support() ->
    "Adds, removes or lists package owners.~n~n"
    "Package owners have full permissions to the package. They can "
    "publish and revert releases and even remove other package owners.~n~n"
    "Supported commmand combinations: ~n~n"
    "  rebar3 hex owner add <package> <username>~n~n"
    "  rebar3 hex owner add <package> <username> <level>~n~n"
    "  rebar3 hex owner add <package> <username> <level> <transfer>~n~n"
    "  rebar3 hex owner list <package>~n~n"
    "  rebar3 hex owner remove <package> <username>~n~n"
    "  rebar3 hex owner transfer <package> <username>~n~n"
    "Argument descriptions: ~n ~n"
    "  <username> - a valid hex username or email address for a hex user~n~n"
    "  <package>  - a valid hex package name~n~n"
    "  <level>    - one of full or maintainer~n~n"
    "  <transfer> - boolean value indicating whether to transfer a specified package or not~n~n".

-spec format_error(any()) -> iolist().
format_error(bad_command) ->
    S = "Invalid command ~n~n",
    support(),
    io_lib:format(S, []);
format_error({validation_errors, Cmd, Package, User, Errors, Message}) ->
    ErrorString = rebar3_hex_results:errors_to_string(Errors),
    Action = verb_to_gerund(Cmd),
    io_lib:format("Error ~ts ~ts as owner of package ~ts : ~ts~n\t~ts", [Action, User, Package, Message, ErrorString]);
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
		{ok, {422, _Headers, #{<<"errors">> := Errors, <<"message">> := Message}}} ->
            erlang:error(?PRV_ERROR({validation_errors, add, Package, UsernameOrEmail, Errors, Message}));
        {ok, {Status, _Headers, _Body}} ->
            erlang:error(?PRV_ERROR({status, Status, Package, UsernameOrEmail}));
        {error, Reason} ->
            erlang:error(?PRV_ERROR({error, Package, UsernameOrEmail, Reason}))
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
            Owners = [owner(Owner) || Owner <- List],
            OwnersString = rebar_string:join(Owners, "\n"),
            rebar3_hex_io:say("~s", [OwnersString]),
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({status, Status, Package});
        {error, Reason} ->
            ?PRV_ERROR({error, Package, Reason})
    end.

owner(Owner) ->
    Name0 = maps:get(<<"username">>, Owner, nil),
    Email0 = maps:get(<<"email">>, Owner, nil),
    {Name, Email} = case {Name0, Email0} of
                        _ when is_binary(Name0), is_binary(Email0) ->
                            {Name0, Email0};
                        _ when is_binary(Name0) ->
                            {Name0, <<"unspecified">>};
                        _ when is_binary(Email0) ->
                            {<<"unspecified">>, Email0};
                        _ ->
                            {<<"unspecified">>, <<"unspecified">>}
                    end,
    binary_to_list(Name) ++ " (" ++ binary_to_list(Email) ++ ")".

verb_to_gerund(add) -> "adding";
verb_to_gerund(remove) -> "removing";
verb_to_gerund(list) -> "listing".
