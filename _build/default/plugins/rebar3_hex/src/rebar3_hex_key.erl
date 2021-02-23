-module(rebar3_hex_key).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, key).
-define(DEPS, []).

-define(ENDPOINT, "keys").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex key [generate <key> | list | revoke <key> | revoke --all]"},
                                 {short_desc, "Remove or list API keys associated with your account"},
                                 {desc, ""},
                                 {opts, [
                                         {all, $a, "all", boolean, "all."},
                                         {keyname, $k, "key-name", string, "key-name"},
                                         {permission, $p, "permission", list, "perms."},
                                         rebar3_hex:repo_opt()
                                        ]
                                 }]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

% TODO: Adjust the spec when this is implemented
%-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
-spec do(rebar_state:t()) -> {'error',{'rebar3_hex_key','bad_command' | 'not_implemented'}}.
do(State) ->
    case rebar3_hex_config:repo(State) of
        {ok, Repo} ->
            handle_command(State, Repo);
        {error, Reason} ->
            ?PRV_ERROR(Reason)
    end.

handle_command(State, Repo) ->
    case rebar_state:command_args(State) of
        ["generate"| _ ] ->
             {ok, Config} = rebar3_hex_config:hex_config_write(Repo),
             {"generate", Params} = rebar3_hex:task_args(State),
            generate(State, Config, Params);
        ["fetch", KeyName] ->
             {ok, Config} = rebar3_hex_config:hex_config_read(Repo),
            fetch(State, Config, KeyName);
        ["revoke", "--all"] ->
            {ok, Config} = rebar3_hex_config:hex_config_write(Repo),
            revoke_all(State, Config);
        ["revoke", KeyName] ->
             {ok, Config} = rebar3_hex_config:hex_config_write(Repo),
            revoke(State, Config, KeyName);
        ["list"] ->
			 {ok, Config} = rebar3_hex_config:hex_config_read(Repo),
            list(State, Config);
        _ ->
            ?PRV_ERROR(bad_command)
    end.

generate(State, HexConfig, Params) ->
    Perms = gather_permissions(proplists:get_all_values(permission, Params)),
    KeyName =  proplists:get_value(keyname, Params, undefined),
    case rebar3_hex_client:key_add(HexConfig, KeyName, Perms) of
        {created, _Res} ->
            rebar3_hex_io:say("Key successfully created", []),
            {ok, State};
        Error ->
            ?PRV_ERROR({generate, Error})
    end.

fetch(State, HexConfig, KeyName) ->
    case rebar3_hex_client:key_get(HexConfig, KeyName) of
        {success, Res} ->
            ok = print_key_details(Res),
            {ok, State};
        Error ->
            ?PRV_ERROR({fetch, Error})
    end.

revoke(State, HexConfig, KeyName) ->
    case rebar3_hex_client:key_delete(HexConfig, KeyName) of
        {success, _Res} ->
             rebar3_hex_io:say("Key successfully revoked", []),
            {ok, State};
        Error ->
            ?PRV_ERROR({revoke, Error})
    end.

revoke_all(State, HexConfig) ->
    case rebar3_hex_client:key_delete_all(HexConfig) of
        {success, _Res} ->
            rebar3_hex_io:say("All keys successfully revoked", []),
            {ok, State};
        Error ->
            ?PRV_ERROR({revoke_all, Error})
    end.

list(State, HexConfig) ->
    case rebar3_hex_client:key_list(HexConfig) of
        {success, Res} ->
            ok = print_results(Res),
            {ok, State};
        Error ->
            ?PRV_ERROR({list, Error})
    end.

gather_permissions([]) ->
    [];
gather_permissions(Perms) ->
    lists:foldl(fun(Name, Acc) ->
                [Domain, Resource] = binary:split(rebar_utils:to_binary(Name), <<":">>),
                [#{<<"domain">> => Domain, <<"resource">> => Resource}] ++ Acc
                end, [], Perms).

print_results(Res) ->
    Header = ["Name", "Created"],
    Rows = lists:map(fun(#{<<"name">> := Name, <<"inserted_at">> := Created}) ->
                                [binary_to_list(Name), binary_to_list(Created)]
                     end, Res),
    ok = rebar3_hex_results:print_table([Header] ++ Rows),
    ok.
print_key_details(#{<<"name">> := Name,
                    <<"inserted_at">> := Created,
                    <<"updated_at">> := Updated,
                    <<"last_use">> := #{
                        <<"ip">> := Addr,
                        <<"used_at">> := Used,
                        <<"user_agent">> := _Agent
                       }
                   }
                 ) ->
    Header = ["Name", "Created", "Updated", "LastUsed", "LastUsedBy"],
    Row = [binary_to_list(Name),
           binary_to_list(Created),
           binary_to_list(Updated),
           binary_to_list(Used),
           binary_to_list(Addr)],
    ok = rebar3_hex_results:print_table([Header] ++ [Row]),
    ok.

-spec format_error(any()) -> iolist().
format_error({list, {unauthorized, _Res}}) ->
    "Not authorized";
format_error({revoke, {not_found, _Res}}) ->
    "Key not found";
format_error({generate, {validation_errors, #{<<"errors">> := Errors, <<"message">> := Message}}}) ->
    ErrorString = rebar3_hex_results:errors_to_string(Errors),
    io_lib:format("~ts~n\t~ts", [Message, ErrorString]);
format_error(bad_command) ->
    "Unknown command. Command must be fetch, generate, list, or revoke";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

