%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_license).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-logger_header("[dgiot]").

-export([init/1
    , description/0
    , check_lincense/3
    , gen_password/1
    , get_hardkey/0
    , check/0
    , check_lincense/0
    , check_plugin/1
    , update_license/1
    , get_license/0
    , check_parse/0
    , change_config/3
    , load_config/2
    , to_md5/1
]).

%%--------------------------------------------------------------------
%% Auth Module Callbacks
%%--------------------------------------------------------------------

init({}) ->
    {ok, #{}}.

description() -> "dgiot license".

check_lincense(Type, Key, License) ->
    case Type of
        <<"standard">> ->
            check_standard_license(Key, dgiot_utils:to_list(License));
        <<"enterprise">> ->
            check_enterprise_license(Key, dgiot_utils:to_list(License));
        <<"ultimate">> -> check_ultimate_license(Key, dgiot_utils:to_list(License));
        _ -> check_old_license(Key, dgiot_utils:to_list(License))
    end.

check_standard_license(Key, License) ->
    write("data/newsw.key", Key),
    NewLicense = gen_password(<<"dgiot_stand2020sdfsdf-", Key/binary, "-203019">>),
    License == NewLicense.

check_enterprise_license(Key, License) ->
    write("data/newsw.key", Key),
    NewLicense = gen_password(<<"dgiot_133enterprise2020sdsfff8dfsdf-", Key/binary, "-2030dd19">>),
    License == NewLicense.

check_ultimate_license(Key, License) ->
    write("data/newsw.key", Key),
    NewLicense = gen_password(<<"dgiot_133ultimate206420sd32sfff8dfsdf-", Key/binary, "-2030dd19">>),
    License == NewLicense.

check_old_license(Key, License) ->
    write("data/sw.key", Key),
    Hash = <<"lorasouth2018test-", Key/binary, "-202019">>,
    MyPassword = gen_password(Hash),
    License == MyPassword.

check_lincense() ->
    check().

check() ->
    case (dgiot_datetime:now_secs() - 1639622310) < 0 of
        true -> true;
        false ->
            case application:get_env(dgiot_license, license) of
                {ok, License} ->
                    OldKey = get_key(),
                    NewKey = get_hardkey(),
                    case check_old_license(OldKey, License) or check_standard_license(NewKey, License) or
                        check_enterprise_license(NewKey, License) or check_ultimate_license(NewKey, License)
                    of
                        false ->
                            false;
                        true ->
                            case os:type() of
                                {win32, _} -> true;
                                _ ->
                                    case check_sudo() of
                                        not_root_user ->
                                            false;
                                        root_user ->
                                            true
                                    end
                            end
                    end;
                _ ->
                    false
            end
    end.

gen_password(License) ->
    binary_to_list(to_md5(License)).

to_md5(V) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(V))])).

get_key() ->
    S = lists:foldl(fun(Cmd, Acc) ->
        [os:cmd(Cmd) | Acc]
                    end, [], ["dmidecode |grep 'Serial Number'", "dmidecode -t processor"]),
    to_md5(lists:concat([S])).

get_hardkey() ->
    Sn = get_sn(),
    Processor = get_processor(),
    <<Sn/binary, Processor/binary>>.

get_sn() ->
    S = case os:type() of
            {win32, _} ->
                lists:foldl(fun(Cmd, Acc) ->
                    [os:cmd(Cmd) | Acc]
                            end, [], ["wmic diskdrive get serialnumber"]);
            _ ->
                lists:foldl(fun(Cmd, Acc) ->
                    [os:cmd(Cmd) | Acc]
                            end, [], ["dmidecode | grep  'Serial Number'"])
        end,
%%    ?LOG(info,"S ~p", [S]),
    to_md5(lists:concat([S])).

get_processor() ->
    S = case os:type() of
            {win32, _} ->
                lists:foldl(fun(Cmd, Acc) ->
                    [os:cmd(Cmd) | Acc]
                            end, [], ["wmic cpu get ProcessorId"]);
            _ ->
                lists:foldl(fun(Cmd, Acc) ->
                    [os:cmd(Cmd) | Acc]
                            end, [], ["dmidecode -t processor"])
        end,
%%    ?LOG(info,"S ~p", [S]),
    to_md5(lists:concat([S])).

write(Path, S) ->
    write(Path, S, []).
write(Path, Format, Args) ->
    case filelib:ensure_dir(Path) of
        {error, Reason} ->
            io:format("write file error, ~p~n", [Reason]);
        ok ->
            case file:open(Path, [write]) of
                {ok, IoDevice} ->
                    io:fwrite(IoDevice, Format, Args),
                    file:close(IoDevice);
                {error, Why} ->
                    ?LOG(error,"write file error, ~p~n", [Why])
            end
    end.

check_sudo() ->
    NewString = string:to_lower(os:cmd("dmidecode")),
    case re:run(NewString, "permission denied") of
        {match, _} ->
            io:format("This software must be run with root privileges ~n"),
            not_root_user;
        _ -> root_user
    end.

check_plugin(Name) ->
    BinName = dgiot_utils:to_binary(Name),
    case BinName of
        <<"dgiot_apihub">> ->
            true;
        <<"dgiot_", _/binary>> ->
%%            dgiot_license:check();
            true;
        _ ->
            true
    end.

get_license() ->
    case application:get_env(dgiot_license, license) of
        {ok, License} -> License;
        _ -> <<"">>
    end.

update_license(#{<<"license">> := License, <<"addr">> := Addr}) ->
    dgiot_license:change_config(dgiot_parse, parse_server, "http://" ++ dgiot_utils:to_list(Addr) ++ ":1337"),
    application:set_env(dgiot_parse, parse_server, "http://" ++ dgiot_utils:to_list(Addr) ++ ":1337"),
    dgiot_license:change_config(dgiot, license, dgiot_utils:to_list(License)),
    application:set_env(dgiot, license, dgiot_utils:to_list(License)).

-record(mapping, {
    variable :: cuttlefish_variable:variable(),
    mapping :: string(),
    default :: term(),
    commented :: term(),
    datatype = [string] :: cuttlefish_datatypes:datatype_list(),
    level = basic :: basic | intermediate | advanced,
    doc = [] :: list(),
    include_default = undefined :: string() | undefined,
    new_conf_value = undefined :: string() | undefined,
    validators = [] :: [string()],
    is_merge = false :: boolean(),
    see = [] :: [cuttlefish_variable:variable()],
    hidden = false :: boolean()
}).

change_config(App, K, V) ->
    Schema = cuttlefish_schema:files([filename:join([code:priv_dir(App), App]) ++ ".schema"]),
    Conf = cuttlefish_conf:file(filename:join([dgiot:get_env(plugins_etc_dir), App]) ++ ".conf"),
    Configs = cuttlefish_generator:map(Schema, Conf),
    {_, Mappings, _} = Schema,
    Map = maps:from_list(proplists:get_value(App, Configs, [])),
    DefaultList = maps:to_list(maps:merge(Map, #{dgiot_utils:to_atom(K) => V})),
    ?LOG(info,"Mappings ~p ", [Mappings]),
    NewMappings =
        lists:foldl(fun(X, Acc) ->
            case X#mapping.variable of
                [_A, B] ->
                    case proplists:get_value(dgiot_utils:to_atom(B), DefaultList) of
                        undefined -> Acc ++ [X];
                        Default -> Acc ++ [X#mapping{default = Default}]
                    end;
                _ -> Acc ++ [X]
            end
                    end, [], Mappings),
    ?LOG(info,"NewMappings ~p ", [NewMappings]),
    cuttlefish_conf:generate_file(NewMappings,
        filename:join([dgiot:get_env(plugins_etc_dir), App]) ++ ".conf").

check_parse() ->
    dgiot_data:init(check_parse_health),
    timer:sleep(100),
    dgiot_data:insert(check_parse_health, parse_health, false),
    spawn(
        fun() ->
            process_flag(trap_exit, true),
            case dgiot_parse:health() of
                {ok, #{<<"status">> := <<"ok">>}} ->
                    dgiot_data:insert(check_parse_health, parse_health, true);
                _ -> dgiot_data:insert(check_parse_health, parse_health, false)
            end
        end),
    timer:sleep(1000),
    case dgiot_data:get(check_parse_health, parse_health) of
        false ->
            false;
        true ->
            true
    end.

load_config(Module, FileName) ->
    {file, Here} = code:is_loaded(Module),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/"]),
    Name = dgiot_utils:to_list(FileName),
    case filename:extension(Name) of
        [] ->
            {ok, Bin} = file:read_file(Dir ++ Name ++ ".json"),
            jsx:decode(Bin, [{labels, binary}, return_maps]);
        _ ->
            {ok, Bin} = file:read_file(Dir ++ Name),
            jsx:decode(Bin, [{labels, binary}, return_maps])
    end.
