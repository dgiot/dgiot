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

-module(dgiot_parse_log).
-author("kenneth").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(DEFField, re:split(application:get_env(?MODULE, delete_field, ""), ",")).

%% API
-export([
    send/2,
    load_LogLevel/0,
    log/2,
    log/1
]).


send(Meta, Payload) when is_list(Payload) ->
    send(Meta, iolist_to_binary(Payload));

send(#{error_logger := _Error_logger, mfa := {M, F, A}} = _Meta, Payload) ->
    Mfa = <<(atom_to_binary(M, utf8))/binary, $/, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>,
    Topic = <<"logger_trace/error/", Mfa/binary>>,
    dgiot_mqtt:publish(Mfa, Topic, Payload),
    Map = jiffy:decode(Payload, [return_maps]),
    NewMap = maps:with([<<"domain">>, <<"time">>, <<"pid">>, <<"msg">>, <<"mfa">>, <<"line">>, <<"level">>, <<"clientid">>, <<"topic">>, <<"peername">>], Map),
    dgiot_parse_cache:save_to_cache(#{<<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Log">>,
        <<"body">> => get_body(NewMap#{<<"time">> =>  dgiot_datetime:now_ms()})});

send(#{mfa := _MFA} = Meta, Payload) ->
    Map = jiffy:decode(Payload, [return_maps]),
    Mfa = dgiot_utils:to_binary(maps:get(<<"mfa">>, Map, <<"all">>)),
    TraceTopic =
        case maps:find(topic, Meta) of
            {ok, TraceTopic1} ->
                BinTraceTopic = dgiot_utils:to_binary(TraceTopic1),
                <<"/", BinTraceTopic/binary>>;
            _ -> <<"">>
        end,
    Topic =
        case maps:find(clientid, Meta) of
            {ok, ClientId1} ->
                BinClientId = dgiot_utils:to_binary(ClientId1),
                <<"logger_trace/trace/", BinClientId/binary, TraceTopic/binary>>;
            _ ->
                Line =
                    case maps:find(<<"line">>, Map) of
                        {ok, Line1} ->
                            dgiot_utils:to_binary(Line1);
                        _ ->
                            <<"0">>
                    end,
                <<"logger_trace/log/", Mfa/binary, "/", Line/binary>>
        end,
    dgiot_mqtt:publish(Mfa, Topic, Payload),
    NewMap = maps:with([<<"domain">>, <<"time">>, <<"pid">>, <<"msg">>, <<"mfa">>, <<"line">>, <<"level">>, <<"clientid">>, <<"topic">>, <<"peername">>], Map),
    dgiot_parse_cache:save_to_cache(#{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Log">>,
        <<"body">> => get_body(NewMap#{<<"time">> =>  dgiot_datetime:now_ms()})});

send(_Meta, Payload) ->
    dgiot_mqtt:publish(<<"logger_trace_other">>, <<"logger_trace/other">>, Payload),
    ok.

get_body(#{<<"msg">> := Msg, <<"clientid">> := _} = Map) when is_map(Msg) ->
    DefaultACL = #{<<"role:admin">> => #{
        <<"read">> => true,
        <<"write">> => true}
    },
    ACl = maps:get(<<"ACL">>, Msg, DefaultACL),
    NewMsg = maps:without([<<"ACL">>], Msg),
    Map#{<<"type">> => <<"json">>, <<"ACL">> => ACl, <<"msg">> => jiffy:encode(NewMsg)};
get_body(#{<<"msg">> := Msg} = Map) when is_map(Msg) ->
    Devaddr = maps:get(<<"devaddr">>, Msg, <<"">>),
    ProductId = maps:get(<<"productid">>, Msg, <<"">>),
    DeviceId = maps:get(<<"deviceid">>, Msg, <<"">>),
    DefaultACL = dgiot_device:get_acl(DeviceId),
    ACl = maps:get(<<"ACL">>, Msg, DefaultACL),
    NewMsg = maps:without([<<"ACL">>], Msg),
    Map#{<<"type">> => <<"json">>, <<"devaddr">> => Devaddr, <<"productid">> => ProductId, <<"deviceid">> => DeviceId, <<"ACL">> => ACl, <<"msg">> => jiffy:encode(NewMsg)};
get_body(Map) ->
    Map#{<<"type">> => <<"text">>,
        <<"ACL">> => #{<<"role:admin">> => #{
            <<"read">> => true,
            <<"write">> => true}
        }}.


log(Method, {Url, Header}) ->
    IsLog = application:get_env(dgiot_parse, log, false),
    IsLog andalso ?LOG(info, "~s ~s ~p", [dgiot_parse_rest:method(Method), Url, Header]);
log(Method, {Url, Header, _, Body}) ->
    IsLog = application:get_env(dgiot_parse, log, false),
    IsLog andalso ?LOG(info, "~s ~s Header:~p  Body:~p", [dgiot_parse_rest:method(Method), Url, Header, Body]).

load_LogLevel() ->
    Level = emqx_logger:get_primary_log_level(),
    case create_logconfig(Level, <<"0">>, <<"dgiot">>, <<"system">>, 0, <<"$dgiot/log/#">>) of
        {ok, #{<<"objectId">> := DgiotlogId}} ->
            create_logconfig(Level, DgiotlogId, <<"dgiot_handle">>, <<"dgiot_handle">>, 2, <<"$dgiot/trace/#">>),
            case create_logconfig(Level, DgiotlogId, <<"dgiot_app">>, <<"dgiot_app">>, 1, <<"$dgiot/log/#">>) of
                {ok, #{<<"objectId">> := ApplogId}} ->
                    create_applog(ApplogId);
                _ ->
                    pass
            end;
        _Ot ->
            pass
    end.

create_applog(DgiotlogId) ->
    Apps = application:loaded_applications(),
    lists:foldl(fun({Appname, _, _}, Acc) ->
        BinAppname = atom_to_binary(Appname),
        case BinAppname of
            <<"dgiot_", _/binary>> ->
                case create_logconfig(<<"info">>, DgiotlogId, BinAppname, <<"app">>, Acc, <<"$dgiot/log/", BinAppname/binary, "/#">>) of
                    {ok, #{<<"objectId">> := ApplogId}} ->
                        AppPath = code:lib_dir(Appname) ++ "/ebin",
                        case file:list_dir_all(AppPath) of
                            {ok, Modules} ->
                                lists:foldl(fun(Mod, Mods) ->
                                    BinMod = dgiot_utils:to_binary(Mod),
                                    case binary:split(BinMod, <<$.>>, [global, trim]) of
                                        [Module, <<"beam">>] ->
                                            AtomMod = binary_to_atom(Module),
                                            Modlevel =
                                                case logger:get_module_level(AtomMod) of
                                                    [{AtomMod, Level} | _] ->
                                                        Level;
                                                    _ ->
                                                        <<"debug">>
                                                end,
                                            case create_logconfig(Modlevel, ApplogId, Module, <<"module">>, Mods, <<"$dgiot/log/", Module/binary, "/#">>) of
                                                {ok, #{<<"objectId">> := ModlogId}} ->
                                                    Functions = AtomMod:module_info(exports),
                                                    lists:foldl(fun({Fun, Num}, Funs) ->
                                                        BinFun = dgiot_utils:to_binary(Fun),
                                                        BinNum = dgiot_utils:to_binary(Num),
                                                        create_logconfig(Modlevel, ModlogId, <<BinFun/binary, "/", BinNum/binary>>, <<"function">>, Funs, <<"$dgiot/log/", Module/binary, "/", BinFun/binary, "/", BinNum/binary, "/#">>),
                                                        Funs + 1
                                                                end, 1, Functions);
                                                _ ->
                                                    Mods
                                            end,
                                            Mods + 1;
                                        _ ->
                                            Mods
                                    end
                                            end, 1, Modules);
                            _Ot ->
                                ?LOG(info, "_Ot ~p", [_Ot]),
                                Acc
                        end;
                    _ ->
                        Acc
                end,
                Acc + 1;
            _ ->
                Acc
        end
                end, 1, Apps).

create_logconfig(Level, Parent, Name, Type, Order, Topic) ->
    create_loglevel(#{
        <<"level">> => Level,
        <<"parent">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"LogLevel">>,
            <<"objectId">> => Parent
        },
        <<"name">> => Name,
        <<"type">> => Type,
        <<"order">> => Order,
        <<"topic">> => Topic
    }).

create_loglevel(LogLevel) ->
    Name1 = maps:get(<<"name">>, LogLevel),
    Type1 = maps:get(<<"type">>, LogLevel),
    LoglevelId = dgiot_parse_id:get_loglevelid(Name1, Type1),
    case dgiot_parse:get_object(<<"LogLevel">>, LoglevelId) of
        {ok, #{<<"objectId">> := LoglevelId, <<"type">> := Type, <<"name">> := Name, <<"level">> := Level}} ->
            dgiot_logger:set_loglevel(Type, Name, Level),
            {ok, #{<<"objectId">> => LoglevelId}};
        _ ->
            dgiot_parse:create_object(<<"LogLevel">>, LogLevel)
    end.

log(Map) ->
%%    io:format("~p ~n",[Map]),
%%    io:format("~p ~n",[jiffy:encode(Map)]),
    Map.
