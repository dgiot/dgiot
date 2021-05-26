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

-module(dgiot_parse).
-author("kenneth").
-include_lib("eunit/include/eunit.hrl").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(DEFField, re:split(application:get_env(?MODULE, delete_field, ""), ",")).


%% API
-export([
    login/2,
    get_role/2,
    add_to_role/4,
    create_session/3,
    check_session/1
]).
-export([
    health/0,
    health/1,
    init_database/2,
    create_object/2,
    create_object/4,
    get_object/2,
    get_object/4,
    update_object/3,
    update_object/5,
    del_object/2,
    del_object/4,
    del_table/1,
    create_schemas/1,
    update_schemas/1,
    del_schemas/1,
    get_schemas/1,
    get_schemas/0,
    set_class_level/2,
    query_object/2,
    query_object/3,
    query_object/4,
    aggregate_object/2,
    aggregate_object/4,
    add_trigger/3,
    del_trigger/2,
    del_trigger/1,
    del_trigger/0,
    update_trigger/3,
    get_trigger/0,
    get_trigger/2,
    add_all_trigger/1,
    read_page/4,
    read_page/5,
    format_data/2,
    batch/1,
    batch/3,
    batch/4,
    import/5,
    import/6,
    request/4,
    request/5,
    graphql/1
]).

-export([
    test_graphql/0,
    get_objectid/2,
    get_deviceid/2,
    get_dictid/2,
    get_shapeid/2,
    subscribe/2,
    send_msg/3
]).

subscribe(Table, Method) ->
    case dgiot_data:get({sub, Table, Method}) of
        not_find ->
            dgiot_data:insert({sub, Table, Method}, [self()]);
        Acc ->
            dgiot_data:insert({sub, Table, Method}, dgiot_utils:unique_2(Acc ++ [self()]))
    end,
    Fun = fun(Args) ->
            case Args of
                [_, Data, _Body] ->
                    dgiot_parse:send_msg(Table, Method, Data),
                    {ok, Data};
                [_, _ObjectId, Data, _Body] ->
                    dgiot_parse:send_msg(Table, Method, Data),
                    {ok,Data};
                _ ->
                    {ok, []}
            end
        end,
    dgiot_hook:add(one_for_one, {Table, Method}, Fun).

send_msg(Table, Method,Args) ->
    Pids = lists:foldl(fun(Pid, Acc) ->
        case is_process_alive(Pid) of
            true ->
                Pid ! {sync_parse, Args},
                Acc ++ [Pid];
            false ->
                Acc
       end
                       end, [], dgiot_data:get({sub, Table, Method})),
     dgiot_data:insert({sub, Table, Method}, Pids).

get_shapeid(DeviceId, Identifier) ->
    <<ShapeId:10/binary, _/binary>> = dgiot_utils:to_md5(<<DeviceId/binary, Identifier/binary, "dgiottopo">>),
    ShapeId.

get_dictid(Key, Type) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Dict">>, #{<<"key">> => Key, <<"type">> => Type}),
    DeviceId.

get_deviceid(ProductId, DevAddr) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
    DeviceId.

get_objectid(Class, Map) ->
    case Class of
        <<"post_classes_product">> ->
            get_objectid(<<"Product">>, Map);
        <<"Product">> ->
            DevType = maps:get(<<"devType">>, Map, <<"">>),
            Category = maps:get(<<"category">>, Map, <<"">>),
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<Pid:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Product", Category/binary, DevType/binary, Name/binary>>),
            Map#{
                <<"objectId">> => Pid
            };
        <<"post_classes_device">> ->
            get_objectid(<<"Device">>, Map);
        <<"Device">> ->
            Product = case maps:get(<<"product">>, Map) of
                          #{<<"objectId">> := ProductId} -> ProductId;
                          ProductId1 -> ProductId1
                      end,
            DevAddr = maps:get(<<"devaddr">>, Map, <<"">>),
            <<Did:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Device", Product/binary, DevAddr/binary>>),
            Map#{
                <<"objectId">> => Did
            };
        <<"post_classes_evidence">> ->
            get_objectid(<<"Evidence">>, Map);
        <<"Evidence">> ->
            Ukey = maps:get(<<"ukey">>, Map, <<"">>),
            TimeStamp = dgiot_utils:to_binary(maps:get(<<"timestamp">>, Map, <<"">>)),
            <<EId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Evidence", Ukey/binary, TimeStamp/binary>>),
            Map#{
                <<"objectId">> => EId
            };
        <<"post_classes_channel">> ->
            get_objectid(<<"Channel">>, Map);
        <<"Channel">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            Type = maps:get(<<"type">>, Map, <<"">>),
            CType = maps:get(<<"cType">>, Map, <<"">>),
            <<CId:10/binary, _/binary>> = dgiot_license:to_md5(<<"Channel", Type/binary, CType/binary, Name/binary>>),
            Map#{
                <<"objectId">> => CId
            };
        <<"post_classes_dict">> ->
            get_objectid(<<"Dict">>, Map);
        <<"Dict">> ->
            Key = maps:get(<<"key">>, Map, <<"">>),
            Type = maps:get(<<"type">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Dict", Key/binary, Type/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"post_classes_instruct">> ->
            get_objectid(<<"Instruct">>, Map);
        <<"Instruct">> ->
            #{<<"objectId">> := DeviceId} = maps:get(<<"device">>, Map, #{<<"objectId">> => <<"">>}),
            Pn = maps:get(<<"pn">>, Map, <<"">>),
            Di = maps:get(<<"di">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Instruct", DeviceId/binary, Pn/binary, Di/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"post_classes_menu">> ->
            get_objectid(<<"Menu">>, Map);
        <<"Menu">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            Url = maps:get(<<"url">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Menu", Name/binary, Url/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"post_classes_permission">> ->
            get_objectid(<<"Permission">>, Map);
        <<"Permission">> ->
            Name = maps:get(<<"name">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Permission", Name/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        <<"post_classes_crond">> ->
            get_objectid(<<"Crond">>, Map);
        <<"Crond">> ->
            Name = maps:get(<<"tid">>, Map, <<"">>),
            <<DId:10/binary, _/binary>> = dgiot_utils:to_md5(<<"Crond", Name/binary>>),
            Map#{
                <<"objectId">> => DId
            };
        _ ->
            Map
    end.


health() ->
    health(?DEFAULT).
health(Name) ->
    Path = <<"/health">>,
    request_rest(Name, 'GET', [], Path, #{}, [{from, rest}]).

%% 登录
login(UserName, Password) ->
    login(?DEFAULT, UserName, Password).
login(Name, UserName, Password) ->
    Path = <<"/login">>,
    Args = #{<<"username">> => UserName, <<"password">> => Password},
    request_rest(Name, 'GET', [], Path, Args, [{from, rest}]).


%% 创建对象
create_object(Class, Map) ->
    create_object(?DEFAULT, Class, Map).
create_object(Name, Class, Map) ->
    create_object(Name, Class, Map, [], [{from, master}]).
create_object(Class, Map, Header, Options) ->
    create_object(?DEFAULT, Class, Map, Header, Options).
create_object(Name, Class, #{<<"objectId">> := _ObjectId} = Map, Header, Options) ->
    Path = <<"/classes/", Class/binary>>,
    request_rest(Name, 'POST', Header, Path, Map, Options);
create_object(Name, Class, Map, Header, Options) ->
    Path = <<"/classes/", Class/binary>>,
    request_rest(Name, 'POST', Header, Path, get_objectid(Class, Map), Options).


%% 获取对象
get_object(Class, ObjectId) ->
    get_object(?DEFAULT, Class, ObjectId).
get_object(Name, Class, ObjectId) ->
    get_object(Name, Class, ObjectId, [], [{from, master}]).
get_object(Class, ObjectId, Header, Options) ->
    get_object(?DEFAULT, Class, ObjectId, Header, Options).
get_object(Name, Class, ObjectId, Header, Options) ->
    Path = <<"/classes/", Class/binary, "/", ObjectId/binary>>,
    request_rest(Name, 'GET', Header, Path, #{}, Options).


%% 更新对象
update_object(Class, ObjectId, Map) ->
    update_object(?DEFAULT, Class, ObjectId, Map).
update_object(Name, Class, ObjectId, Map) ->
    update_object(Name, Class, ObjectId, Map, [], [{from, master}]).
update_object(Class, ObjectId, Map, Header, Options) ->
    update_object(?DEFAULT, Class, ObjectId, Map, Header, Options).
update_object(Name, Class, ObjectId, Map, Header, Options) ->
    Path = <<"/classes/", Class/binary, "/", ObjectId/binary>>,
    request_rest(Name, 'PUT', Header, Path, Map, Options).


%% 批处理
batch(Requests) ->
    batch(?DEFAULT, Requests).
batch(Name, Requests) ->
    batch(Name, Requests, [], [{from, master}]).
batch(Requests, Header, Opts) ->
    batch(?DEFAULT, Requests, Header, Opts).
batch(Name, Requests, Header, Opts) ->
    request_rest(Name, 'POST', Header, <<"/batch">>, #{<<"requests">> => Requests}, Opts).


%% 创建表结构
create_schemas(Fields) ->
    create_schemas(?DEFAULT, Fields).
create_schemas(Name, #{<<"className">> := Class} = Fields) ->
    Path = <<"/schemas/", Class/binary>>,
    request_rest(Name, 'POST', [], Path, Fields, [{from, master}]).

%% 更新表结构
update_schemas(Fields) ->
    update_schemas(?DEFAULT, Fields).
update_schemas(Name, #{<<"className">> := Class} = Fields) ->
    Path = <<"/schemas/", Class/binary>>,
    request_rest(Name, 'PUT', [], Path, Fields, [{from, master}]).


%% 删除表结构
del_schemas(Class) ->
    del_schemas(?DEFAULT, Class).
del_schemas(Name, Class) ->
    Path = <<"/schemas/", Class/binary>>,
    request_rest(Name, 'DELETE', [], Path, #{}, [{from, master}]).

%% 获取表结构
get_schemas() ->
    get_schemas(<<>>).
get_schemas(Class) ->
    get_schemas(?DEFAULT, Class).
get_schemas(Name, Class) ->
    Path = <<"/schemas/", Class/binary>>,
    request_rest(Name, 'GET', [], Path, #{}, [{from, master}]).

%% 设置表权限
set_class_level(Class, Permissions) ->
    set_class_level(?DEFAULT, Class, Permissions).
set_class_level(Name, Class, Permissions) ->
    Path = <<"/schemas/", Class/binary>>,
    Body = #{<<"classLevelPermissions">> => Permissions},
    request_rest(Name, 'PUT', [], Path, Body, [{from, master}]).


%% limit和skip参数进行分页
%% 传递order逗号分隔列表按多个字段进行排序
%% http://docs.parseplatform.org/rest/guide/#query-constraints
query_object(Class, Args) ->
    query_object(?DEFAULT, Class, Args).
query_object(Name, Class, Args) ->
    query_object(Name, Class, Args, [], [{from, master}]).
query_object(Class, Args, Header, Options) ->
    query_object(?DEFAULT, Class, Args, Header, Options).
query_object(Name, Class, Args, Header, Options) ->
    Path = <<"/classes/", Class/binary>>,
    request_rest(Name, 'GET', Header, Path, Args, Options).

graphql(Data) ->
    Header =
        case maps:get(<<"access_token">>, Data, <<"undefined">>) of
            <<"undefined">> -> [];
            Token -> [{"X-Parse-Session-Token", dgiot_utils:to_list(Token)}]
        end,
    ?LOG(info,"Header ~p", [Header]),
    graphql(?DEFAULT, Header, maps:without([<<"access_token">>], Data)).
graphql(Name, Header, Data) ->
    request_rest(Name, 'POST', Header, <<"/graphql">>, Data, []).


%% limit和skip参数进行分页
%% 传递order逗号分隔列表按多个字段进行排序
%% http://docs.parseplatform.org/rest/guide/#query-constraints
aggregate_object(Class, Args) ->
    aggregate_object(?DEFAULT, Class, Args).
aggregate_object(Name, Class, Args) ->
    aggregate_object(Name, Class, Args, [], [{from, master}]).
aggregate_object(Class, Args, Header, Options) ->
    aggregate_object(?DEFAULT, Class, Args, Header, Options).
aggregate_object(Name, Class, Args, Header, Options) ->
    Path = <<"/aggregate/", Class/binary>>,
    request_rest(Name, 'GET', Header, Path, Args, Options).


%% 删除对象
del_object(Class, ObjectId) ->
    del_object(?DEFAULT, Class, ObjectId).
del_object(Name, Class, ObjectId) ->
    del_object(Name, Class, ObjectId, [], [{from, master}]).
del_object(Class, ObjectId, Header, Options) ->
    del_object(?DEFAULT, Class, ObjectId, Header, Options).
del_object(Name, Class, ObjectId, Header, Options) ->
    Path = <<"/classes/", Class/binary, "/", ObjectId/binary>>,
    request_rest(Name, 'DELETE', Header, Path, #{}, Options).

%% 删除表格
del_table(Class) ->
    del_table(?DEFAULT, Class).
del_table(Name, Class) ->
    Path = <<"/purge/", Class/binary>>,
    request_rest(Name, 'DELETE', [], Path, #{}, [{from, master}]).

%% 创建触发器
add_trigger(Class, TriggerName, Url) ->
    add_trigger(?DEFAULT, Class, TriggerName, Url).
add_trigger(Name, Class, TriggerName, Url) ->
    true = lists:member(TriggerName, [<<"beforeSave">>, <<"beforeDelete">>, <<"afterSave">>, <<"afterDelete">>]),
    Path = <<"/hooks/triggers">>,
    Body = #{
        <<"className">> => Class,
        <<"triggerName">> => TriggerName,
        <<"url">> => Url
    },
    request_rest(Name, 'POST', [], Path, Body, [{from, master}]).


%% 获取触发器
get_trigger() ->
    get_trigger(?DEFAULT).
get_trigger(Name) ->
    Path = <<"/hooks/triggers">>,
    request_rest(Name, 'GET', [], Path, #{}, [{from, master}]).
get_trigger(Class, TriggerName) ->
    get_trigger(?DEFAULT, Class, TriggerName).
get_trigger(Name, Class, TriggerName) ->
    Path = <<"/hooks/triggers/", Class/binary, "/", TriggerName/binary>>,
    request_rest(Name, 'GET', [], Path, #{}, [{from, master}]).


%% 更新触发器
update_trigger(Class, TriggerName, Url) ->
    update_trigger(?DEFAULT, Class, TriggerName, Url).
update_trigger(Name, Class, TriggerName, Url) ->
    Path = <<"/hooks/triggers/", Class/binary, "/", TriggerName/binary>>,
    Body = #{<<"url">> => Url},
    request_rest(Name, 'PUT', [], Path, Body, [{from, master}]).


%% 删除触发器
del_trigger() ->
    case get_trigger() of
        {ok, Results} ->
            Fun =
                fun(#{<<"className">> := Class}) ->
                    del_trigger(Class)
                end,
            lists:foreach(Fun, Results);
        {error, Reason} ->
            {error, Reason}
    end.

del_trigger(Class) ->
    del_trigger(?DEFAULT, Class).
del_trigger(Name, Class) ->
    lists:foreach(
        fun(TriggerName) ->
            del_trigger(Name, Class, TriggerName)
        end, [<<"beforeSave">>, <<"beforeDelete">>, <<"afterSave">>, <<"afterDelete">>]).
del_trigger(Name, Class, TriggerName) ->
    Path = <<"/hooks/triggers/", Class/binary, "/", TriggerName/binary>>,
    Body = #{<<"__op">> => <<"Delete">>},
    request_rest(Name, 'PUT', [], Path, Body, [{from, master}]).


add_all_trigger(Host) ->
    add_all_trigger(?DEFAULT, Host).

add_all_trigger(Name, Host) ->
    case get_trigger(Name) of
        {ok, Triggers} ->
            NTrig = lists:foldl(
                fun(Trigger, Acc) ->
                    ClassName = maps:get(<<"className">>, Trigger),
                    TriggerName = maps:get(<<"triggerName">>, Trigger),
                    Url = maps:get(<<"url">>, Trigger),
                    Acc#{<<ClassName/binary, "/", TriggerName/binary>> => Url}
                end, #{}, Triggers),
            case get_schemas(Name, <<>>) of
                {ok, #{<<"results">> := Results}} ->
                    Fun =
                        fun
                            (#{<<"className">> := Class}, Acc) when Class == <<"_Session">> ->
                                Acc;
                            (#{<<"className">> := Class}, Acc) ->
                                lists:foldl(
                                    fun(TriggerName, Acc1) ->
                                        Path = <<Host/binary, "/hooks/parse_trigger/do?class=", Class/binary, "&name=", TriggerName/binary>>,
                                        Key = <<Class/binary, "/", TriggerName/binary>>,
                                        case maps:get(Key, NTrig, undefined) of
                                            Path ->
                                                Acc1;
                                            _ ->
                                                case add_trigger(Name, Class, TriggerName, Path) of
                                                    {ok, _} ->
                                                        [{Key, success} | Acc1];
                                                    {error, Reason} ->
                                                        ?LOG(error,"~p,~p~n", [Key, Reason]),
                                                        [Reason | Acc1]
                                                end
                                        end
                                    end, Acc, [<<"beforeSave">>, <<"beforeDelete">>, <<"afterSave">>, <<"afterDelete">>])
                        end,
                    lists:foldl(Fun, [], Results)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

read_page(Class, Query, Skip, PageSize) ->
    read_page(?DEFAULT, Class, Query, Skip, PageSize).
read_page(Name, Class, Query, Skip, PageSize) ->
    %?LOG(info,"~p~n", [ Query#{<<"limit">> => PageSize, <<"skip">> => Skip}]),
    case query_object(Name, Class, Query#{<<"limit">> => PageSize, <<"skip">> => Skip}) of
        {error, Reason} ->
            {error, Reason};
        {ok, #{<<"results">> := Page}} ->
            {ok, Page}
    end.

%% 更新Session
create_session(UserId, SessionToken, TTL) ->
    create_session(?DEFAULT, UserId, SessionToken, TTL).
create_session(Name, UserId, SessionToken, TTL) ->
    case get_object(Name, <<"_User">>, binary:replace(UserId, <<" ">>, <<>>, [global])) of
        {ok, #{<<"objectId">> := UserId} = UserInfo} ->
            Now = dgiot_datetime:nowstamp() + dgiot_utils:to_int(TTL) - 8 * 60 * 60,
            Map = #{
                <<"sessionToken">> => SessionToken,
                <<"restricted">> => false,
                <<"installationId">> => <<>>,
                <<"expiresAt">> => #{
                    <<"__type">> => <<"Date">>,
                    <<"iso">> => dgiot_datetime:format(Now, <<"YY-MM-DDTHH:NN:SS.000Z">>)
                },
                <<"user">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"_User">>,
                    <<"objectId">> => UserId
                },
                <<"createdWith">> => #{
                    <<"action">> => <<"login">>,
                    <<"authProvider">> => <<"token">>
                }
            },
            case create_object(Name, <<"_Session">>, Map) of
                {ok, #{<<"objectId">> := _SessionId}} ->
                    {ok, UserInfo#{<<"sessionToken">> => SessionToken}};
                {error, Why} ->
                    {error, Why}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

check_session(Token) ->
    check_session(?DEFAULT, Token).
check_session(Name, Token) ->
    Now = dgiot_datetime:nowstamp() - 8 * 60 * 60,
    Where = #{
        <<"objectId">> => #{
            <<"$select">> => #{
                <<"key">> => <<"user">>,
                <<"query">> => #{
                    <<"className">> => <<"_Session">>,
                    <<"where">> => #{
                        <<"sessionToken">> => Token,
                        <<"expiresAt">> => #{
                            <<"$gte">> => #{
                                <<"__type">> => <<"Date">>,
                                <<"iso">> => dgiot_datetime:format(Now, <<"YY-MM-DDTHH:NN:SS.000Z">>)
                            }
                        }
                    }
                }
            }
        }
    },
    case query_object(Name, <<"_User">>, #{<<"where">> => Where, <<"limit">> => 1}) of
        {ok, #{<<"results">> := [User]}} ->
            {ok, User};
        {ok, #{<<"results">> := []}} ->
            {error, #{<<"code">> => 101, <<"error">> => <<"Object not found.">>}};
        {error, Reason} ->
            {error, Reason}
    end.


%% 查取角色
get_role(UserId, SessionToken) ->
    get_role(?DEFAULT, UserId, SessionToken).
get_role(Name, UserId, SessionToken) ->
    Query = #{
        <<"keys">> => [<<"name">>, <<"alias">>, <<"org_type">>, <<"tag">>],
        <<"where">> => #{
            <<"users">> => #{
                <<"className">> => <<"_User">>,
                <<"objectId">> => UserId,
                <<"__type">> => <<"Pointer">>
            }
        }
    },
    case query_object(Name, <<"_Role">>, Query) of
        {ok, #{<<"results">> := RoleResults}} ->
            Roles =
                lists:foldr(
                    fun(#{<<"objectId">> := RoleId, <<"name">> := Name1, <<"alias">> := Alias, <<"org_type">> := Org_type} = X, Acc) ->
                        Role = #{<<"name">> => Name1, <<"alias">> => Alias, <<"org_type">> => Org_type, <<"tag">> => maps:get(<<"tag">>, X, #{})},
                        Acc#{RoleId => Role}
                    end, #{}, RoleResults),
            RoleIds =
                lists:foldr(
                    fun(#{<<"objectId">> := RoleId}, Acc) ->
                        Acc ++ [RoleId]
                    end, [], RoleResults),
            case get_rules(Name, RoleIds, SessionToken) of
                {ok, Rules} ->
                    Info = #{
                        <<"rules">> => Rules,
                        <<"roles">> => Roles
                    },
                    {ok, Info};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% 根据角色获取权限
get_rules(Name, RoleIds, SessionToken) ->
    Requests = [#{
        <<"method">> => <<"GET">>,
        <<"path">> => <<"/classes/Permission">>,
        <<"body">> => #{
            <<"keys">> => [<<"name">>],
            <<"where">> => #{
                <<"$relatedTo">> => #{
                    <<"key">> => <<"rules">>,
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId
                    }
                }
            }
        }
    } || RoleId <- RoleIds],
    case dgiot_parse:batch(Name, Requests, [{"X-Parse-Session-Token", binary_to_list(SessionToken)}], [{from, rest}]) of
        {ok, Results1} ->
            {ok, lists:foldr(
                fun(#{<<"success">> := #{<<"results">> := R}}, Acc1) ->
                    lists:foldr(fun(#{<<"name">> := Name1}, Acc2) ->
                        case lists:member(Name, Acc2) of
                            true ->
                                Acc2;
                            false ->
                                [Name1 | Acc2]
                        end
                                end, Acc1, R)
                end, [], Results1)};
        {error, Reason} ->
            {error, Reason}
    end.

add_to_role(Info, Field, Class, ObjectIds) ->
    add_to_role(?DEFAULT, Info, Field, Class, ObjectIds).

add_to_role(Name, #{<<"objectId">> := RoleId} = Info, Field, Class, ObjectIds) ->
    Users = [#{
        <<"__type">> => <<"Pointer">>,
        <<"className">> => Class,
        <<"objectId">> => ObjectId
    } || ObjectId <- ObjectIds],
    case update_object(Name, <<"_Role">>, RoleId, Info#{Field => #{
        <<"__op">> => <<"AddRelation">>,
        <<"objects">> => Users
    }}) of
        {ok, #{<<"updatedAt">> := _}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
add_to_role(Name, #{<<"name">> := <<"role:", RoleName/binary>>} = Role, Field, Class, ObjectIds) ->
    add_to_role(Name, Role#{<<"name">> => RoleName}, Field, Class, ObjectIds);
add_to_role(Name, #{<<"name">> := RoleName} = Role, Field, Class, ObjectIds) ->
    case query_object(Name, <<"_Role">>, #{<<"where">> => #{<<"name">> => RoleName}}) of
        {ok, #{<<"results">> := []}} ->
            {error, #{error => <<"Role: ", RoleName/binary, " not find!">>}};
        {ok, #{<<"results">> := [#{<<"objectId">> := RoleId}]}} ->
            Info = maps:without([<<"name">>], Role),
            add_to_role(Name, Info#{<<"objectId">> => RoleId}, Field, Class, ObjectIds);
        {error, Reason} ->
            {error, Reason}
    end.

import(Class, Datas, Count, Fun, Acc) ->
    import(?DEFAULT, Class, Datas, Count, Fun, Acc).

import(Name, Class, {json, Path}, Count, Fun, Acc) ->
    ?LOG(info,"~p import to ~p:~p~n", [Name, Class, Path]),
    case file:read_file(Path) of
        {ok, Bin} ->
            case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
                {'EXIT', Reason} ->
                    {error, Reason};
                Datas ->
                    import(Name, Class, Datas, Count, Fun, Acc)
            end;
        {error, Reason} ->
            {error, Reason}
    end;

import(Name, Class, Datas, Count, Fun, Acc) ->
    import(Name, Class, Datas, Count, [], Fun, Acc).

import(Name, Class, Datas, Count, Requests, Fun, Acc) when length(Requests) == Count; Datas == [] ->
    case batch(Name, Requests) of
        {error, Reason} ->
            {error, Reason};
        {ok, Results} ->
            ResAcc = Fun(Results, Acc),
            case Datas == [] of
                true -> ResAcc;
                false -> import(Name, Class, Datas, Count, Fun, ResAcc)
            end
    end;

import(Name, Class, [Data | Other], Count, Requests, Fun, Acc) when length(Requests) < Count ->
    try
        NewRequests = [#{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"classes/", Class/binary>>,
            <<"body">> => format_data(Class, Data)
        } | Requests],
        import(Name, Class, Other, Count, NewRequests, Fun, Acc)
    catch
        _: {error, not_add} ->
            import(Name, Class, Other, Count, Requests, Fun, Acc)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
                {'EXIT', Reason} ->
                    {error, Reason};
                Data ->
                    {ok, Data}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 根据Map查找出关联数据
format_data(<<"Dict">>, Data) ->
    NewData =
        case Data of
            #{<<"key">> := _} -> Data;
            _ -> Data#{<<"key">> => dgiot_license:to_md5(jsx:encode(Data))}
        end,
    maps:fold(
        fun(Key, Value, Acc) ->
            case format_value(<<"Dict">>, Key, Value) of
                not_add ->
                    throw({error, not_add});
                {<<"Pointer">>, ParentId} ->
                    Acc#{Key => Value#{<<"objectId">> => ParentId}};
                {<<"AddRelation">>, Objects} ->
                    Acc#{Key => Value#{<<"objects">> => Objects}};
                NValue ->
                    Acc#{Key => NValue}
            end
        end, #{}, NewData);

format_data(Class, Data) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            case format_value(Class, Key, Value) of
                not_add ->
                    throw({error, not_add});
                {<<"Pointer">>, ParentId} ->
                    Acc#{Key => Value#{<<"objectId">> => ParentId}};
                {<<"AddRelation">>, Objects} ->
                    Acc#{Key => Value#{<<"objects">> => Objects}};
                NValue ->
                    Acc#{Key => NValue}
            end
        end, #{}, Data).

format_value(_Class, _Key, #{<<"__type">> := <<"Pointer">>, <<"className">> := ClassName, <<"objectId">> := #{<<"where">> := Where}}) ->
    case query_object(ClassName, #{<<"where">> => Where}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId} | _]}} ->
            {<<"Pointer">>, ObjectId};
        {ok, #{<<"results">> := []}} ->
            {error, {Where, <<"object not find!">>}};
        {error, Reason} ->
            {error, Reason}
    end;
format_value(Class, Key, #{<<"__op">> := <<"AddRelation">>, <<"objects">> := Objects}) ->
    Fun =
        fun(ClassName, ObjectId) ->
            #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => ClassName,
                <<"objectId">> => ObjectId
            }
        end,
    {<<"AddRelation">>, lists:foldl(
        fun(#{<<"className">> := ClassName} = Object, Acc) ->
            case format_value(Class, Key, Object) of
                {<<"Pointer">>, ObjectId} ->
                    [Fun(ClassName, ObjectId) | Acc];
                {error, _Reason} ->
                    Acc
            end
        end, [], Objects)};
format_value(Class, Key, #{<<"iskey">> := IsKey, <<"value">> := Value}) ->
    case IsKey of
        false ->
            Value;
        true ->
            case query_object(Class, #{<<"keys">> => Key, <<"where">> => #{Key => Value}}) of
                {ok, #{<<"results">> := []}} ->
                    ?LOG(info,"~p, ~p,~p", [Class, Key, Value]),
                    Value;
                {ok, #{<<"results">> := _}} ->
                    not_add;
                {error, Reason} ->
                    {error, Reason}
            end
    end;
format_value(_Class, _Key, Value) when is_binary(Value) ->
    Node = atom_to_list(node()),
    [NodeName, Host] = string:tokens(Node, "@"),
    lists:foldl(
        fun({RE, Replace}, New) ->
            re:replace(New, RE, Replace, [global, {return, binary}])
        end, Value, [{<<"\\{host\\}">>, Host}, {<<"\\{node\\}">>, Node}, {<<"\\{nodename\\}">>, NodeName}]);
format_value(_Class, _Key, Value) ->
    Value.


%% Rest请求
request_rest(Name, Method, Header, Path, Body, Options) ->
    handle_response(request(Name, Method, Header, Path, Body, Options)).
request(Method, Header, Path, Options) ->
    request(Method, Header, Path, <<>>, Options).
request(Method, Header, Path0, Body, Options) ->
    request(?DEFAULT, Method, Header, Path0, Body, Options).
request(Name, Method, Header, Path0, Body, Options) ->
    case dgiot_parse_channel:get_config(Name) of
        {ok, Cfg} ->
            NewOpts = [{cfg, Cfg} | Options],
            dgiot_parse_rest:request(Method, Header, Path0, Body, NewOpts);
        {error, Reason} ->
            {error, Reason}
    end.


get_tables(Dirs) -> get_tables(Dirs, []).
get_tables([], Acc) -> Acc;
get_tables([Dir | Other], Acc) ->
    Dir0 = Dir ++ "/tables/",
    case file:list_dir(Dir0) of
        {ok, Files} ->
            Acc2 = lists:foldl(
                fun(File, Acc1) ->
                    Path = Dir0 ++ File,
                    case filelib:is_file(Path) andalso read_file(Path) of
                        {ok, Data} ->
                            lists:concat([Data, Acc1]);
                        false ->
                            Acc1;
                        {error, Reason} ->
                            ?LOG(error,"load error ~p,~p~n", [Path, Reason]),
                            Acc1
                    end
                end, Acc, Files),
            get_tables(Other, Acc2);
        {error, enoent} ->
            get_tables(Other, Acc)
    end.


%% 初始化数据库，op : 如果已经存在是删除还是合并
init_database(Dirs, Op) ->
    init_database(?DEFAULT, Dirs, Op).
init_database(Name, Dirs, Op) when Op == merge; Op == delete ->
    case get_schemas(Name, <<>>) of
        {ok, #{<<"results">> := OldSchemas1}} ->
            OldSchemas =
                lists:foldl(
                    fun(#{<<"className">> := Class} = Tab, Acc) ->
                        Fields = maps:without(?DEFField, maps:get(<<"fields">>, Tab, #{})),
                        case maps:size(Fields) == 0 of
                            true ->
                                Acc#{Class => Tab};
                            false ->
                                Acc#{Class => Tab#{<<"fields">> => Fields}}
                        end
                    end, #{}, OldSchemas1),
            file:write_file("data/db.schema", jsx:encode(maps:values(OldSchemas))),
            Schemas = get_tables(Dirs),
            init_tables(Name, OldSchemas, Schemas, Op);
        {error, Reason} ->
            ?LOG(error,"~p~n", [Reason]),
            {error, Reason}
    end.

init_tables(Name, OldSchemas, Schemas, Op) ->
    lists:foreach(
        fun(#{<<"className">> := Class} = Schema) ->
            NewFields = maps:get(<<"fields">>, Schema, #{}),
            Result =
                case maps:get(Class, OldSchemas, undefined) of
                    undefined ->
                        create_schemas(Name, Schema);
                    #{<<"fields">> := OldFields} when Op == merge ->
                        {Targets, Fields} = merge_table(Name, Class, maps:without(?DEFField, NewFields), maps:without(?DEFField, OldFields)),
                        TargetTab = maps:keys(Targets),
                        case length(TargetTab) > 0 of
                            true ->
                                % @todo 需要提前创建这些表
                                ?LOG(info,"~p~n", [TargetTab]);
                            false ->
                                ok
                        end,
                        UpdateSchema = Schema#{<<"fields">> => Fields},
                        case maps:size(Fields) == 0 of
                            true ->
                                update_schemas(Name, maps:without([<<"fields">>], UpdateSchema));
                            false ->
                                update_schemas(Name, UpdateSchema)
                        end;
                    #{<<"fields">> := _OldFields} when Op == delete ->
                        case del_schemas(Name, Class) of
                            {ok, _} ->
                                create_schemas(Name, Schema);
                            Err ->
                                Err
                        end
                end,
            case Result of
                {error, #{<<"message">> := Why}} ->
                    ?LOG(error,"~p:~p~n", [Class, Why]);
                {error, #{<<"error">> := Why}} ->
                    ?LOG(error,"~p:~p~n", [Class, Why]);
                ok ->
                    ok;
                {ok, _Rtn} ->
                    %?LOG(info,"~p:create success -> ~p~n", [Class, Rtn]),
                    ok
            end
        end, Schemas).

merge_table(Name, Class, NewFields, OldFields) ->
    maps:fold(
        fun(Key, Type, {Targets, Acc}) ->
            case maps:get(Key, NewFields, no) of
                no ->
                    {Targets, Acc#{Key => #{<<"__op">> => <<"Delete">>}}};
                Type ->
                    {Targets, maps:without([Key], Acc)};
                NewType ->
                    update_schemas(Name, #{<<"className">> => Class, <<"fields">> => #{Key => #{<<"__op">> => <<"Delete">>}}}),
                    case is_map(Type) andalso maps:get(<<"targetClass">>, NewType, false) of
                        false ->
                            {Targets, Acc};
                        TargetClass ->
                            {Targets#{TargetClass => true}, Acc}
                    end
            end
        end, {#{}, NewFields}, OldFields).


handle_response(Result) ->
    Fun =
        fun(Res, Body) ->
            case catch jsx:decode(Body, [{labels, binary}, return_maps]) of
                {'EXIT', Reason} ->
                    {error, Reason};
                #{<<"code">> := Code, <<"error">> := #{<<"routine">> := Reason}} ->
                    {error, #{<<"code">> => Code, <<"error">> => Reason}};
                Map when map_size(Map) == 0 ->
                    Res;
                Map ->
                    {Res, Map}
            end
        end,
    case Result of
        {ok, HTTPCode, _Headers, Body} when HTTPCode == 200; HTTPCode == 201 ->
            Fun(ok, Body);
        {ok, HTTPCode, _Headers, Body} when HTTPCode == 404; HTTPCode == 400; HTTPCode == 500 ->
            Fun(error, Body);
        {ok, _HTTPCode, _Headers, Body} ->
            Fun(error, Body);
        {error, #{<<"code">> := Code, <<"routine">> := Reason}} ->
            {error, #{<<"code">> => Code, <<"error">> => Reason}};
        {error, Reason} ->
            {error, #{<<"code">> => 1, <<"error">> => Reason}}
    end.


all_test() ->
    {ok, #{<<"objectId">> := Id}} = create_object(<<"test">>, #{<<"name">> => 1}),
    {ok, #{<<"results">> := [#{<<"objectId">> := Id1}]}} = query_object(<<"test">>, #{<<"name">> => 1}),
    ?assertEqual(Id1, Id),
    {ok, #{<<"updatedAt">> := _}} = update_object(<<"test">>, Id, #{<<"age">> => 12}),
    {ok, #{<<"age">> := Age}} = get_object(<<"test">>, Id),
    ?assertEqual(Age, 12),
    del_object(<<"test">>, Id),
    {error, _} = get_object(<<"test">>, Id).

test_graphql() ->
    Data = #{
        <<"operationName">> => <<"Health">>,
        <<"variables">> => #{},
        <<"query">> => <<"query Health {\n  health\n}\n">>
    },
%%    {"operationName":"Health","variables":{},"query":"query Health {\n  health\n}\n"}
    graphql(Data).
