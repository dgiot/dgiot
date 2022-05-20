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
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(DEFField, re:split(application:get_env(?MODULE, delete_field, ""), ",")).

%% API
-export([
    health/0,
    health/1,
    init_database/2,
    create_object/2,
    create_object/3,
    create_object/4,
    get_object/2,
    get_object/3,
    get_object/4,
    update_object/3,
    update_object/4,
    update_object/5,
    del_object/2,
    del_object/4,
    del_table/1,
    get_schemas/0,
    get_schemas/1,
    get_schemas/2,
    create_schemas/1,
    create_schemas/2,
    update_schemas/1,
    del_schemas/1,
    del_schemas/2,
    set_class_level/2,
    query_object/2,
    query_object/3,
    query_object/4,
    aggregate_object/2,
    aggregate_object/4,
    read_page/4,
    read_page/5,
    format_data/2,
    batch/1,
    batch/2,
    batch/3,
    batch/4,
    import/5,
    import/6,
    request/4,
    request/5,
    get_token/1,
    get_qs/1,
    update/0
]).

-export([
    request_rest/6
]).

update() ->
%%    物模型更新
    dgiot_product:update_properties(),
%%    表及其字段更新
%%    topics更新
    dgiot_product:update_topics(),
%%    产品字段新增
%%  适配https://gitee.com/dgiiot/dgiot/issues/I583AD
    dgiot_product:update_product_filed(#{<<"profile">> => #{}, <<"content">> => #{}}),
%%    菜单更新
%%    API更新
    ok.

health() ->
    health(?DEFAULT).
health(Name) ->
    Path = <<"/health">>,
    request_rest(Name, 'GET', [], Path, #{}, [{from, rest}]).

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
    request_rest(Name, 'POST', Header, Path, dgiot_parse_id:get_objectid(Class, Map), Options).


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

del_filed_schemas(Class, Fileds) ->
    del_filed_schemas(?DEFAULT, Class, Fileds).
%%
del_filed_schemas(Name, Class, Fileds) ->
    Path = <<"/schemas/", Class/binary>>,
    NewFields = lists:foldl(
        fun(X, Acc) ->
            Acc#{X => #{<<"__op">> => <<"Delete">>}}
        end, #{}, Fileds),

    Method = <<"PUT">>,
    Body = #{
        <<"className">> => <<Class/binary>>,
        <<"fields">> => NewFields,
        <<"_method">> => Method
    },
    request_rest(Name, 'PUT', [], Path, Body, [{from, master}]).

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

import(Class, Datas, Count, Fun, Acc) ->
    import(?DEFAULT, Class, Datas, Count, Fun, Acc).

import(Name, Class, {json, Path}, Count, Fun, Acc) ->
    ?LOG(info, "~p import to ~p:~p~n", [Name, Class, Path]),
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
            <<"path">> => <<"/classes/", Class/binary>>,
            <<"body">> => Data
        } | Requests],
        import(Name, Class, Other, Count, NewRequests, Fun, Acc)
    catch
        _: {error, not_add} ->
            import(Name, Class, Other, Count, Requests, Fun, Acc)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_token(Header) ->
    case proplists:get_value("X-Parse-Session-Token", Header) of
        undefined ->
            proplists:get_value(<<"X-Parse-Session-Token">>, Header);
        Token1 ->
            Token1
    end.

get_qs(Map) ->
    lists:foldl(
        fun
            ({N, V}, <<>>) ->
                NewV = format_value(V),
                <<"?", N/binary, "=", NewV/binary>>;
            ({N, V}, Acc) ->
                NewV = format_value(V),
                <<Acc/binary, "&", N/binary, "=", NewV/binary>>
        end, <<>>, maps:to_list(Map)).

format_value(V) when is_binary(V) ->
    dgiot_httpc:urlencode(V);
format_value(V) ->
    Json = jsx:encode(V),
%%    V1 = re:replace(Json, <<"\[.*?\]">>, <<"">>, [global, {return, binary}, unicode]),
    dgiot_httpc:urlencode(Json).

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
            _ -> Data#{<<"key">> => dgiot_utils:to_md5(jsx:encode(Data))}
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
                    ?LOG(info, "~p, ~p,~p", [Class, Key, Value]),
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
    Response = request(Name, Method, Header, Path, Body, Options),
    handle_response(Response).
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
                            ?LOG(error, "load error ~p,~p~n", [Path, Reason]),
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
            ?LOG(error, "~p~n", [Reason]),
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
                                ?LOG(info, "~p~n", [TargetTab]);
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
                    ?LOG(error, "~p:~p~n", [Class, Why]);
                {error, #{<<"error">> := Why}} ->
                    ?LOG(error, "~p:~p~n", [Class, Why]);
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
            case jsx:is_json(Body) of
                true ->
                    case catch jsx:decode(Body, [{labels, binary}, return_maps]) of
                        {'EXIT', Reason} ->
                            {error, Reason};
                        #{<<"code">> := Code, <<"error">> := #{<<"routine">> := Reason}} ->
                            {error, #{<<"code">> => Code, <<"error">> => Reason}};
                        Map when map_size(Map) == 0 ->
                            Res;
                        Map ->
                            {Res, Map}
                    end;
                false ->
                    Res
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


