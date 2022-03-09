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

-module(dgiot_data_handler).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_api.hrl").
-behavior(dgiot_rest).
-dgiot_rest(all).

%% API
-export([swagger_data/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/iotdev">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_data.json">>, ?MODULE, [], priv)
swagger_data() ->
    [
        dgiot_http_server:bind(<<"/swagger_data.json">>, ?MODULE, [], priv)
    ].


%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            ?LOG(debug, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_utils:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            ?LOG(debug, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% System 概要: 服务器文件上传 描述:文件上传到服务器
%% OperationId:post_upload
%% 请求:POST /iotapi/upload
do_request(post_upload, #{<<"file">> := FileInfo}, #{<<"user">> := #{<<"objectId">> := UserId}}, _Req) ->
    ?LOG(info, "FileInfo ~p", [FileInfo]),
    Key = dgiot_license:to_md5(jsx:encode(FileInfo#{node => node()})),
    case dgiot_parse:create_object(<<"Dict">>, #{
        <<"ACL">> => #{UserId => #{<<"read">> => true, <<"write">> => true}},
        <<"type">> => <<"file">>,
        <<"key">> => Key,
        <<"data">> => FileInfo#{node => node()}
    }) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            {ok, FileInfo#{<<"objectId">> => ObjectId}};
        {error, Reason} ->
            {error, Reason}
    end;

%% System 概要: 发送短信验证码 描述:发送短信,短信验证码发送成功后,则会在缓存中写入action + mobile, 用户下一步提交时，可以根据此键查询验证通过
%% OperationId:post_sendsms_action
%% 请求:POST /iotapi/sendsms/:Action
do_request(post_sendsms, #{<<"account">> := Account, <<"nationcode">> := NationCode}, _Context, _Req) ->
    case dgiot_notification:send_verification_code(NationCode, Account) of
        {error, Reason} ->
            {500, #{code => 1, error => Reason}};
        {ok, Map} ->
            {ok, Map}
    end;

%% System 概要: 验证手机号/邮箱是否通过 描述:验证手机号/邮箱是否通过
%% OperationId:post_verify_code
%% 请求:POST /iotapi/verify_code
do_request(post_verify_code_action, #{<<"account">> := Account, <<"code">> := Code} = Args, _Context, Req) ->
    case dgiot_notification:check_verification_code(Account, Code) of
        true ->
            dgiot_verify_code:handle(Args, Req);
        false ->
            {400, unicode:characters_to_binary(<<"验证码未通过！"/utf8>>)}
    end;

do_request(post_upload_token, #{<<"from">> := <<"fastdfs">>}, _Context, Req0) ->
    {ok, Body, _Req1} = dgiot_req:read_body(Req0),
    case jsx:decode(Body, [{labels, binary}, return_maps]) of
        #{<<"path">> := Path, <<"auth_token">> := AuthToken} = Info ->
            case dgiot_auth:get_session(AuthToken) of
                #{<<"roles">> := Role} = User ->
                    case maps:values(Role) of
                        [#{<<"name">> := Path} | _] -> {200, <<"ok">>};
                        _ ->
                            case User of
                                #{<<"objectId">> := UserId} ->
                                    case dgiot_parse:get_role(UserId, AuthToken) of
                                        {ok, _Result} -> {200, <<"ok">>};
                                        _ -> {200, <<"fail">>}
                                    end;
                                _ ->
                                    ?LOG(warning, "post_upload_token No Login ~p", [Info]),
                                    {200, <<"fail">>}
                            end
                    end
            end;
        _ ->
            case _Req1 of
                #{body := Body1} ->
                    case jsx:decode(Body1, [{labels, binary}, return_maps]) of
                        #{<<"auth_token">> := AuthToken} ->
                            case dgiot_auth:get_session(AuthToken) of
                                #{<<"roles">> := _Role} = _User ->
                                    {200, <<"ok">>};
                                _ ->
                                    {200, <<"fail">>}
                            end;
                        _ ->
                            {200, <<"fail">>}
                    end
            end
    end;

do_request(post_graphql, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    case dgiot_parse:graphql(Body#{<<"access_token">> => SessionToken}) of
        {ok, Result} ->
            {200, Result};
        Other -> Other
    end;


%% Thing 概要: 导库 描述:查询物模型
%% OperationId:get_thing
%% 请求:GET /iotapi/get_thing
do_request(get_thing, #{<<"productid">> := ProductId, <<"moduleType">> := ModuleType},
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    getThing(ProductId, ModuleType, SessionToken);

%% Thing 概要: 导库 描述:添加物模型
%% OperationId:post_thing
%% 请求:PUT /iotapi/post_thing
do_request(post_thing, #{<<"productid">> := ProductId, <<"item">> := Item} = _Body,
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    postThing(ProductId, Item, SessionToken);

%% Thing 概要: 导库 描述:修改物模型
%% OperationId:put_thing
%% 请求:PUT /iotapi/put_thing
do_request(put_thing, #{<<"productid">> := ProductId, <<"item">> := Item} = _Body,
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    putTing(ProductId, Item, SessionToken);


%% Thing 概要: 导库 描述:删除物模型
%% OperationId:put_thing
%% 请求:PUT /iotapi/put_thing
do_request(delete_thing, #{<<"productid">> := ProductId, <<"item">> := Item} = _Body,
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    deleteThing(ProductId, SessionToken, Item);


%% Product 概要: 导库 描述:json文件导库
%% OperationId:post_product
%% 请求:POST /iotapi/post_product
do_request(post_product, #{<<"file">> := FileInfo, <<"appid">> := Appid} = _Body,
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case maps:get(<<"contentType">>, FileInfo, <<"unknow">>) of
        <<"application/x-zip-compressed">> -> post_product(FileInfo, Appid, SessionToken);
        <<"application/zip">> -> post_product(FileInfo, Appid, SessionToken);
        ContentType ->
            {error, <<"contentType error, contentType:", ContentType/binary>>}
    end;

%% Device 概要: 查询设备 描述:查询设备时序数据
%% OperationId:post_device
%% 请求:POST /iotapi/post_device
do_request(post_device, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_parse:query_object(<<"Device">>, Body#{<<"include">> => <<"product">>}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := _Result} = All} ->
            {200, All};
        {error, What} ->
            {error, What}
    end;

%% Device 概要: 查询设备 描述:序列号添加设备
%% OperationId:post_adddevice
%% 请求:POST /iotapi/post_adddevice
do_request(post_adddevice, #{<<"devaddr">> := Devaddr, <<"productid">> := ProductId, <<"longitude">> := Longitude, <<"latitude">> := Latitude}, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
    case dgiot_auth:get_session(SessionToken) of
        #{<<"objectId">> := UserId} ->
            Acl =
                case dgiot_parse:get_roleids(UserId) of
                    [RoleId | _] ->
                        case dgiot_parse:get_object(<<"_Role">>, RoleId) of
                            {error, _} ->
                                #{<<"role:admin">> => #{<<"read">> => true, <<"write">> => true}};
                            {ok, #{<<"ACL">> := Acl1}} ->
                                Acl1;
                            _ ->
                                #{<<"role:admin">> => #{<<"read">> => true, <<"write">> => true}}
                        end;
                    _ ->
                        #{<<"role:admin">> => #{<<"read">> => true, <<"write">> => true}}
                end,
            case dgiot_parse:get_object(<<"Product">>, ProductId) of
                {ok, #{<<"devType">> := DevType, <<"name">> := ProductName}} ->
                    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                        {ok, _Re} ->
                            {_, Result} = dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"ACL">> => Acl, <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => dgiot_utils:to_float(Longitude), <<"latitude">> => dgiot_utils:to_float(Latitude)}}),
                            {ok, Result#{<<"objectId">> => DeviceId}};
                        _R ->
                            dgiot_device:create_device(#{
                                <<"devaddr">> => Devaddr,
                                <<"name">> => <<ProductName/binary, Devaddr/binary>>,
                                <<"ip">> => <<>>,
                                <<"isEnable">> => true,
                                <<"product">> => ProductId,
                                <<"ACL">> => Acl,
                                <<"status">> => <<"ONLINE">>,
                                <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => dgiot_utils:to_float(Longitude), <<"latitude">> => dgiot_utils:to_float(Latitude)},
                                <<"brand">> => ProductName,
                                <<"devModel">> => DevType
                            })
                    end;
                Error2 -> ?LOG(info, "Error2 ~p ", [Error2])
            end;
        _ ->
            {error, <<"Not Allowed.">>}
    end;

%% Product 概要: 导库 描述:json文件导库
%% OperationId:post_product
%% 请求:POST /iotapi/post_product
do_request(get_product, #{<<"name">> := Name}, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    FileName = unicode:characters_to_list(Name) ++ ".zip",
    case get_product(Name, FileName, SessionToken) of
        {ok, ZipFile} ->
            Headers = #{
                <<"content-type">> => <<"application/zip">>,
                <<"Content-Disposition">> => unicode:characters_to_binary("attachment;filename=" ++ FileName)
            },
            {200, Headers, ZipFile};
        Err ->
            Err
    end;

%% Product 概要: 导库 描述:json文件导库
%% OperationId:post_product
%% 请求:POST /iotapi/post_product
do_request(post_hash_class, #{<<"class">> := Class} = Body, #{<<"sessionToken">> := _SessionToken} = _Context, _Req) ->
    {ok, dgiot_parse:get_objectid(Class, Body)};

%% 档案 概要: 导库 描述:json文件导库
%% OperationId:post_product
%% 请求:POST /iotapi/post_product
do_request(post_export_data, #{<<"classname">> := Name} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    Filter = maps:without([<<"classname">>], Body),
    FileName = unicode:characters_to_list(Name) ++ ".zip",
    ?LOG(info, "Name ~p Filter ~p ", [Name, Filter]),
    case get_class(Name, Filter, FileName, SessionToken) of
        {ok, ZipFile} ->
            Headers = #{
                <<"content-type">> => <<"application/zip">>,
                <<"Content-Disposition">> => unicode:characters_to_binary("attachment;filename=" ++ FileName)
            },
            {200, Headers, ZipFile};
        Err ->
            Err
    end;

%% DB 概要: 导库 描述:json文件导库
%% OperationId:post_import_data
%% 请求:POST /iotapi/import_data
do_request(post_import_data, #{<<"className">> := Class, <<"file">> := FileInfo}, _Context, _Req) ->
    DataResult =
        case maps:get(<<"contentType">>, FileInfo, <<"unknow">>) of
            <<"application/x-zip-compressed">> -> get_json_from_zip(FileInfo);
            <<"application/zip">> -> get_json_from_zip(FileInfo);
            <<"application/json">> ->
                Dir = list_to_binary(dgiot_http_server:get_env(?APP, docroot)),
                #{<<"path">> := <<"/", Path/binary>>} = FileInfo,
                FilePath = filename:join([Dir, Path]),
                case file:read_file(FilePath) of
                    {ok, Bin} ->
                        case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
                            {'EXIT', Reason} ->
                                {error, Reason};
                            Datas -> {ok, Datas}
                        end;
                    {error, Reason2} ->
                        {error, Reason2}
                end;
            ContentType ->
                {error, <<"contentType error, contentType:", ContentType/binary>>}
        end,
    case DataResult of
        {ok, Data} ->
            Fun =
                fun(Res, Acc) ->
                    ?LOG(info, "Res ~p", [Res]),
                    lists:concat([Acc, Res])
                end,
            case dgiot_parse:import(Class, Data, length(Data), Fun, []) of
                {error, Reason1} ->
                    {error, Reason1};
                Result ->
                    {ok, Result}
            end;
        Error -> Error
    end;


%% 文件 概要: 导库 描述:json文件导出
%% OperationId:post_export_file
%% 请求:POST /iotapi/post_export_file
do_request(post_export_file, #{<<"files">> := Files}, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    case dgiot_parse:query_object(<<"_Role">>, #{
        <<"keys">> => [<<"name">>, <<"tag">>],
        <<"limit">> => 1}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"name">> := AppName, <<"tag">> := #{<<"appconfig">> := Config}} | _]}} ->
            Root = unicode:characters_to_list(maps:get(<<"home">>, Config, <<"D:/dgiot/dgiot_data_center/datacenter/file/files">>)),
            FileServer = unicode:characters_to_list(maps:get(<<"file">>, Config, <<"http://127.0.0.1:1250/shapes/upload">>)),
            App = unicode:characters_to_list(AppName),
            Url = string:sub_string(FileServer, 1, string:rchr(FileServer, $/)),
            FileList = lists:foldl(fun(X, Acc) ->
                [_, _, FileApp, FileName] = re:split(X, <<"/">>),
                Acc ++ [{FileApp, FileName}]
                                   end, [], Files),
            FileUrl = Url ++ dgiot_utils:get_file(Root, FileList, App),
            {ok, #{
                <<"url">> => unicode:characters_to_binary(FileUrl)
            }};
        _ -> {error, <<"not find">>}
    end;

%% 文件 概要: 导库 描述:文件导入
%% OperationId:post_import_file
%% 请求:POST /iotapi/post_import_file
do_request(post_import_file, #{<<"path">> := Path}, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    case dgiot_parse:query_object(<<"_Role">>, #{
        <<"keys">> => [<<"name">>, <<"tag">>],
        <<"limit">> => 1}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"name">> := AppName, <<"tag">> := #{<<"appconfig">> := Config}} | _]}} ->
            Root = unicode:characters_to_list(maps:get(<<"home">>, Config, <<"D:/dgiot/dgiot_data_center/datacenter/file/files">>)),
            [_, _, FileApp, FileName] = re:split(Path, <<"/">>),
            ?LOG(info, "FileApp ~p", [FileApp]),
            case AppName of
                FileApp ->
                    NewPath = unicode:characters_to_list(FileApp) ++ "/" ++ unicode:characters_to_list(FileName),
                    case dgiot_utils:post_file(Root, NewPath) of
                        {ok, _} -> {ok, #{<<"url">> => unicode:characters_to_binary(NewPath)}};
                        Error -> Error
                    end;
                _ ->
                    {error, #{
                        <<"appname">> => AppName,
                        <<"filename">> => FileApp
                    }
                    }
            end;
        _ -> {error, <<"not find">>}
    end;

%% Product 概要: 导库 描述:json文件导库
%% OperationId:post_menus
%% 请求:POST /iotapi/post_menu
do_request(post_menu, #{<<"file">> := FileInfo} = _Body,
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "FileInfo ~p", [FileInfo]),
    case maps:get(<<"contentType">>, FileInfo, <<"unknow">>) of
        <<"application/x-zip-compressed">> -> post_menu(FileInfo, SessionToken);
        <<"application/zip">> -> post_menu(FileInfo, SessionToken);
        ContentType ->
            {error, <<"contentType error, contentType:", ContentType/binary>>}
    end;

%% Relation 概要: 增加关系 描述:json文件导库
%% OperationId:post_relation
%% 请求:POST /iotapi/relation
do_request(post_relation, #{<<"destClass">> := DestClass, <<"destId">> := DestId, <<"destField">> := DestField,
    <<"srcClass">> := SrcClass, <<"srcId">> := SrcId} = _Body, _Context, _Req) ->
    Map =
        #{DestField =>
        #{
            <<"__op">> => <<"AddRelation">>,
            <<"objects">> => [
                #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => SrcClass,
                    <<"objectId">> => SrcId
                }
            ]
        }
        },
    dgiot_parse:update_object(DestClass, DestId, Map);

%% Relation 概要: 删除关系 描述:json文件导库
%% OperationId:post_relation
%% 请求:DELETE /iotapi/relation
do_request(delete_relation, #{<<"destClass">> := DestClass, <<"destId">> := DestId, <<"destField">> := DestField,
    <<"srcClass">> := SrcClass, <<"srcId">> := SrcId} = _Body, _Context, _Req) ->
    Map =
        #{DestField =>
        #{
            <<"__op">> => <<"RemoveRelation">>,
            <<"objects">> => [
                #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => SrcClass,
                    <<"objectId">> => SrcId
                }
            ]
        }
        },
    dgiot_parse:update_object(DestClass, DestId, Map);

%% Product 概要: 导库 描述:json文件导库
%% OperationId:post_tree
%% 请求:POST /iotapi/post_tree
do_request(post_tree, #{<<"class">> := Class, <<"parent">> := Parent, <<"filter">> := Filter}, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    dgiot_parse_handler:get_classtree(Class, Parent, jsx:decode(Filter, [{labels, binary}, return_maps]), SessionToken);

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    ?LOG(info, "_Args ~p", [_Args]),
    {error, <<"Not Allowed.">>}.

get_class(Name, Filter, FileName, SessionToken) ->
    case dgiot_parse:query_object(Name, Filter,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Data}} ->
            NewData = lists:foldl(fun(X, Acc) ->
                Acc ++ [maps:without([<<"createdAt">>, <<"updatedAt">>, <<"children">>], X)]
                                  end, [], Data),
            BinFile = unicode:characters_to_binary(jsx:encode(NewData)),
            case zip:create(FileName, [{"data.json", BinFile}], [memory]) of
                {ok, {_ZipFile, Bin}} ->
                    {ok, Bin};
                {error, What} ->
                    {error, What}
            end;
        _ -> {error, <<"not find">>}
    end.

get_product(Name, FileName, SessionToken) ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{<<"name">> => Name}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [Product]}} ->
            BinFile = unicode:characters_to_binary(jsx:encode(Product)),
            case zip:create(FileName, [{"product.json", BinFile}], [memory]) of
                {ok, {_ZipFile, Bin}} ->
                    {ok, Bin};
                {error, What} ->
                    {error, What}
            end;
        _ -> {error, <<"not find">>}
    end.

post_product(FileInfo, Appid, SessionToken) ->
    Dir = list_to_binary(dgiot_http_server:get_env(?APP, docroot)),
    #{<<"path">> := <<"/", Path/binary>>} = FileInfo,
    FilePath = filename:join([Dir, Path]),
    case zip:unzip(unicode:characters_to_list(FilePath), [memory]) of
        {ok, [{_, Bin}]} ->
            case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
                {'EXIT', Reason1} ->
                    {error, Reason1};
                #{<<"name">> := ProductName} = Product ->
                    case catch dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{<<"name">> => ProductName}},
                        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                        {ok, #{<<"results">> := []}} ->
                            NewProduct =
                                Product#{
                                    <<"ACL">> => #{
                                        <<"role:", Appid/binary>> => #{<<"read">> => true, <<"write">> => true
                                        }
                                    }
                                },
                            case catch dgiot_parse:create_object(<<"Product">>,
                                maps:without([<<"createdAt">>, <<"updatedAt">>, <<"children">>], NewProduct),
                                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                                {ok, Result} ->
                                    {200, Result};
                                {'EXIT', Reason2} ->
                                    {error, Reason2}
                            end;
                        {ok, _} -> {error, #{<<"result">> => <<"product exist">>}};
                        {'EXIT', Reason3} ->
                            {error, Reason3}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

post_menu(FileInfo, SessionToken) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"roles">> := _Roles} ->
            Dir = list_to_binary(dgiot_http_server:get_env(?APP, docroot)),
            #{<<"path">> := <<"/", Path/binary>>} = FileInfo,
            FilePath = filename:join([Dir, Path]),
            case zip:unzip(dgiot_utils:to_list(FilePath), [memory]) of
                {ok, [{_, Bin}]} ->
                    case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
                        {'EXIT', Reason1} ->
                            {error, Reason1};
                        Menus ->
                            {ok, dgiot_install:save_menu(<<"0">>, Menus, #{})}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ -> {error, <<"not auth">>}
    end.

get_json_from_zip(FileInfo) ->
    Dir = list_to_binary(dgiot_http_server:get_env(?APP, docroot)),
    #{<<"path">> := <<"/", Path/binary>>} = FileInfo,
    FilePath = filename:join([Dir, Path]),
    case zip:unzip(dgiot_utils:to_list(FilePath), [memory]) of
        {ok, [{_, Bin}]} ->
            case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
                {'EXIT', Reason1} ->
                    {error, Reason1};
                Json ->
                    {ok, Json}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Thing 概要: 导库 描述:查询物模型
%% OperationId:get_thing
%% 请求:GET /iotapi/get_thing
getThing(ProductId, ModuleType, SessionToken) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"thing">> := Thing}} ->
            Modules = maps:get(ModuleType, Thing, []),
            {_, Maps} =
                lists:foldl(fun(X, {Num, Acc}) ->
                    case X of
                        #{<<"updateAt">> := UpdateAt} ->
                            BinUpdateAt = dgiot_utils:to_binary(UpdateAt),
                            {Num, Acc#{BinUpdateAt => X}};
                        _ ->
                            BinUpdateAt = dgiot_utils:to_binary(1577854035000 + Num),
                            {Num + 1, Acc#{BinUpdateAt => X}}
                    end
                            end, {0, #{}}, Modules),
            Keys = maps:keys(Maps),
            NewModules =
                lists:foldl(fun(X, Acc) ->
                    Acc ++ [maps:get(X, Maps)]
                            end, [], lists:sort(Keys)),
            {ok, NewModules};
        Error ->
            {error, Error}
    end.

%% Thing 概要: 导库 描述:添加物模型
%% OperationId:post_thing
%% 请求:POST /iotapi/post_thing
postThing(ProductId, Item, SessionToken) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"thing">> := Thing}} ->
            ModuleType = maps:get(<<"moduleType">>, Item, <<"properties">>),
            Properties = maps:get(<<"properties">>, Thing, []),
            #{<<"identifier">> := Identifier} = Item,
            {Ids, NewProperties} =
                lists:foldl(fun(X, {Ids1, Acc}) ->
                    case X of
                        #{<<"identifier">> := Identifier} ->
                            {Ids1 ++ [Identifier], Acc};
                        _ ->
                            {Ids1, Acc ++ [X]}
                    end
                            end, {[], [Item]}, Properties),
            case length(Ids) == 0 of
                true ->
                    {_, R} = dgiot_parse:update_object(<<"Product">>, ProductId,
                        #{<<"thing">> => Thing#{ModuleType => NewProperties}},
                        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
                    {ok, R#{<<"code">> => 200}};
                false ->
                    {ok, #{<<"code">> => 204, <<"msg">> => <<Identifier/binary, " already existed">>}}
            end;
        Error ->
            {error, Error}
    end.

%% Thing 概要: 导库 描述:更新模型
%% OperationId:put_thing
%% 请求:PUT /iotapi/put_thing
putTing(ProductId, Item, SessionToken) ->
    ModuleType = maps:get(<<"moduleType">>, Item, <<"properties">>),
    #{<<"identifier">> := Identifier} = Item,
    case dgiot_parse:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"thing">> := Thing}} ->
            Properties = maps:get(ModuleType, Thing, []),
            NewProperties =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"identifier">> := Identifier} ->
                            Acc ++ [Item];
                        _ ->
                            Acc ++ [X]
                    end
                            end, [], Properties),
            {_, R} = dgiot_parse:update_object(<<"Product">>, ProductId,
                #{<<"thing">> => Thing#{ModuleType => NewProperties}},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
            {ok, R#{<<"code">> => 200}};
        Error ->
            {error, Error}
    end.

%% Thing 概要: 导库 描述:删除模型
%% OperationId:delete_thing
%% 请求:DELETE /iotapi/delete_thing
deleteThing(ProductId, SessionToken, Item) ->
    #{<<"identifier">> := Identifier} = Item,
    ModuleType = maps:get(<<"moduleType">>, Item, <<"properties">>),
    case dgiot_parse:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"thing">> := Thing}} ->
            Modules = maps:get(ModuleType, Thing),
            {Ids, NewModules} =
                lists:foldl(fun(X, {Ids1, Acc}) ->
                    case X of
                        #{<<"identifier">> := Identifier} ->
                            {Ids1, Acc};
                        #{<<"identifier">> := Identifier1, <<"dataForm">> := #{<<"collection">> := Collection}} ->
                            case binary:match(Collection, [Identifier]) of
                                nomatch ->
                                    {Ids1, Acc ++ [X]};
                                _ ->
                                    case Ids1 of
                                        [] ->
                                            {Ids1 ++ [Identifier1], Acc};
                                        _ ->
                                            {Ids1 ++ [<<",", Identifier1/binary>>], Acc}
                                    end
                            end;
                        _ ->
                            {Ids1, Acc ++ [X]}
                    end
                            end, {[], []}, Modules),
            case length(Ids) == 0 of
                true ->
                    {_, R} = dgiot_parse:update_object(<<"Product">>, ProductId,
                        #{<<"thing">> => Thing#{ModuleType => NewModules}},
                        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
                    {ok, R#{<<"code">> => 200}};
                false ->
                    BinIds = dgiot_utils:to_binary(Ids),
                    {ok, #{<<"code">> => 204, <<"msg">> => <<BinIds/binary, " use ", Identifier/binary>>}}
            end;
        Error ->
            {error, Error}
    end.
