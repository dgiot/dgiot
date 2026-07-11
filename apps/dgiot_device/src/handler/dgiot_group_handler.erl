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

-module(dgiot_group_handler).
-author("dgiot").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_group/0]).
-export([handle/4, post_group/2]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/pump">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_group.json">>, ?MODULE, [], priv)
swagger_group() ->
    [
        dgiot_http_server:bind(<<"/swagger_group.json">>, ?MODULE, [], priv)
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
            ?LOG(info, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_utils:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            ?LOG(debug, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.

%% 设备迁移列表
do_request(post_dev_transfer, #{<<"objectId">> := DeviceId, <<"rolename">> := RoleName} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"name">> := DevName, <<"devaddr">> := Devaddr}} ->
            #{<<"nick">> := Nick} = dgiot_auth:get_session(SessionToken),
            OldRole = dgiot_device:get_appname(DeviceId),
            Msg = <<Nick/binary, " 将设备从部门【"/utf8, OldRole/binary, "】转移到新部门【"/utf8, RoleName/binary, "】"/utf8>>,
            dgiot_parsex:create_object(<<"Log">>, #{
                <<"deviceid">> => DeviceId
                , <<"msg">> => Msg
                , <<"domain">> => [<<"DeviceTransferlist">>]
                , <<"type">> => <<"DeviceTransferlist">>
                , <<"mfa">> => RoleName
                , <<"line">> => 1
                , <<"topic">> => OldRole
                , <<"peername">> => DevName
                , <<"devaddr">> => Devaddr
            }),
            {ok, #{<<"data">> => <<"ok">>, <<"msg">> => Msg, <<"status">> => 0}};
        _ ->
            {ok, #{<<"msg">> => <<"not find device">>, <<"status">> => 0}}
    end;

%% 设备迁移拒绝
do_request(post_dev_transfer, #{<<"logid">> := LogId, <<"status">> := false} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_parsex:get_object(<<"Log">>, LogId) of
        {ok, #{<<"deviceid">> := DeviceId, <<"mfa">> := RoleName, <<"topic">> := OldRole}} ->
            #{<<"nick">> := Nick} = dgiot_auth:get_session(SessionToken),
            Msg = <<Nick/binary, " 拒绝 将设备从部门【"/utf8, OldRole/binary, "】转移到新部门【"/utf8, RoleName/binary, "】"/utf8>>,
            dgiot_parsex:update_object(<<"Log">>, LogId, #{
                <<"deviceid">> => DeviceId
                , <<"msg">> => Msg
                , <<"line">> => 3
                , <<"time">> => dgiot_datetime:nowstamp()
            });
        _ ->
            {ok, #{<<"msg">> => <<"not find log">>, <<"status">> => 0}}
    end;

%% 设备迁移
do_request(post_dev_transfer, #{<<"logid">> := LogId} = _Args, _Context, _Req) ->
    case dgiot_parsex:get_object(<<"Log">>, LogId) of
        {ok, #{<<"deviceid">> := DeviceId, <<"mfa">> := RoleName}} ->
            ACL = #{<<"role:", RoleName/binary>> => #{<<"read">> => true, <<"write">> => true}},
            case dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"ACL">> => ACL}) of
                {ok, Data} ->
                    dgiot_parsex:update_object(<<"Log">>, LogId, #{
                        <<"deviceid">> => DeviceId
                        , <<"line">> => 2
                        , <<"time">> => dgiot_datetime:nowstamp()
                    }),
                    dgiot_device:put(#{<<"objectId">> => DeviceId, <<"ACL">> => ACL}),
                    {ok, #{<<"data">> => Data, <<"status">> => 0}};
                {_, Data} ->
                    {ok, #{<<"data">> => Data, <<"msg">> => <<"error">>, <<"status">> => 0}}
            end;
        _ ->
            {ok, #{<<"msg">> => <<"not find log">>, <<"status">> => 0}}
    end;


do_request(get_sports, _Args, _Context, _Req) ->
    Items =
        case dgiot_parsex:query_object(<<"Device">>, #{<<"where">> => #{<<"product">> => <<"2dd1d8b42a">>}}) of
            {ok, #{<<"results">> := Devices}} ->
                lists:foldl(fun(#{
                    <<"objectId">> := ObjectId,
                    <<"name">> := Name,
                    <<"state">> := State,
                    <<"realstate">> := Realstate
                }, Acc) ->
                    Acc ++ [#{
                        <<"objectId">> => ObjectId,
                        <<"nick">> => Name,
                        <<"status">> => State,
                        <<"team">> => Realstate
                    }]
                            end, [], Devices);
            _ ->
                []
        end,
    {ok, #{<<"objectId">> => <<"objectid">>, <<"data">> => #{<<"items">> => Items},
        <<"msg">> => <<"ok">>,
        <<"status">> => 0}};


do_request(post_sports, #{<<"ids">> := Ids, <<"type">> := Type} = _Args, _Context, _Req) ->
%%    io:format("Args ~p~n", [Args]),
    lists:foldl(fun(DeviceId, _Acc) ->

        dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"realstate">> => Type})

                end, [], binary:split(Ids, <<$,>>, [global, trim])),

    {ok, #{<<"data">> => #{}}};

%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================
%% group 概要: 获取组设备 描述:获取组设备
%% OperationId:get_group_id
%% 请求:get /iotapi/get_group_id
do_request(get_group_id, #{<<"id">> := Id},
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "Id ~p", [Id]),
    case dgiot_parsex:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"product">> => Id}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [Device | _]}} ->
            #{<<"devaddr">> := Devaddr, <<"objectId">> := ParentId} = Device,
            Result =
                lists:foldl(fun(#{<<"devaddr">> := SubDevAddr, <<"objectId">> := SubDeviceId, <<"product">> := SubProduct}, Acc) ->
                    New = maps:with([<<"thing">>, <<"name">>], SubProduct),
                    Acc ++ [New#{
                        <<"parentid">> => ParentId,
                        <<"subdevaddr">> => SubDevAddr,
                        <<"subprodid">> => maps:get(<<"objectId">>, SubProduct),
                        <<"subdevid">> => SubDeviceId
                    }]
                            end, [], dgiot_device:get_sub_device(Devaddr, SessionToken)),
            {ok, #{<<"results">> => Result}};
        Error -> Error
    end;

%% group 概要: 标识采样点 描述:标识采样点
%% OperationId:post_group
%% 请求:post /iotapi/post_group
do_request(put_group, #{<<"thing">> := Thing, <<"topo">> := Topo}, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "Topo ~p Thing ~p ", [Topo, Thing]),
    put_group(maps:merge(Thing, Topo), SessionToken);

%% group 概要: 标识采样点 描述:标识采样点
%% OperationId:put_group
%% 请求:put /iotapi/put_group
do_request(post_group, #{<<"name">> := _Name, <<"devType">> := _DevType} = Body,
    #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    post_group(Body, SessionToken);


%% group 概要: 标识采样点 描述:标识采样点
%% OperationId:delete_group
%% 请求:delete /iotapi/delete_group
do_request(delete_group, #{<<"name">> := _Name, <<"devType">> := _DevType} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "Body ~p ", [Body]),
    delete_group(Body, SessionToken);

do_request(get_product_statistics, #{<<"type">> := Type} = _Args, #{<<"sessionToken">> := SessionToken}, _Body) ->
%%    Data = get_statistics(SessionToken),
    Data = dgiot_product:get_product_statistics(Type, SessionToken),
    {ok, #{<<"status">> => 0, msg => <<"ok">>, <<"data">> => Data}};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

put_group(#{<<"productid">> := ProductId, <<"topoid">> := TopoId, <<"thingid">> := ThingId} = Payload, SessionToken) ->
    case dgiot_parsex:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"config">> := Config, <<"thing">> := #{<<"properties">> := Properties}}} when length(Properties) > 0 ->
            ControlList =
                lists:foldl(fun(X, Acc) ->
                    case maps:with([<<"identifier">>, <<"style">>], X) of
                        #{<<"identifier">> := TopoId, <<"style">> := Style} ->
                            R0 = lists:foldl(fun(Y, Acc1) ->
                                case Y of
                                    #{<<"identifier">> := ThingId, <<"dataType">> := #{<<"type">> := <<"string">>}} ->
                                        Acc1 ++ [X#{
                                            <<"productId">> => ProductId,
                                            <<"wumoxing">> => #{
                                                <<"identifier">> => ThingId,
                                                <<"value">> => ""
                                            },
                                            <<"style">> => Style#{
                                                <<"backColor">> => <<"rgba(46,176,80,1)">>}}];
                                    #{<<"identifier">> := ThingId, <<"dataType">> := #{<<"specs">> := Specs}} ->
                                        Acc1 ++ [X#{
                                            <<"productId">> => ProductId,
                                            <<"wumoxing">> => Specs#{
                                                <<"identifier">> => ThingId,
                                                <<"value">> => ""
                                            },
                                            <<"style">> => Style#{
                                                <<"backColor">> => <<"rgba(46,176,80,1)">>}}];
                                    _ -> Acc1
                                end
                                             end, [], Properties),
                            case R0 of
                                [] -> Acc ++ [X];
                                L0 -> Acc ++ L0
                            end;
                        _ -> Acc ++ [X]
                    end
                            end, [], maps:get(<<"components">>, Config)),
            NewProperties =
                lists:foldl(fun(X, Acc) ->
                    case maps:with([<<"identifier">>], X) of
                        #{<<"identifier">> := ThingId} ->
                            R = lists:foldl(fun(Y, Acc1) ->
                                case Y of
                                    #{<<"identifier">> := TopoId,
                                        <<"address">> := Address} ->
                                        Acc1 ++ [X#{<<"dataForm">> => #{
                                            <<"address">> => Address,
                                            <<"quantity">> => TopoId}}];
                                    _ ->
                                        Acc1
                                end
                                            end, [], Properties),
                            case R of
                                [] -> Acc ++ [X];
                                L -> Acc ++ L
                            end;
                        _ -> Acc ++ [X]
                    end
                            end, [], Properties),
            case dgiot_parsex:update_object(<<"Product">>, ProductId, #{
                <<"config">> => Config#{<<"components">> => ControlList},
                <<"thing">> => #{<<"properties">> => NewProperties}}) of
                {ok, _} -> {ok, #{
                    <<"config">> => Config#{<<"components">> => ControlList},
                    <<"thing">> => #{<<"properties">> => NewProperties}}
                };
                Error -> Error
            end;
        {ok, #{<<"config">> := Config}} ->
            ControlList =
                lists:foldl(fun(X, Acc) ->
                    case maps:with([<<"identifier">>, <<"style">>], X) of
                        #{<<"identifier">> := TopoId, <<"style">> := Style} ->
                            Specs = maps:get(<<"wumoxing">>, X, #{}),
                            Acc ++ [X#{
                                <<"productId">> => ProductId,
                                <<"wumoxing">> => Specs#{
                                    <<"identifier">> => ThingId,
                                    <<"subprodid">> => maps:get(<<"subprodid">>, Payload, <<"">>),
                                    <<"subdevid">> => maps:get(<<"subdevid">>, Payload, <<"">>),
                                    <<"value">> => ""
                                },
                                <<"style">> => Style#{
                                    <<"backColor">> => <<"rgba(46,176,80,1)">>}}];
                        _ -> Acc ++ [X]
                    end
                            end, [], maps:get(<<"components">>, Config)),
            case dgiot_parsex:update_object(<<"Product">>, ProductId, #{
                <<"config">> => Config#{<<"components">> => ControlList}}) of
                {ok, _} -> {ok, #{
                    <<"config">> => Config#{<<"components">> => ControlList}}
                };
                Error -> Error
            end;
        Return ->
            ?LOG(info, "Return ~p", [Return]),
            Return
    end.

post_group(Body, SessionToken) ->
%%    HostName = dgiot_utils:get_hostname(),
    NatIP = dgiot_utils:get_natip(),
    ComputerKey = dgiot_utils:get_macs(),
    <<Addr:12/binary, _/binary>> = ComputerKey,
    ProductName = case maps:get(<<"name">>, Body, <<"">>) of
                      <<"">> ->
                          Addr;
                      Name -> Name
                  end,
    NewBody = maps:without([<<"topo">>], Body),
    Acl = case dgiot_auth:get_session(SessionToken) of
              #{<<"roles">> := Roles} = _User ->
                  [#{<<"name">> := Role} | _] = maps:values(Roles),
                  #{<<"role:", Role/binary>> => #{
                      <<"read">> => true,
                      <<"write">> => true}
                  };
              Err -> {400, Err}
          end,
    Body1 = NewBody#{
        <<"desc">> => <<"DGIOT设备分组"/utf8>>,
        <<"netType">> => <<"WIFI">>,
        <<"category">> => #{<<"objectId">> => <<"e5a9059441">>, <<"__type">> => <<"Pointer">>, <<"className">> => <<"Category">>},
        <<"config">> => #{},
%%        为什么以下id写死呀？
        <<"channel">> => #{<<"type">> => 1, <<"tdchannel">> => <<"24b9b4bc50">>, <<"taskchannel">> => <<"0edaeb918e">>, <<"otherchannel">> => [<<"11ed8ad9f2">>]},
        <<"thing">> => #{},
        <<"ACL">> => Acl,
        <<"name">> => ProductName,
        <<"nodeType">> => 2},
%%    io:format("NewBody ~p~n", [Body1]),

    case dgiot_product:create_product(Body1, SessionToken) of
        {_, #{<<"objectId">> := ProductId}} ->
            <<NewAddr:12/binary, _/binary>> = dgiot_utils:to_md5(<<ProductId/binary, Addr/binary>>),
            CreateArgs = #{
                <<"status">> => <<"ONLINE">>,
                <<"devaddr">> => NewAddr,
                <<"name">> => ProductName,
                <<"ip">> => NatIP,
                <<"brand">> => <<"DGIOT分组设备"/utf8>>,
                <<"devModel">> => <<"DGIOT_GROUP">>,
                <<"product">> => ProductId,
                <<"ACL">> => Acl},
%%            io:format("CreateArgs ~p~n", [CreateArgs]),
            dgiot_device:create_device(CreateArgs);
        Error ->
            Error
    end.

delete_group(#{<<"name">> := _Name, <<"devType">> := _DevType} = Where, SessionToken) ->
    case dgiot_parsex:query_object(<<"Product">>, #{<<"where">> => Where, <<"limit">> => 1}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ProductId} | _]}} ->
            case dgiot_parsex:query_object(<<"Device">>, #{<<"where">> => Where, <<"limit">> => 1}) of
                {ok, #{<<"results">> := [#{<<"objectId">> := DeviceId} | _]}} ->
                    dgiot_parsex:del_object(<<"Device">>, DeviceId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
                _ -> pass
            end,
            dgiot_parsex:del_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        Error -> Error
    end.
