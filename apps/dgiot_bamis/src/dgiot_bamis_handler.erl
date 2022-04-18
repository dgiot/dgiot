%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_bamis_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_amis/0]).
-export([handle/4]).
%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/amis">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_amis.json">>, ?MODULE, [], priv)
swagger_amis() ->
    [
        dgiot_http_server:bind(<<"/swagger_bamis.json">>, ?MODULE, [], priv)  %需要于priv/swagger/目录下放置swagger.json文件名保持一致
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


%%%===================================================================
%%% 所有配置在 amis 组件中的接口，都要符合下面的返回格式
%% 适配amis接口参数
%% {
%%   "status": 0,
%%   "msg": "",
%%   "data": {
%%     ...其他字段
%%   }
%% }
%% status: 返回 0，表示当前接口正确返回，否则按错误请求处理；
%% msg: 返回接口处理信息，主要用于表单提交或请求失败时的 toast 显示；
%% data: 必须返回一个具有 key-value 结构的对象。
%% https://aisuda.bce.baidu.com/amis/zh-CN/docs/types/api#%E6%8E%A5%E5%8F%A3%E8%BF%94%E5%9B%9E%E6%A0%BC%E5%BC%8F-%E9%87%8D%E8%A6%81-
%%%===================================================================

%% iot_hub 概要: 大屏数据任务推送 描述:启动任务推送大屏数据
%% OperationId:post_dashboard
%% 请求:POST /iotapi/post_dashboard
do_request(post_dashboard, Arg, Context, _Req) ->
    dgiot_dashboard:post_dashboard(Arg, Context),
    {200, <<"success">>};

do_request(post_big_screen, Args, Context, _Req) ->
    dgiot_dashboard:post_dashboard(Args, Context),
    {200, <<"success">>};

%% iot_hub 概要: 查询平台api资源 描述:总控台
%% OperationId:get_amis
%% 请求:POST /iotapi/get_amis
do_request(get_amis, #{<<"deviceid">> := Deviceid}, _Context, _Req) ->
    case dgiot_parse:get_object(<<"Device">>, Deviceid) of
        {ok, #{<<"profile">> := Profile}} ->
            {ok, Profile};
        _ ->
            {error, #{}}
    end;

%% iot_hub 概要: 查询amis设备
%% OperationId:get_amis
%% 请求:POST /iotapi/get_amis_device
do_request(get_amis_device, #{<<"deviceid">> := Deviceid}, _Context, _Req) ->
    case dgiot_parse:get_object(<<"Device">>, Deviceid) of
        {ok, Info} ->
            {ok, #{<<"status">> => 0, <<"msg">> => <<"success">>, <<"data">> => Info}};
        _ ->
            {error, #{<<"status">> => 404, <<"msg">> => <<"error">>, <<"result">> => <<"deviec info null">>}}
    end;

%% Role模版 概要: 修改amis设备
%% OperationId:put_amis_device
%% 请求:POST /iotapi/put_amis_device
do_request(put_amis_device, Body, #{<<"sessionToken">> := SessionToken}, _Req0) ->
%%    io:format("Body ~p~n", [Body]),
    case dgiot_bamis:put_amis_device(Body, SessionToken) of
        {ok, Info} ->
            {ok, Info};
        _ ->
            {error, #{<<"code">> => 404, <<"result">> => <<"device info null">>}}
    end;

%% iot_hub 概要: 删除amis设备
%% OperationId:del_amis_device
%% 请求:POST /iotapi/del_amis_device
do_request(delete_amis_device, #{<<"deviceid">> := DeviceId} = _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_parse:del_object(<<"Device">>, DeviceId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        ok ->
            {ok, #{
                <<"status">> => 200,
                <<"msg">> => "delete success"
            }};
        _ ->
            {ok, #{<<"status">> => 200, <<"msg">> => "delete error"}}
    end;

%% iot_hub 概要: 创建amis设备
%% OperationId:created_amis_device
%% 请求:POST /iotapi/created_amis_device
do_request(post_amis_device, #{<<"deviceid">> := Deviceid, <<"ChannelId">> := ChannelId, <<"DTUIP">> := DTUIP} = _Body, _Context, _Req) ->
    case dgiot_bamis:created_amis_device(Deviceid, ChannelId, DTUIP) of
        {ok, Info} ->
            {ok, Info};
        _ ->
            {error, #{<<"code">> => 404, <<"result">> => <<"device info null">>}}
    end;

do_request(post_update_product, _Body, _Context, _Req) ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{}}) of
        {ok, #{<<"results">> := Products}} ->
            io:format("~s ~p Products = ~p.~n", [?FILE, ?LINE, Products]),
            lists:foldl(fun(Product, _Acc) ->
                case Product of
                    #{<<"objectId">> := ProductId, <<"thing">> := #{<<"properties">> := Properties} = _Thing} ->
                        NewProperties =
                            lists:foldl(fun(X, Acc) ->
                                case X of
                                    #{<<"dataForm">> := #{<<"protocol">> := <<"modbus">>, <<"data">> := Data, <<"address">> := Address,
                                        <<"slaveid">> := Slaveid, <<"operatetype">> := Operatetype, <<"originaltype">> := Originaltype} = DataForm} ->
                                        Acc ++ [X#{
                                            <<"dataForm">> => maps:without([<<"address">>, <<"data">>, <<"slaveid">>, <<"operatetype">>, <<"originaltype">>],
                                                DataForm#{<<"protocol">> => <<"MODBUSRTU">>}),
                                            <<"dataSource">> => #{
                                                <<"data">> => Data,
                                                <<"address">> => Address,
                                                <<"slaveid">> => Slaveid,
                                                <<"_dlinkindex">> => 0,
                                                <<"operatetype">> => Operatetype,
                                                <<"originaltype">> => Originaltype,
                                                <<"registersnumber">> => 0
                                            }
                                        }];
                                    _ ->
                                        Acc ++ [X]
                                end
                                        end, [], Properties),
                        dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"thing">> => #{<<"properties">> => NewProperties}});
                    _ ->
                        pass
                end
                        end, [], Products);
        _Error ->
            {error, #{<<"code">> => 404, <<"result">> => <<"device info null">>}}
    end;

do_request(get_big_screen, _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_dashboard:dashboard(SessionToken) of
        {ok, Info} ->
            {ok, #{
                <<"status">> => 200,
                <<"msg">> => <<"success">>,
                <<"data">> => Info
            }};
        _ ->
            {error, #{<<"code">> => 404, <<"result">> => <<"product info null">>}}
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    ?LOG(info, "_OperationId:~p~n", [_OperationId]),
    {error, <<"Not Allowed.">>}.
