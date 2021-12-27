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
-module(dgiot_location_handler).
-author("johnliu").
-behavior(dgiot_rest).
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_location.hrl").
-dgiot_rest(all).
%% API
-export([swagger_meter/0]).
-export([handle/4, check_auth/3]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/meter">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_meter.json">>, ?MODULE, [], priv)
swagger_meter() ->
    [
        dgiot_http_server:bind(<<"/swagger_meter.json">>, ?MODULE, [], priv)
    ].

check_auth(_OperationID, _Args, Req) ->
    {true, #{}, Req}.

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


%% TDengine 概要: 获取当前产品下的所有设备数据 描述:获取当前产品下的所有设备数据
%% OperationId:get_td_cid_pid
%% 请求:GET /iotapi/td/prodcut/:productId
do_request(get_iq60_ctrl, #{
    <<"pid">> := ProductId,
    <<"devaddr">> := DevAddr,
    <<"ctrlflag">> := CtrlFlag,
    <<"devpass">> := DevPass
}, _Context, Req0) ->
    get_iq60_ctrl(Req0, ProductId, DevAddr, CtrlFlag, DevPass);

do_request(get_iq60_ctrl_status, #{
    <<"pid">> := ProductId,
    <<"ctrlflag">> := CtrlFlag,
    <<"devaddr">> := DevAddr
}, _Context, _Req) ->
    TopicCtrl = <<"thingctrl/", ProductId/binary, "/", DevAddr/binary>>,
    ThingData = #{<<"devaddr">> => DevAddr, <<"ctrlflag">> => CtrlFlag, <<"apiname">> => get_iq60_ctrl_status},
    Payload = [#{<<"appdata">> => #{}, <<"thingdata">> => ThingData}],
    dgiot_mqtt:publish(DevAddr, TopicCtrl, jsx:encode(Payload));


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    ?LOG(info, "_OperationId:~p~n", [_OperationId]),
    {error, <<"Not Allowed.">>}.

get_iq60_ctrl(Req0, ProductId, DevAddr, CtrlFlag, DevPass) ->
    Sendtopic = <<"thingctrl/", ProductId/binary, "/", DevAddr/binary>>,
    ThingData = #{<<"devaddr">> => DevAddr, <<"ctrlflag">> => CtrlFlag, <<"devpass">> => DevPass, <<"apiname">> => get_iq60_ctrl},
    Payload = [#{<<"pid">> => self(),<<"appdata">> => #{}, <<"thingdata">> => ThingData}],
    case dgiot_mqtt:has_routes(Sendtopic) of
        true ->
            dgiot_mqtt:publish(DevAddr, Sendtopic, jsx:encode(Payload)),
            receive
                {ctrl_meter, Msg} ->
                    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Msg), Req0),
                    {ok, Resp};
                {error} ->
                    Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"CTRL_METER_FAILED">>, Req0),
                    {ok, Resp}
            after 10000 ->
                Resp = cowboy_req:reply(200, #{
                    <<"content-type">> => <<"text/plain">>
                }, <<"CTRL_METER_TIMEOUT">>, Req0),
                {ok, Resp}
            end;
        false ->
            Resp = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"METER_OFFLINE">>, Req0),
            {ok, Resp}
    end.





