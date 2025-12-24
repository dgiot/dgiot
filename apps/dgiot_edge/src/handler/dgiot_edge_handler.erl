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
-module(dgiot_edge_handler).
-author("stoneliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_edge/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/pump">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_feeders.json">>, ?MODULE, [], priv)
swagger_edge() ->
    [
        dgiot_http_server:bind(<<"/swagger_edge.json">>, ?MODULE, [], priv)
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
                      false ->
                          dgiot_ctl:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
%%            ?LOG(debug,"do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================
%% 获取串口
%%  ls -l /dev/ttyUSB*
%%  echo /dev/ttyUSB*
%%  "/dev/ttyUSB1\n/dev/ttyUSB2\n/dev/ttyUSB3\n"
%%  dgiot_utils:to_binary(os:cmd("cd /dev \n ls usb_*")).
do_request(get_ttyusbs, _Args, _Context, _Req) ->
    Usb = dgiot_utils:to_binary(os:cmd("cd /dev \n ls usb_*")),
    L =
        case binary:split(Usb, <<$\n>>, [global, trim]) of
            Usbs when length(Usbs) > 0 ->
                Usbs;
            _ ->
                []
        end,
    {ok, #{<<"result">> => L}};

%% 发送串口数据
%% 自动回复
do_request(post_edge_write, #{<<"serialport">> := Serialport, <<"action">> := <<"autoresv">>, <<"messagetype">> := Messagetype, <<"data">> := Data} = _Args, _Context, _Req) ->
%%    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, _Args]),
    NewData = dgiot_edge:get_writeData(Messagetype, Data),
    dgiot_data:insert({autoresv, Serialport}, {true, NewData}),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success"/utf8>>}};

%% 停止
do_request(post_edge_write, #{<<"serialport">> := Serialport, <<"action">> := <<"disable">>} = _Args, _Context, _Req) ->
%%    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, _Args]),
    edge_worker:stop(Serialport),
    dgiot_data:delete({autoresv, Serialport}),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success"/utf8>>}};

%% frequency 0 只发一次
do_request(post_edge_write, #{<<"frequency">> := 0, <<"serialport">> := Serialport, <<"messagetype">> := Messagetype, <<"data">> := Data} = _Args, _Context, _Req) ->
    dgiot_data:delete({autoresv, Serialport}),
    NewData = dgiot_edge:get_writeData(Messagetype, Data),
    case dgiot_serial_client:write(self(), Serialport, NewData) of
        pass ->
            {ok, #{<<"status">> => 0, <<"msg">> => <<"串口未连接"/utf8>>}};
        _ ->
            {ok, #{<<"status">> => 0, <<"msg">> => <<"success"/utf8>>}}
    end;

%% 循环发送
do_request(post_edge_write, #{<<"action">> := <<"enable">>, <<"serialport">> := Serialport} = Args, _Context, _Req) ->
%%    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, Args]),
    supervisor:start_child(edge_task, [Args]),
    dgiot_data:delete({autoresv, Serialport}),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success"/utf8>>}};

%% 统计
%% {dgiot_data:get({package_write_count,<<"usb5">>}), dgiot_data:get({package_recv_count,<<"usb5">>}), dgiot_data:get({package_write_count,<<"usb7">>}), dgiot_data:get({package_recv_count,<<"usb7">>})}.
do_request(get_package_statistics, #{<<"serialport">> := Serialport}, _Context, _Req) ->
    {ok, #{<<"data">> => #{
        <<"write_count">> => dgiot_metrics:get(dgiot_edge, <<Serialport/binary, "_send">>),
        <<"write_bytes">> => dgiot_metrics:get(dgiot_edge, <<Serialport/binary, "_send_bytes">>),
        <<"recv_count">> => dgiot_metrics:get(dgiot_edge, <<Serialport/binary, "_recv">>),
        <<"recv_bytes">> => dgiot_metrics:get(dgiot_edge, <<Serialport/binary, "_recv_bytes">>)},
        <<"status">> => 0, <<"msg">> => <<"success"/utf8>>}};

%% 清零
do_request(get_zero_clearing, #{<<"serialport">> := _Serialport}, _Context, _Req) ->
%%    dgiot_metrics:dec(dgiot_edge, <<Serialport/binary, "_send">>, 0),
%%    dgiot_metrics:dec(dgiot_edge, <<Serialport/binary, "_send_bytes">>, 0),
%%    dgiot_metrics:dec(dgiot_edge, <<Serialport/binary, "_recv">>, 0),
%%    dgiot_metrics:dec(dgiot_edge, <<Serialport/binary, "_recv_bytes">>, 0),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success"/utf8>>}};

do_request(get_serial_info_deviceid, #{<<"deviceid">> := DeviceId, <<"serialport">> := Serialport}, _Context, _Req) ->
    Data =
        case dgiot_parse:get_object(<<"Device">>, DeviceId) of
            {ok, #{<<"content">> := Content}} ->
                maps:get(Serialport, Content, #{});
            _ ->
                #{}
        end,
    {ok, #{<<"data">> => Data, <<"status">> => 0, <<"msg">> => <<"success">>}};

%%     dgiot_data:insert({send_messagetype, Serialport}, Messagetype),
do_request(post_messagetype, #{<<"type">> := Type, <<"messagetype">> := Messagetype, <<"serialport">> := Serialport}, _Context, _Req) ->
    dgiot_data:insert({dgiot_utils:to_atom(Type), Serialport}, Messagetype),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success">>}};

%% 云服务器接收盒子推送的串口日志记录
do_request(post_serial_log, #{<<"deviceid">> := DeviceId, <<"data">> := Data}, _Context, _Req) ->
    create_devicelog(DeviceId, Data),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success">>}};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

create_devicelog(DeviceId, Data) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"ACL">> := Acl, <<"product">> := #{<<"objectId">> := ProductId}}} ->
            NewData = #{
                <<"ACL">> => Acl,
                <<"devaddr">> => dgiot_datetime:now_ms(),
                <<"createtime">> => dgiot_datetime:now_ms(),
                <<"data">> => Data,
                <<"type">> => <<"serial_log">>,
                <<"ispush">> => false,
                <<"status">> => <<"serial_log">>,
                <<"product">> => #{
                    <<"objectId">> => ProductId,
                    <<"className">> => <<"Product">>,
                    <<"__type">> => <<"Pointer">>
                },
                <<"device">> => #{
                    <<"objectId">> => DeviceId,
                    <<"className">> => <<"Device">>,
                    <<"__type">> => <<"Pointer">>
                }
            },
            dgiot_parse:create_object(<<"Devicelog">>, NewData);
        _ ->
            pass
    end.
