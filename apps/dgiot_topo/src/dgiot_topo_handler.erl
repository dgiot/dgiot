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
-module(dgiot_topo_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([handle/4]).
-export([swagger_topo/0]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/opc">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_topo.json">>, ?MODULE, [], priv)
swagger_topo() ->
    [
        dgiot_http_server:bind(<<"/swagger_topo.json">>, ?MODULE, [], priv)
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
            {200, Headers, #{}, Req};
        {ok, Res} ->
            {200, Headers, Res, Req};
        {Status, Res} ->
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% topo 概要: 获取组态
%% OperationId:topo
%% 请求:GET topo
do_request(get_topo, Arg, Context, _Req) ->
    case dgiot_topo:get_topo(Arg, Context) of
        {ok, Success} ->
            {ok, Success};
        {error, Reason} ->
            {400, Reason}
    end;


%% topo 概要: 发送组态数据
%% OperationId:topo
%% 请求:post post_send_topo
do_request(post_topo, Arg, Context, _Req) ->
    case dgiot_topo:put_topo(Arg, Context) of
        {ok, Success} ->
            {ok, Success};
        {error, Reason} ->
            {400, Reason}
    end;

%% topo 概要: 获取组态物模型详情
%% OperationId:topo
%% 请求:post get_konva_thing
do_request(get_konva_thing, Arg, Context, _Req) ->
    case dgiot_topo:get_konva_thing(Arg, Context) of
        {ok, Success} ->
            {ok, Success};
        {error, Reason} ->
            {400, Reason}
    end;

%% topo 概要: 修改组态
%% OperationId:topo
%% 请求:post post_send_topo
do_request(post_konva_thing, Arg, Context, _Req) ->
    case dgiot_topo:edit_konva(Arg, Context) of
        {ok, Success} ->
            {ok, Success};
        {error, Reason} ->
            {400, Reason}
    end;

do_request(post_dashboard, Arg, Context, _Req) ->
    dgiot_dashboard:post_dashboard(Arg, Context),
    {200, <<"success">>};

do_request(get_devicedict, #{<<"deviceid">> := Deviceid}, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
%%    case dgiot_parse:get_object(<<"Device">>, <<"566cf263dc">>, [{"X-Parse-Session-Token", <<"r:e53794ae4bb367b13f73ddd5891e2755">>}], [{from, rest}]) of
    case dgiot_parse:get_object(<<"Device">>, Deviceid, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"basedata">> := #{<<"deviceDict">> := DeviceDict}}} ->
            {ok, DeviceDict};
        _ ->
            {error, []}
    end;

%% 查询设备所拥有权限
do_request(get_deviceacl, #{<<"deviceid">> := Deviceid}, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_parse:get_object(<<"Device">>, Deviceid, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"ACL">> := ACL}} ->
            {ok, ACL};
        _ ->
            {error, []}
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.
