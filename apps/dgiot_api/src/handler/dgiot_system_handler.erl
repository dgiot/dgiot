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

-module(dgiot_system_handler).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_rest).

%% API
-export([swagger_system/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
swagger_system() ->
    [
        dgiot_http_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
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
            ?LOG(info,"do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> list_to_binary(io_lib:format("~p", [Reason]))
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
           ?LOG(error,"do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
           ?LOG(error,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
           ?LOG(error,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
           ?LOG(error,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
           ?LOG(error,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================


%% System 概要: 对插件操作 描述:对插件操作, start, stop, reload
%% OperationId:post_system_plugin_action
%% 请求:POST /iotapi/system/plugin/:Action
do_request(post_plugin_app, Args, Context, Req) ->
    dgiot_common_handler:do_request(post_plugin_app, Args, Context, Req);

%% System 概要: 获取插件 描述:获取插件
%% OperationId:get_system_app
%% 请求:GET /iotapi/system/app
do_request(get_plugin, Args, Context, Req) ->
    dgiot_common_handler:do_request(get_plugin, Args, Context, Req);

%% System 概要: 获取节点 描述:获取节点
%% OperationId:get_nodes
%% 请求:GET /iotapi/nodes
do_request(get_nodes, _Args, _Context, _Req) ->
    Nodes = dgiot_node:get_nodes(),
    {ok, #{ nodes => Nodes }};

%% System 概要: 根据swagger产生代码 描述:根据swagger产生代码
%% OperationId:generate_code
%% 请求:POST /iotapi/swagger/generate/:type
do_request(post_generate_api_type, Args, Context, Req) ->
    dgiot_common_handler:do_request(generate_api, Args, Context, Req);

%% Log 概要: 更新日志配置 描述:更新日志配置
%% OperationId:put_log_level
%% 请求:PUT /iotapi/log/level
do_request(put_log_level, Args, Context, Req) ->
    dgiot_common_handler:do_request(put_log_level, Args, Context, Req);

%% Log 概要: 获取日志配置 描述:获取日志配置
%% OperationId:get_log_level
%% 请求:GET /iotapi/log/level
do_request(get_log_level, Args, Context, Req) ->
    dgiot_common_handler:do_request(get_log_level, Args, Context, Req);

%% System 概要: 统计获取 描述:调用Grafanfa查询曲线
%% OperationId:get_chart_version
%% 请求:GET /iotapi/chart/:version
do_request(get_chart_version, #{<<"query">> := Query, <<"start">> := Start, <<"end">> := End, <<"step">> := Step} = Args, _Context, _Req) ->
    Version = maps:get(<<"version">>, Args, <<"v1">>),
    case dgiot_stats:query_range(Version, Query, Start, End, Step) of
        {ok, StatusCode, Data} ->
            {StatusCode, Data};
        {error, Reason} ->
            {error, Reason}
    end;

%% System 概要: 编译代码 描述:在线编译代码
%% OperationId:post_compile
%% 请求:POST /iotapi/compile
do_request(post_compile, Args, Context, Req) ->
    dgiot_common_handler:do_request(post_compile, Args, Context, Req);


%% System 概要: 集群操作 描述:加入,退出集群
%% OperationId:post_cluster
%% 请求:POST /iotapi/cluster
do_request(post_cluster, Args, Context, Req) ->
    dgiot_common_handler:do_request(post_cluster, Args, Context, Req);

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.
