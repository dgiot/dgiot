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
-module(dgiot_view_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_view/0]).
-export([handle/4]).
%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/amis">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_view.json">>, ?MODULE, [], priv)
swagger_view() ->
    [
        dgiot_http_server:bind(<<"/swagger_view.json">>, ?MODULE, [], priv)  %需要于priv/swagger/目录下放置swagger.json文件名保持一致
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
do_request(post_dashboard, #{<<"dashboardId">> := <<"null">>} = Arg, _Context, _Req) ->
    {200, Arg};

do_request(post_dashboard, Arg, Context, _Req) ->
    dgiot_dashboard:post_dashboard(Arg, Context),
    {200, Arg#{<<"status">> => 0}};

%% iot_hub 概要: amis 变量替换
%% OperationId:post_amis
%% 请求:POST /iotapi/post_amis
do_request(post_amis, #{<<"viewid">> := Viewid, <<"render">> := Render} = _Arg, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    Amisdata =
        case dgiot_parse:get_object(<<"View">>, Viewid, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
            {ok, #{<<"data">> := Data} = View} ->
                Json = dgiot_json:encode(Data),
                NewJson =
                    maps:fold(fun(K, V, Acc) ->
                        re:replace(Acc, <<"%{", K/binary, "}">>, V, [global, {return, binary}])
                              end, Json, Render),
                View#{<<"data">> := dgiot_json:decode(NewJson)};
            _ ->
                #{}
        end,
    {200, Amisdata};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
%%    io:format("~s ~p _OperationId = ~p.~n", [?FILE, ?LINE, _OperationId]),
%%    io:format("~s ~p _Args = ~p.~n", [?FILE, ?LINE, _Args]),
    {error, <<"Not Allowed.">>}.
