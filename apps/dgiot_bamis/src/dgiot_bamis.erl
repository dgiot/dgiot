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

%% @doc dgiot_bamis Protocol
-module(dgiot_bamis).
-include("dgiot_bamis.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_swagger(<<"amis">>).

-export([
    get/1,
    post/1,
    put/1,
    delete/1,
    start_http/0
]).

-define(APP, ?MODULE).
%% amis api 接口适配
%% https://aisuda.bce.baidu.com/amis/zh-CN/docs/types/api#%E6%8E%A5%E5%8F%A3%E8%BF%94%E5%9B%9E%E6%A0%BC%E5%BC%8F-%E9%87%8D%E8%A6%81-

%% @description 接口说明
%% before 请求前的接口适配器拦截
%% after 请求返回的数据适配器，目前已适配amis返回格式参数，参数如下

%% @description 参数说明
%% status: 返回 0，表示当前接口正确返回，否则按错误请求处理；
%% msg: 返回接口处理信息，主要用于表单提交或请求失败时的 toast 显示；
%% data: 必须返回一个具有 key-value 结构的对象。

%% @description 备注
%% 已实现 get post 的适配工作
get({'before', Data}) ->
    io:format("amis get before: ~p~n", [Data]),
    Data;
get({'after', Data}) ->
    io:format("amis get after: ~p~n", [Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"数据请求成功"/utf8>>,
        <<"data">> => Data
    }.
post({'before', Data}) ->
    io:format("amis post before ~p~n", [Data]),
    Data;
post({'after', Data}) ->
    io:format("amis post after ~p~n", [Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"数据提交成功"/utf8>>,
        <<"data">> => Data
    }.
put({'before', Data}) ->
    io:format("amis put before ~p~n", [Data]),
    Data;
put({'after', Data}) ->
    io:format("amis put after ~p~n", [Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"修改成功"/utf8>>,
        <<"data">> => Data
    }.
delete({'before', Data}) ->
    io:format("amis delete before ~p~n", [Data]),
    Data;
delete({'after', Data}) ->
    io:format("amis delete after ~p~n", [Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"删除成功"/utf8>>,
        <<"data">> => Data
    }.


start_http() ->
    Port = 9089,
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    DocRoot = Root ++ "www",
    dgiot_http_server:start_http(?MODULE, Port, DocRoot).