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
-module(dgaiot_openai_handler).
-author("stoneliu").
-behavior(dgiot_rest).
-include_lib("dgiot/include/logger.hrl").
-dgiot_rest(all).

%% API
-export([swagger_openai/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/pump">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_feeders.json">>, ?MODULE, [], priv)
swagger_openai() ->
    [
        dgiot_http_server:bind(<<"/swagger_openai.json">>, ?MODULE, [], priv)
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

%% iot_hub 概要: 查询平台api资源 描述: 调用openai接口
%% OperationId:post_completions
%% 请求:GET /iotapi/
do_request(post_completions, Args, #{<<"sessionToken">> := _SessionTokfen} = _Context, _Req) ->
    Id = dgiot_parse_id:get_sessionId(_SessionTokfen),
    Openai_key =
        case dgiot_parse:get_object(<<"_Session">>, Id) of
            {ok, #{<<"user">> := #{<<"objectId">> := UserId}}} ->
                case dgiot_parse:get_object(<<"_User">>, UserId) of
                    {ok, #{<<"tag">> := #{<<"chat">> := #{<<"openai">> := Key}}}} ->
                        Key;
                    _ ->
                        application:get_env(dgaiot_openai, openai_key, <<"sk-12312313123">>)
                end;
            _ ->
                application:get_env(dgaiot_openai, openai_key, <<"sk-12312313123">>)
        end,
    io:format("~s ~p Openai_key = ~p.~n", [?FILE, ?LINE, Openai_key]),
    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, Args]),
    case dgaiot_openai:process_text(dgiot_utils:to_list(Openai_key), Args) of
        {ok, #{<<"choices">> := [#{<<"message">> := Message} | _]}} ->
            {ok, Message};
        _ ->
            {ok, #{}}
    end;

do_request(post_milvus, Args, _Context, _Req) ->
    Url = "https://prod.dgiotcloud.cn/iotapi/openapikey",
    case httpc:request(post, {Url, [], "application/json", dgiot_json:encode(Args)}, [], []) of
        {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
            case jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]) of
                #{<<"api_key">> := Api_key} ->
                    Python3path = "/data/dgiot/miniconda3/envs/milvus/rag_llm.py ",
                    PythonBody = dgiot_utils:to_list(base64:encode(dgiot_json:encode(Args#{<<"api_key">> => Api_key}))),
                    Cmd = "python3 " ++ Python3path ++ PythonBody,
                    io:format("~s ~p Cmd = ~p.~n", [?FILE, ?LINE, Cmd]),
                    Msg = os:cmd(Cmd),
                    io:format("~s ~p Msg = ~p.~n", [?FILE, ?LINE, Msg]),
                    {ok, #{<<"code">> => <<"200">>, <<"msg">> => Msg}};
                _ ->
                    {ok, #{<<"code">> => <<"500">>, <<"error">> => <<"unauthorized">>}}
            end;
        Error ->
            {ok, #{<<"code">> => <<"500">>, <<"error">> => Error}}
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
%%    io:format("~s ~p _Context = ~p.~n", [?FILE, ?LINE, _Context]),
    {error, <<"Not Allowed.">>}.

%%test()->
%%    PythonBody = dgiot_utils:to_list(base64:encode(dgiot_json:encode(#{<<"action">> => <<"query">>, <<"productid">> => <<"e3ee494031">>, <<"deviceid">> => <<"d4fbd025f9">>, <<"question">> => <<"北京在哪里"/utf8>>}))),
%%    PythonBody = dgiot_utils:to_list(base64:encode(dgiot_json:encode(#{<<"action">> => <<"create_device">>, <<"productid">> => <<"e3ee494031">>, <<"deviceid">> => <<"d4fbd025f9">>, <<"path">> => <<"milvus_docs/en/faq/*.md">>}))),
%%    case catch base64:decode(os:cmd("python3 /data/dgiot/miniconda3/envs/milvus/rag_llm.py " ++ PythonBody)) of
%%        {'EXIT', _Error} ->
%%            #{};
%%        PythonStr ->
%%            case jsx:is_json(PythonStr) of
%%                true ->
%%                    dgiot_json:decode(PythonStr);
%%                _ ->
%%                    #{}
%%            end
%%    end.
