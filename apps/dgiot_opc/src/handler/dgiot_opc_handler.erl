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
-module(dgiot_opc_handler).
-author("johnliu").
-behavior(dgiot_rest).
-include_lib("dgiot/include/logger.hrl").
-dgiot_rest(all).


%% API
-export([swagger_opc/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/opc">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_opc.json">>, ?MODULE, [], priv)
swagger_opc() ->
    [
        dgiot_http_server:bind(<<"/swagger_opc.json">>, ?MODULE, [], priv)
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
            ?LOG(info, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================
do_request(post_head, #{<<"items">> := Items, <<"productid">> := ProductId}, _Context, _Req) ->
    Head =
        case dgiot_product:lookup_prod(ProductId) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                lists:foldl(fun(Item, Acc) ->
                    lists:foldl(fun(Prop, Acc1) ->
                        case Prop of
                            #{<<"name">> := Name, <<"identifier">> := Identifier,
                                <<"dataType">> := #{<<"type">> := _Type, <<"das">> := Das}} ->
                                case lists:member(Item, Das) of
                                    true ->
                                        Acc1#{Identifier => Name};
                                    _ ->
                                        Acc1
                                end;
                            _ ->
                                Acc1
                        end
                                end, Acc, Props)
                            end, #{}, Items);
            _Error ->
                #{}
        end,
    {ok, #{<<"code">> => 200, <<"head">> => Head}};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.
