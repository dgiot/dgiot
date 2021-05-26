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

-module(dgiot_bridge_handler).
-author("dgiot").
-behavior(dgiot_rest).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_bridge/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/bridge">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_bridge.json">>, ?MODULE, [], priv)
swagger_bridge() ->
    [
        dgiot_http_server:bind(<<"/swagger_bridge.json">>, ?MODULE, [], priv)
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
            {500, Headers, #{ <<"error">> => Err }};
        ok ->
            dgiot_logger:debug("do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            dgiot_logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            dgiot_logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            dgiot_logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            dgiot_logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

do_request(post_control_channel, #{<<"id">> := ChannelId, <<"action">> := Action}, _Context, _Req)
    when Action == <<"enable">>; Action == <<"disable">> ->
    dgiot_bridge:control_channel(ChannelId, Action);

%% Decoder 概要: 获取指令集 描述:根据产品ID关联的解码器获取指令集
%% OperationId:get_cmd_productid
%% 请求:GET /iotapi/cmd/:productId
do_request(get_decoder_func_id, #{<<"id">> := Id }, _Context, _Req) ->
    {200, dgiot_decoder:get_funcs(Id)};

%% Decoder 概要: 新增解码器 描述:给产品新增解码器
%% OperationId:post_classes_decoder_pid
%% 请求:POST /iotapi/classes/Decoder/:pid
do_request(post_classes_decoder, Data, #{ <<"user">> := #{<<"objectId">> := UserId}, <<"sessionToken">> := SessionToken }, _Req) ->
    ACL = maps:get(<<"ACL">>, Data, #{ UserId => #{ <<"read">> => true, <<"write">> => true } }),
    dgiot_parse:create_object(<<"Dict">>, #{
        <<"type">> => <<"decoder">>,
        <<"ACL">> => ACL,
        <<"data">> => maps:without([<<"ACL">>], Data)
    }, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);

%% Decoder 概要: 更新解码器 描述:根据ID更新解码器
%% OperationId:put_classes_decoder_id
%% 请求:PUT /iotapi/classes/Decoder/:id
do_request(put_classes_decoder_id, #{ <<"id">> := Id } = Data, #{ <<"user">> := #{<<"objectId">> := _UserId}, <<"sessionToken">> := SessionToken }, _Req) ->
    Data1 =
        case maps:get(<<"ACL">>, Data, no) of
            no -> #{};
            ACL -> #{<<"ACL">> => ACL}
        end,
    case dgiot_parse:update_object(<<"Dict">>, Id, Data1#{
        <<"type">> => <<"decoder">>,
        <<"data">> => maps:without([<<"id">>, <<"ACL">>], Data)
    }, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, Result} ->
            {ok, Result};
        {error, #{ <<"error">> := #{ <<"coder">> := 101 } } = Result} ->
            {404, Result};
        {error, Reason} ->
            {error, Reason}
    end;

%% Decoder 概要: 获取解码器 描述:根据ID获取解码器
%% OperationId:get_classes_decoder_id
%% 请求:GET /iotapi/classes/Decoder/:id
do_request(get_classes_decoder_id, #{ <<"id">> := Id }, #{ <<"sessionToken">> := Session }, _Req) ->
    case dgiot_parse:get_object(<<"Dict">>, Id, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
        {ok, Result} ->
            {ok, Result};
        {error, #{ <<"error">> := #{ <<"coder">> := 101 } } = Result} ->
            {404, Result};
        {error, Reason} ->
            {error, Reason}
    end;

%% Decoder 概要: 删除解码器 描述:根据ID删除解码器
%% OperationId:delete_classes_decoder_id
%% 请求:DELETE /iotapi/classes/Decoder/:id
do_request(delete_classes_decoder_id, #{ <<"id">> := Id }, #{ <<"sessionToken">> := Session }, _Req) ->
    case dgiot_parse:del_object(<<"Dict">>, Id, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
        {ok, Result} ->
            {ok, Result};
        {error, #{ <<"error">> := #{ <<"coder">> := 101 } } = Result} ->
            {404, Result};
        {error, Reason} ->
            {error, Reason}
    end;

%% Decoder 概要: 查询解码器 描述:查询解码器
%% OperationId:get_classes_decoder
%% 请求:GET /iotapi/classes/Decoder
do_request(get_classes_decoder, Args, #{<<"sessionToken">> := Session}, _Req) ->
    Query =
        maps:fold(
            fun(Key, Value, Acc) ->
                case Value of
                    _ when Key == <<"where">> ->
                        case Value of
                            undefined ->
                                Acc#{ Key => #{ <<"type">> => <<"decoder">> } };
                            _ ->
                                Where = jsx:decode(Value, [{labels, binary}, return_maps]),
                                Acc#{ Key => Where#{ <<"type">> => <<"decoder">> } }
                        end;
                    undefined ->
                        Acc;
                    _ ->
                        Acc#{Key => Value}
                end
            end, #{}, Args),
    case dgiot_parse:query_object(<<"Dict">>, Query, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end;


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.
