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
-module(dgiot_ffmpeg_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_system/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_ffmpeg.json">>, ?MODULE, [], priv)
swagger_system() ->
    [
        dgiot_http_server:bind(<<"/swagger_ffmpeg.json">>, ?MODULE, [], priv)
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
                      false -> dgiot_framework:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            {200, Headers, #{}, Req};
        {ok, Res} ->
            {200, Headers, Res, Req};
        {Status, Res} ->
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================
%%  视频取证的API接口
%% FFMPEG 概要: 获取视频取证任务信息 描述:json文件导库
%% OperationId:get_ffmpeg
%% 请求:GET /iotapi/ffmpeg
do_request(get_ffmpeg, #{<<"type">> := Type, <<"devaddr">> := DevAddr, <<"product">> := Product} = _Body,
        #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"_Body ~p", [_Body]),
    get_ffmpeg(Type, DevAddr, Product, SessionToken);

%%  视频取证的API接口
%% FFMPEG 概要: 修改视频取证任务 描述:json文件导库
%% OperationId:put_ffmpeg
%% 请求:PUT /iotapi/ffmpeg
do_request(put_ffmpeg, _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"_Body ~p", [_Body]),
    put_ffmpeg(_Body, SessionToken);

do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.


get_ffmpeg(Type, DevAddr, Product, SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"devaddr">> => DevAddr, <<"product">> => Product}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [Device | _]}} ->
            ?LOG(info,"Device ~p ", [Device]),
            Dis =
                case Device of
                    #{<<"route">> := _Route} ->
                        case Type of
                            <<"ALL">> ->
                                [<<DevAddr/binary, "/VIDEO">>, <<DevAddr/binary, "/LIVEVIDEO">>];
                            _ ->
                                [<<DevAddr/binary, "/", Type/binary>>]
                        end;
                    _ ->
                        case Type of
                            <<"ALL">> ->
                                [
                                    <<DevAddr/binary, "/VIDEO">>, <<DevAddr/binary, "/LIVEVIDEO">>,
                                    <<DevAddr/binary, "/SCREENVIDEO">>, <<DevAddr/binary, "/LIVESCREENVIDEO">>
                                ];
                            _ ->
                                [<<DevAddr/binary, "/", Type/binary>>]
                        end
                end,
            NewDis =
                lists:foldl(fun(#{<<"devaddr">> := DevAddr1}, Acc) ->
                    case Type of
                        <<"ALL">> -> Acc ++ [<<DevAddr1/binary, "/VIDEO">>, <<DevAddr1/binary, "/LIVEVIDEO">>];
                        _ -> Acc ++ [<<DevAddr1/binary, "/", Type/binary>>]
                    end
                            end, Dis, dgiot_shadow:get_sub_device(DevAddr)),
            dgiot_parse:query_object(<<"Instruct">>, #{
                <<"keys">> => [<<"name">>, <<"di">>, <<"enable">>, <<"interval">>, <<"rotation">>],
                <<"where">> => #{<<"di">> => #{<<"$in">> => dgiot_utils:unique_1(NewDis)}}},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        _Error -> _Error
    end.


put_ffmpeg(#{<<"type">> := Type, <<"devaddr">> := DevAddr, <<"product">> := Product} = Body, SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"devaddr">> => DevAddr, <<"product">> => Product}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [Device | _]}} ->
            Dis =
                case Device of
                    #{<<"route">> := _Route} ->
                        case Type of
                            <<"ALL">> ->
                                [<<DevAddr/binary, "/VIDEO">>, <<DevAddr/binary, "/LIVEVIDEO">>];
                            _ ->
                                [<<DevAddr/binary, "/", Type/binary>>]
                        end;
                    _ ->

                        case Type of
                            <<"ALL">> ->
                                [
                                    <<DevAddr/binary, "/VIDEO">>, <<DevAddr/binary, "/LIVEVIDEO">>,
                                    <<DevAddr/binary, "/SCREENVIDEO">>, <<DevAddr/binary, "/LIVESCREENVIDEO">>
                                ];
                            _ ->
                                [<<DevAddr/binary, "/", Type/binary>>]
                        end
                end,
            NewDis =
                lists:foldl(fun(#{<<"devaddr">> := DevAddr1}, Acc) ->
                    case Type of
                        <<"ALL">> -> Acc ++ [<<DevAddr1/binary, "/VIDEO">>, <<DevAddr1/binary, "/LIVEVIDEO">>];
                        _ -> Acc ++ [<<DevAddr1/binary, "/", Type/binary>>]
                    end
                            end, Dis, dgiot_shadow:get_sub_device(DevAddr)),
            case dgiot_parse:query_object(<<"Instruct">>, #{
                <<"keys">> => [<<"name">>, <<"di">>, <<"enable">>, <<"interval">>, <<"rotation">>],
                <<"where">> => #{<<"di">> => #{<<"$in">> => dgiot_utils:unique_1(NewDis)}}},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                {ok, #{<<"results">> := Resutls}} ->
                    {ok, #{<<"results">> =>
                    lists:foldl(fun(#{<<"objectId">> := ObjectId}, Acc) ->
                        {_, R} = dgiot_parse:update_object(<<"Instruct">>, ObjectId,
                            maps:with([<<"enable">>, <<"interval">>], Body)),
                        Acc ++ [R]
                                                     end, [], Resutls)
                    }};
                Error -> Error
            end;
        _Error -> _Error
    end.

