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

-module(dgiot_tdengine_handler).
-author("dgiot").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot_tdengine/include/dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_tdengine/0]).
-export([handle/4]).


%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/tdengine">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_tdengine.json">>, ?MODULE, [], priv)
swagger_tdengine() ->
    [
        dgiot_http_server:bind(<<"/swagger_tdengine.json">>, ?MODULE, [], priv)
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
                      false -> list_to_binary(io_lib:format("~p", [Reason]))
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            ?LOG(debug, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            %?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================


%% TDengine 概要: 获取当前产品下的所有设备数据 描述:获取当前产品下的所有设备数据
%% OperationId:get_td_cid_pid
%% 请求:GET /iotapi/td/prodcut/:productId
do_request(get_product_pid, #{
    <<"pid">> := ProductId,
    <<"where">> := Where,
    <<"keys">> := _Keys
} = Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    Fun =
        fun(ChannelId) ->
            TableName = ?Table(ProductId),
            Query = maps:without([<<"pid">>], Args),
            case jsx:is_json(Where) of
                true ->
                    case dgiot_tdengine:query_object(ChannelId, TableName, Query#{
                        <<"db">> => ProductId,
                        <<"where">> => jsx:decode(Where, [{labels, binary}, return_maps])
                    }) of
                        {ok, Data} ->
                            {ok, Data};
                        {error, Reason} ->
                            {400, Reason}
                    end;
                false ->
                    {400, <<"where is not json">>}
            end
        end,
    dgiot_product_tdengine:do_channel(ProductId, SessionToken, Fun);


%% TDengine 概要: 获取当前产品下的所有设备数据 描述:获取当前产品下的所有设备数据
%% OperationId:get_td_productid_channelid_addr_productid
%% 请求:GET /iotapi/td/:ProductId/:channelId/:addr/:productId
do_request(get_device_deviceid, #{<<"deviceid">> := DeviceId} = Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
            ?LOG(info, "DeviceId ~p", [DeviceId]),
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}} ->
                    dgiot_product_tdengine:get_product_data(Channel, ProductId, DeviceId, Args);
                _ ->
                    {error, <<"not find device">>}
            end
    end;

%% TDengine 概要: 获取设备历史数据图表 描述:获取设备历史数据图表
do_request(get_echart_deviceid, #{<<"deviceid">> := DeviceId, <<"style">> := Style} = Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}} ->
                    case Style of
                        <<"amis_table">> ->
                            dgiot_device_echart:get_data_by_month(Channel, ProductId, DeviceId, Args);
                        <<"echart_category">> ->
                            dgiot_device_echart:get_data_by_echart_category(Channel, ProductId, DeviceId, Args);
                        _ ->
                            dgiot_device_echart:get_echart_data(Channel, ProductId, DeviceId, Args)

                    end;
                _ ->
                    {error, <<"not find device">>}
            end
    end;

%% TDengine 概要: 获取当前设备最新时序数据卡片
do_request(get_devicecard_deviceid, #{<<"deviceid">> := DeviceId} = Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} ->
            {error, Error};
        {ok, Channel} ->
%%            ?LOG(info,"DeviceId ~p", [DeviceId]),
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}} ->
                    dgiot_device_card:get_device_card(Channel, ProductId, DeviceId, Args);
                _ ->
                    {error, <<"not find device">>}
            end
    end;

%% TDengine 概要: 获取gps轨迹
do_request(get_gps_track_deviceid, #{<<"deviceid">> := DeviceId}, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case DeviceId of
        <<"all">> ->
            case dgiot_parse:query_object(<<"Device">>, #{}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                {ok, #{<<"results">> := Results}} ->
                    NewResults =
                        lists:foldl(fun(#{<<"objectId">> := ObjectId, <<"name">> := Name}, Acc) ->
                            LineList =
                                case dgiot_product_tdengine:get_channel(SessionToken) of
                                    {error, Error} -> {error, Error};
                                    {ok, Channel} ->
                                        case dgiot_device:lookup(ObjectId) of
                                            {ok, #{<<"productid">> := ProductId}} ->
                                                {ok, #{<<"results">> := Results1}} = dgiot_device_tdengine:get_gps_track(Channel, ProductId, ObjectId),
                                                Results1;
                                            _ ->
                                                []
                                        end
                                end,
                            Acc ++ [#{<<"objectId">> => ObjectId, <<"name">> => Name, <<"lineList">> => LineList}]
                                    end, [], Results),
                    {ok, #{<<"results">> => NewResults}};
                _ ->
                    {ok, #{<<"results">> => []}}
            end;
        _ ->
            case dgiot_product_tdengine:get_channel(SessionToken) of
                {error, Error} -> {error, Error};
                {ok, Channel} ->
                    case dgiot_device:lookup(DeviceId) of
                        {ok, #{<<"productid">> := ProductId}} ->
                            dgiot_device_tdengine:get_gps_track(Channel, ProductId, DeviceId);
                        _ ->
                            {ok, #{<<"results">> => []}}
                    end
            end
    end;

%% 档案 概要: 导库 描述: 导出td zip
%% OperationId:
%% 请求:POST /iotapi/post_export_td
do_request(post_export_td, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req0) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} ->
            {error, Error};
        {ok, ChannelId} ->
%%            io:format("~s ~p ChannelId = ~p.~n", [?FILE, ?LINE, ChannelId]),
            MS = dgiot_utils:to_binary(dgiot_datetime:now_ms()),
            Path = list_to_binary(dgiot_http_server:get_env(dgiot_api, docroot)),
            file:make_dir(<<Path/binary, "/download">>),
            file:make_dir(<<Path/binary, "/download/tdengine">>),
            FileName =
                case maps:find(<<"deviceid">>, Body) of
                    error ->
                        "/download/tdengine/" ++ dgiot_utils:to_list(MS) ++ ".zip";
                    {ok, DeviceId} ->
                        dgiot_utils:to_list(<<"/download/tdengine/", DeviceId/binary, "_", MS/binary, ".zip">>)
                end,
            SchemasFile = dgiot_utils:to_list(Path) ++ FileName,
            dgiot_channelx:do_message(ChannelId, {export_data, Body, SchemasFile}),
            {ok, #{<<"path">> => dgiot_utils:to_binary(FileName)}}
    end;


%% DB 概要: 导库 描述: zip导入td
%% OperationId:post_import_td
%% 请求:POST /iotapi/post_import_td
do_request(post_import_td, #{<<"file">> := FileInfo}, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} ->
            {error, Error};
        {ok, ChannelId} ->
            case get_json_from_zip(FileInfo) of
                {ok, Result} ->
                    dgiot_tdengine:import(ChannelId, Result);
                _ ->
                    pass
            end
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

get_json_from_zip(FileInfo) ->
    Dir = list_to_binary(dgiot_http_server:get_env(dgiot_api, docroot)),
    #{<<"path">> := <<"/", Path/binary>>} = FileInfo,
    FilePath = filename:join([Dir, Path]),
    case zip:unzip(dgiot_utils:to_list(FilePath), [memory]) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.


