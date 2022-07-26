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
-module(dgiot_factory_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").
-define(PRODUCTID, <<"ec71804a3d">>).


%% API
-export([swagger_factory/0]).
-export([handle/4]).
%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/amis">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_amis.json">>, ?MODULE, [], priv)
swagger_factory() ->
    [
        dgiot_http_server:bind(<<"/swagger_factory.json">>, ?MODULE, [], priv)  %需要于priv/swagger/目录下放置swagger.json文件名保持一致
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
    io:format("~s ~p OperationID ~p ~n", [?FILE, ?LINE, OperationID]),
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

do_request(get_factory_calendar, _Args, _Context, _Body) ->
    case dgiot_factory_calendar:get_calendar() of
        {ok, _, FactoryCalendar} ->
            {ok, FactoryCalendar};
        {Error, Res} ->
            {Error, Res};
        Res ->
            {error, Res}
    end;

do_request(post_worker_shift, #{<<"default">> := Default, <<"other">> := Other} = _Args, _Context, _Body) ->
    case dgiot_factory_calendar:post_calendar(#{<<"default">> => Default, <<"other">> => Other}) of
        {ok, _, FactoryCalendar} ->
            {ok, FactoryCalendar};
        {Error, Res} ->
            {Error, Res};
        Res ->
            {error, Res}
    end;





do_request(get_worker_shift, #{<<"date">> := Data, <<"workshop">> := Workshop} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->
    case {Data, Workshop} of
        {undefined, _} ->
            io:format("~s ~p here~n", [?FILE, ?LINE]),
            case dgiot_factory_shift:get_all_shift(Data, Workshop, SessionToken) of
                {ok, Res} ->
                    {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"item">> => Res}}};
                {error, Msg} ->
                    {error, Msg}
            end;
        {_, undefined} ->
            io:format("~s ~p here~n", [?FILE, ?LINE]),
            case dgiot_factory_shift:get_all_shift(Data, Workshop, SessionToken) of
                {ok, Res} ->
                    {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"item">> => Res}}};
                {error, Msg} ->
                    {error, Msg}
            end;
        _ ->

            case dgiot_factory_shift:get_shift(Data, Workshop) of
                {ok, Res} ->
                    {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"item">> => Res}}};
                {error, Msg} ->
                    {error, Msg}
            end
    end;

do_request(post_worker_shift, #{<<"shift">> := Shifts} = _Args, _Context, _Body) ->
    io:format("~s ~p_Args= ~p ~n", [?FILE, ?LINE, _Args]),
    dgiot_factory_shift:post_shift(Shifts),
    {ok, <<"修改成功">>};

do_request(post_data, #{<<"objectid">> := DeviceId} = Args, _Context, _Body) ->
    io:format("~s ~p DeviceId= ~p ~n", [?FILE, ?LINE, dgiot_utils:to_binary(string:to_lower(binary_to_list(DeviceId)))]),
    case dgiot_factory_data:handle_data(dgiot_utils:to_binary(string:to_lower(binary_to_list(DeviceId))), a, Args) of
        {ok, _} ->
            {ok, <<"修改成功"/utf8>>};
        {error, Msg} ->
            {error, Msg}
    end;

do_request(get_data, #{<<"objectId">> := undefined, <<"type">> := Type, <<"where">> := Where, <<"limit">> := Limit, <<"skip">> := Skip,<<"new">> := New} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->

    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
            case dgiot_factory_data:get_device_list() of
                {ok, DeviceList} ->
                    case dgiot_factory_data:get_work_sheet(Type, Channel, DeviceList, Where, Limit, Skip,New) of
                        {ok, {Total, Res}} ->
                            {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"total">> => Total, <<"items">> => Res}}};
                        _ ->

                            {error, <<"get_data_failed">>}
                    end;
                _ ->
                    io:format("~s ~p here~n",[?FILE,?LINE]),
                    {error, <<"notfinddevice">>}

            end
    end;


do_request(get_data, #{<<"objectId">> := DeviceId, <<"type">> := Type, <<"where">> := Where, <<"limit">> := Limit, <<"skip">> := Skip,<<"new">> := New} = _Args,
    #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
            case dgiot_factory_data:get_work_sheet(Type, Channel, DeviceId, Where, Limit, Skip,New) of
                {ok, {Total, Res}} ->
                    {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"total">> => Total, <<"items">> => Res}}};
                _ ->

                    {error, <<"get_data_failed">>}
            end
    end;



do_request(post_stored, #{<<"objectId">> := DeviceId,<<"operator">> := Operator,<<"quality">>:=Quality} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
            case dgiot_factory_utils:store_all(Channel, DeviceId,Operator,Quality) of
                {ok, _} ->
                    {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => #{}}};
                _ ->
                    {error, <<"get_data_failed">>}
            end
    end;


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    io:format("~s ~p here ~n",[?FILE,?LINE]),
    ?LOG(info, "_OperationId:~p~n", [_Args]),
    {error, <<"Not Allowed.">>}.
