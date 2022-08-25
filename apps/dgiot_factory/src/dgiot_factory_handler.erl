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

do_request(get_factory_calendar, #{<<"depart">> := Depart} = _Args, _Context, _Body) ->
    case dgiot_factory_calendar:get_calendar(Depart) of
        {ok, _, FactoryCalendar} ->
            {ok, FactoryCalendar};
        {Error, Res} ->
            {Error, Res};
        Res ->
            {error, Res}
    end;

do_request(post_factory_calendar, #{<<"default">> := Default, <<"other">> := Other, <<"depart">> := Depart} = _Args, _Context, _Body) ->
    case dgiot_factory_calendar:post_calendar(Depart, #{<<"default">> => Default, <<"other">> => Other}) of
        {ok, _, FactoryCalendar} ->
            {ok, FactoryCalendar};
        {Error, Res} ->
            {Error, Res};
        Res ->
            {error, Res}
    end;





do_request(get_worker_shift, #{<<"depart">> := Depart, <<"date">> := Data, <<"workshop">> := Workshop, <<"limit">> := Limit, <<"skip">> := Skip} = _Args,
    #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->
    Department = case Depart of
                     undefined ->
                         <<10, 10, 230, 180, 129, 232, 175, 186, 231, 148, 159, 228, 186, 167, 231, 174, 161, 231, 144, 134>>;
                     _ ->
                         Depart
                 end,
    case dgiot_factory_shift:get_all_shift(Department, Data, Workshop, SessionToken) of
        {ok, Res} ->
            {Total, Result} = dgiot_factory_getdata:filter_data(Limit, Skip, Res),
            {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"total">> => Total, <<"item">> => Result}}};
        _ ->
            {error, <<"get_data_failed">>}
    end;
do_request(post_worker_shift, #{<<"shift">> := Shifts} = _Args, _Context, _Body) ->
    dgiot_factory_shift:post_shift(Shifts),
    {ok, #{<<"status">> => 0, msg => <<"修改成功"/utf8>>, <<"data">> => #{}}};

do_request(get_data, #{<<"productId">> := undefined, <<"objectId">> := undefined, <<"type">> := Type, <<"starttime">> := Start, <<"endtime">> := End, <<"where">> := Where, <<"limit">> := Limit, <<"skip">> := Skip, <<"new">> := New} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
            ProductId = <<"ec71804a3d">>,
            case dgiot_factory_getdata:get_device_list(ProductId) of
                {ok, DeviceList} ->
                    case dgiot_factory_getdata:get_work_sheet(ProductId, Type, Start, End, Channel, DeviceList, Where, Limit, Skip, New) of
                        {ok, {Total, Res}} ->
                            {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"total">> => Total, <<"items">> => Res}}};
                        _ ->

                            {error, <<"get_data_failed">>}
                    end;
                _ ->

                    {error, <<"notfinddevice">>}

            end
    end;

do_request(get_data, #{<<"productId">> := ProductId, <<"objectId">> := undefined, <<"type">> := Type, <<"starttime">> := Start, <<"endtime">> := End, <<"where">> := Where, <<"limit">> := Limit, <<"skip">> := Skip, <<"new">> := New} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
            case dgiot_factory_getdata:get_device_list(ProductId) of
                {ok, DeviceList} ->
                    case dgiot_factory_getdata:get_work_sheet(ProductId, Type, Start, End, Channel, DeviceList, Where, Limit, Skip, New) of
                        {ok, {Total, Res}} ->
                            {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"total">> => Total, <<"items">> => Res}}};
                        _ ->

                            {error, <<"get_data_failed">>}
                    end;
                _ ->
                    {error, <<"notfinddevice">>}

            end
    end;


do_request(get_data, #{<<"productId">> := ProductId,<<"objectId">> := DeviceId, <<"type">> := Type, <<"starttime">> := Start, <<"endtime">> := End, <<"where">> := Where, <<"limit">> := Limit, <<"skip">> := Skip, <<"new">> := New} = _Args,
    #{<<"sessionToken">> := SessionToken} = _Context, _Body) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} -> {error, Error};
        {ok, Channel} ->
                    case dgiot_factory_getdata:get_work_sheet(ProductId, Type, Start, End, Channel, DeviceId, Where, Limit, Skip, New) of
                        {ok, {Total, Res}} ->
                            {ok, #{<<"status">> => 0, msg => <<"数据请求成功"/utf8>>, <<"data">> => #{<<"total">> => Total, <<"items">> => Res}}};
                        _ ->

                            {error, <<"get_data_failed">>}
                    end

    end;
do_request(get_material, #{<<"objectId">> := DeviceId} = _Args, _Context, _Req) ->
    case dgiot_factory_material:get_material_record(DeviceId) of
        {ok, Res} ->
            {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => maps:values(Res)}};
        _ ->
            error
    end;
do_request(post_material, #{<<"objectId">> := DeviceId, <<"shift">> :=  Data} = _Args, _Context, _Req) ->
    case dgiot_factory_material:post_material(DeviceId, Data) of
        {ok, _} ->
            {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => []}};
        _ ->
            error
    end;


do_request(get_warehouse_material, #{<<"limit">> := Limit,<<"skip">> := Skip,<<"where">> := Where} = _Args, _Context, _Req) ->
    case dgiot_factory_material:get_warehouse_material(Limit, Skip, Where) of
        {ok, {Total,Res}} ->
            {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => #{<<"total">> => Total,<<"items">> =>Res}}};
        _ ->
            error
    end;
%%do_request(post_warehouse_material,  #{<<"objectId">> :=Id ,<<"record">> := Record} =_Args, _Context, _Req) ->
    do_request(post_warehouse_material,  _Args, _Context, _Req) ->
        case dgiot_factory_material:put_warehouse_material( _Args) of
        {ok, _} ->
            {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => []}};
        _ ->
            error
    end;


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    io:format("~s ~p _Args = ~p  ~n", [?FILE, ?LINE, _Args]),
    ?LOG(info, "_OperationId:~p~n", [_Args]),
    {error, <<"Not Allowed.">>}.
