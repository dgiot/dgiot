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

do_request(get_material, #{<<"objectId">> := DeviceId, <<"dept">> := Depart} = _Args, _Context, _Req) ->
    case dgiot_factory_material:get_material_record(DeviceId, Depart) of
        {ok, Res} ->
            {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => maps:values(Res)}};
        _ ->
            error
    end;
do_request(post_material, #{<<"objectId">> := DeviceId, <<"shift">> := Data} = _Args, _Context, _Req) ->
    case dgiot_factory_material:post_material(DeviceId, Data) of
        {ok, _} ->
            {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => []}};
        _ ->
            error
    end;

do_request(get_workertree, _Arg, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    Data = dgiot_factory_utils:get_usertree(_Arg, SessionToken),
    {ok, #{
        <<"status">> => 0,
        <<"msg">> => <<"ok">>,
        <<"data">> => #{<<"options">> => Data}
    }};

%% 获取车间大屏数据
do_request(get_factory_screen, _Arg, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} ->
            {error, Error};
        {ok, Channel} ->
            Arg = dgiot_factory_utils:kill_undefined(_Arg),
            case dgiot_factory_screen:get_screen(Arg, Channel) of
                {ok, Res} ->
                    {ok, #{<<"status">> => 0, msg => <<"操作成功"/utf8>>, <<"data">> => Res}};
                _R ->
                    {ok, #{<<"status">> => 1, msg => <<"操作失败"/utf8>>}}
            end
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    io:format("~s ~p _Args = ~p  ~n", [?FILE, ?LINE, _Args]),
    ?LOG(info, "_OperationId:~p~n", [_Args]),
    {error, <<"Not Allowed.">>}.
