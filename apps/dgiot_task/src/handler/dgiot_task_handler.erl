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

-module(dgiot_task_handler).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_rest).
-dgiot_rest(all).

%% API
-export([swagger_task/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_task.json">>, ?MODULE, [], priv)
swagger_task() ->
    [
        dgiot_http_server:bind(<<"/swagger_task.json">>, ?MODULE, [], priv)
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
do_request(get_task, #{<<"vcaddr">> := VcAddr} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    Where =
        case VcAddr of
            <<"all">> -> #{<<"where">> => #{<<"cType">> => <<"INSTRUCT">>}};
            _ -> #{<<"where">> => #{<<"config.vcaddr">> => VcAddr, <<"cType">> => <<"INSTRUCT">>}}
        end,
    dgiot_parse:query_object(<<"Channel">>, Where, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);

do_request(put_task, #{<<"channelId">> := ChannelId} = Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "Args ~p", [Args]),
    case dgiot_parse:get_object(<<"Channel">>, ChannelId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := ChannelId} = Data} ->
            Config = maps:get(<<"config">>, Data),
            NewConfig = maps:merge(Config, maps:without([<<"channelId">>, <<"vcaddr">>], Args)),
            NewData = maps:without([<<"channelId">>, <<"createdAt">>, <<"updatedAt">>], Data),
            AppData = maps:get(<<"appdata">>, Args, #{}),
            R = dgiot_parse:update_object(<<"Channel">>, ChannelId, NewData#{
                <<"config">> => NewConfig#{<<"appdata">> => AppData#{<<"sessionToken">> => SessionToken}}
            }, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
            ?LOG(info, "NewData ~p", [NewData]),
            dgiot_bridge:control_channel(ChannelId, <<"disable">>),
            dgiot_bridge:control_channel(ChannelId, <<"enable">>),
            R;
        Error -> Error
    end;

do_request(put_task, #{<<"vcaddr">> := VcAddr} = Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "Args ~p", [Args]),
    Where =
        case VcAddr of
            <<"all">> -> #{<<"where">> => #{<<"cType">> => <<"INSTRUCT">>}};
            _ -> #{<<"where">> => #{<<"config.vcaddr">> => VcAddr, <<"cType">> => <<"INSTRUCT">>}}
        end,
    case dgiot_parse:query_object(<<"Channel">>, Where, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId} = Data | _]}} ->
            Config = maps:get(<<"config">>, Data),
            NewConfig = maps:merge(Config, maps:without([<<"channelId">>, <<"vcaddr">>], Args)),
            NewData = maps:without([<<"channelId">>, <<"createdAt">>, <<"updatedAt">>], Data),
            R = dgiot_parse:update_object(<<"Channel">>, ChannelId, NewData#{
                <<"config">> => NewConfig
            }, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
            ?LOG(info, "NewData ~p", [NewData]),
            dgiot_bridge:control_channel(ChannelId, <<"disable">>),
            dgiot_bridge:control_channel(ChannelId, <<"enable">>),
            R;
        Error -> Error
    end;


do_request(delete_task, #{<<"id">> := VcAddr} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "Args ~p", [_Args]),
    Where =
        case VcAddr of
            <<"all">> -> #{<<"where">> => #{<<"cType">> => <<"INSTRUCT">>}};
            _ -> #{<<"where">> => #{<<"config.vcaddr">> => VcAddr, <<"cType">> => <<"INSTRUCT">>}}
        end,
    case dgiot_parse:query_object(<<"Channel">>, Where, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId} | _]} = Result} when length(Result) > 0 ->
            dgiot_bridge:control_channel(ChannelId, <<"disable">>),
            {ok, #{<<"results">> => Result}};
        Error -> Error
    end;


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.





