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
-module(dgiot_ota_channel).
-behavior(dgiot_channelx).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"OTA">>).
-record(state, {
    id,
    product,
    path,
    md5,
    env = #{}}
).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3, send_upgrade/1]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    title => #{
        zh => <<"OTA升级通道"/utf8>>
    },
    description => #{
        zh => <<"OTA升级通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/NOTIFICATION.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    dgiot_parse_hook:subscribe(<<"Files/*">>, put, ChannelId, [<<"content">>]),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

%% 设备端上报升级进度信息
handle_message({ota_progress, ProductId, DevAddr, _Payload}, State) ->
    _DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    {ok, State};

%% 上报最新版本
handle_message({firmware_report, ProductId, DevAddr, #{<<"version">> := Version}}, State) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{}),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"basedata">> := Basedata} = _Device} ->
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => Basedata#{<<"version">> => Version}});
        _ ->
            pass
    end,
    send_upgrade(State);
121

%% 升级命令
handle_message({sync_parse, _Pid, 'after', put, _Token, <<"Files">>, #{<<"content">> := #{
    <<"enable">> := true, <<"key">> := ProductId, <<"path">> := Path, <<"md5">> := Md5} = Content}}, #state{env = Env} = State) ->
    Devices = get_deviceids(Content),
    {ok, State#state{product = ProductId, path = Path, md5 = Md5, env = Env#{<<"devices">> => Devices}}};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.

%% 全部设备
get_deviceids(#{<<"upgraderange">> := <<"0">>, <<"key">> := ProductId}) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"devaddr">>], <<"where">> => #{<<"product">> => ProductId}}) of
        {ok, #{<<"results">> := Devices}} when length(Devices) > 0 ->
            lists:foldl(fun(#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr}, Acc) ->
                Acc ++ [#{<<"objectId">> => DeviceId, <<"devaddr">> => Devaddr}]
                        end, [], Devices);
        _ -> []
    end;

%% 定向升级
get_deviceids(#{<<"upgraderange">> := <<"1">>, <<"picker">> := Picker}) ->
    case binary:split(Picker, <<$,>>, [global, trim]) of
        DevIds when length(DevIds) > 0 ->
            lists:foldl(fun(DeviceId, Acc) ->
                case dgiot_device:lookup(DeviceId) of
                    {ok, #{<<"devaddr">> := Devaddr}} ->
                        Acc ++ [#{<<"objectId">> => DeviceId, <<"devaddr">> => Devaddr}];
                    _ ->
                        Acc
                end
                        end, [], DevIds);
        _ ->
            []

    end;

%% 分组升级
get_deviceids(#{<<"upgraderange">> := <<"2">>, <<"danwei">> := RoleName}) ->
    case dgiot_parse_auth:check_roles(RoleName) of
        {200, #{<<"access_token">> := Depart_token}} ->
            case dgiot_parse:query_object(<<"Device">>, #{}, [{"X-Parse-Session-Token", Depart_token}], [{from, rest}]) of
                {ok, #{<<"results">> := Devices}} when length(Devices) > 0 ->
                    lists:foldl(fun(#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr}, Acc) ->
                        Acc ++ [#{<<"objectId">> => DeviceId, <<"devaddr">> => Devaddr}]
                                end, [], Devices);
                _ -> []
            end;
        _ ->
            []
    end;

%%
get_deviceids(_Content) ->
    [].

send_upgrade(#state{env = #{<<"devices">> := Devices}} = State) when length(Devices) == 0 ->
    {stop, State};

send_upgrade(#state{product = ProductId, path = Path, md5 = Md5, env = #{<<"devices">> := [#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr} | Rest]} = Env} = State) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := Devaddr}} ->
            Topic = <<"$dg/device/", ProductId/binary, "/", Devaddr/binary, "/ota/upgrade">>,
            Data = #{<<"path">> => Path, <<"md5">> => Md5},
            dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Data));
        _ ->
            pass
    end,
    {ok, State#state{env = Env#{<<"devices">> => Rest}}}.
















