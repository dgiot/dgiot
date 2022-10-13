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
-module(dgiot_printer_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_printer.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"PRINTER">>).
-record(state, {id, env}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"打印机资源通道"/utf8>>
    },
    description => #{
        zh => <<"打印机资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/printer.png">>,
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
    dgiot_printer:start_hooks(Args),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "channel ~p", [Event]),
    {ok, State}.

handle_message({dlink_login, do_after, ProductId, Devaddr, DeviceId, _Ip}, State) ->
    timer:sleep(1 * 1000),
    RequestTopic = <<"$dg/device/", ProductId/binary, "/", Devaddr/binary, "/properties">>,
    Payload = #{<<"cmd">> => <<"scan_printer">>, <<"data">> => #{}},
    dgiot_mqtt:publish(DeviceId, RequestTopic, jsx:encode(Payload)),
    {ok, State};

handle_message({dlink_firmware_report, ProductId, DevAddr, Payload}, State) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            Profile = maps:get(<<"profile">>, Device, #{}),
            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"profile">> => Profile#{<<"printers">> => Payload}});
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(ChannelType, ChannelId, #state{env = Args} = _State) ->
    dgiot_printer:stop_hooks(Args),
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.
