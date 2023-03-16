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
-module(dgiot_bacnet_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"BACNET">>).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-record(state, {id, step, env = #{}}).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"BACnet采集通道"/utf8>>
    },
    description => #{
        zh => <<"BACnet采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"CLINEADDRESS">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"http://192.168.1.13:8080"/utf8>>,
        title => #{
            zh => <<"BACnet采集地址"/utf8>>
        },
        description => #{
            zh => <<"BACnet采集地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 47808,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/BACnet.jpg">>,
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
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => 1
    }).

%% 通道初始化
init(?TYPE, ChannelId, #{<<"port">> := Port}) ->
    NewArgs = #{
        <<"port">> => Port
    },
    State = #state{
        id = ChannelId,
        env = NewArgs
    },
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 5000, dgiot_datetime:now_secs() + 300000),
    ChildSpec = dgiot_bacnet_broadcast:childspec(ChannelId, NewArgs) ++ dgiot_bacnet_worker:childspec(ChannelId, NewArgs),
    {ok, State, ChildSpec}.

%% 初始化池子
handle_init(State) ->
    {ok, State}.


%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventId, Event]),
    ok.

handle_message(start_client, #state{id = ChannelId, env = #{<<"port">> := Port}} = State) ->
    case dgiot_data:get({start_client, ChannelId}) of
        not_find ->
            lists:map(fun(Ip) ->
                dgiot_bacnet_broadcast:start_connect(ChannelId, #{
                    <<"auto_reconnect">> => 10,
                    <<"port">> => Port,
                    <<"ip">> => Ip
                })
                      end, dgiot_udp_broadcast:get_ipaddrs());
        _ ->
            pass
    end,
    {ok, State};

handle_message({deliver, _Topic, Msg}, State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    case jsx:is_json(Payload) of
        false ->
            {ok, State};
        true ->
            {ok, State}
    end;


handle_message({whois, Ip, Port, _Buff}, #state{id = ChannelId} = State) ->
    dgiot_bacnet_worker:start_connect(ChannelId,
        #{
            <<"auto_reconnect">> => 10,
            <<"ip">> => dgiot_utils:to_list(Ip),
            <<"port">> => Port
        }),
    {ok, State};

handle_message(Message, State) ->
    ?LOG(info, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

