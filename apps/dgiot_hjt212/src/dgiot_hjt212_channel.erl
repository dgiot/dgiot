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
-module(dgiot_hjt212_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_hjt212.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"hjt212">>).
-dgiot_data("ets").
-export([init_ets/0]).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"HJT212"/utf8>>
    },
    description => #{
        zh => <<"hjt212"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 7533,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"devtype">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"环境监测"/utf8>>,
        title => #{
            zh => <<"环境监测"/utf8>>
        },
        description => #{
            zh => <<"厂家名称"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/modbus.png">>,
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

init_ets() ->
    dgiot_data:init(?HJT212_ETS).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{
    <<"port">> := Port,
    <<"devtype">> := DevType
} = _Args) ->
    State = #state{
        id = ChannelId,
        devtype = DevType
    },
    case dgiot_parse:get_object(<<"Channel">>, ChannelId) of
        {ok, Channel} ->
            App = get_app(Channel),
            {ok, State, dgiot_hjt212_tcp:start(Port, State#state{app = App})};
        _ ->
            {ok, State, []}
    end;

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    dgiot_hjt212:load_thing(),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_message({rule, #{clientid := DevAddr, connected_at := _ConnectedAt}, #{peername := PeerName} = _Context}, State) ->
    ?LOG(error, "DevAddr ~p PeerName ~p", [DevAddr, PeerName]),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, disconnected_at := _DisconnectedAt}, _Context}, State) ->
    ?LOG(error, "DevAddr ~p ", [DevAddr]),
    {ok, State};

handle_message({rule, #{clientid := DevAddr, payload := Payload, topic := _Topic}, _Msg}, #state{id = ChannelId} = State) ->
    ?LOG(error, "DevAddr ~p Payload ~p ChannelId ~p", [DevAddr, Payload, ChannelId]),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.

get_app(#{<<"ACL">> := Acl}) ->
    Predicate = fun(E) ->
        case E of
            <<"role:", _/binary>> -> true;
            _ -> false
        end
                end,
    [<<"role:", App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
    App.


