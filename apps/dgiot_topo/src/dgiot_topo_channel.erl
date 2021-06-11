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
-module(dgiot_topo_channel).
%%-behavior(dgiot_channelx).
-define(TYPE, <<"DGIOTTOPO">>).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-record(state, {id, env = #{}}).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"TOPO组态通道"/utf8>>
    },
    description => #{
        zh => <<"TOPO组态通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"TOPOBRAND">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"dgiottopo"/utf8>>,
        title => #{
            zh => <<"DGIOT厂商"/utf8>>
        },
        description => #{
            zh => <<"DGIOT厂商"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => 1
    }).

%% 通道初始化
init(?TYPE, ChannelId, #{<<"product">> := _Products} = ChannelArgs) ->
    NewEnv = get_newenv(ChannelArgs),
    State = #state{
        id = ChannelId,
        env = NewEnv#{}
    },
    {ok, State}.

%% 初始化池子
handle_init(State) ->
    dgiot_topo:get_product(),
    dgiot_mqtt:subscribe(<<"thing/topo/rest">>),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info,"channel ~p, ~p", [EventId, Event]),
    ok.

handle_message({deliver, _Topic, Msg}, State) ->
    Payload = binary_to_term(dgiot_mqtt:get_payload(Msg)),
    ?LOG(info,"Payload ~p", [Payload]),
    case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
        [<<"thing">>, <<"topo">>, <<"rest">>] ->
            dgiot_mqtt:subscribe(Payload);
        [<<"thing">>, ProductId, Devaddr, <<"post">>] ->
            DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
            Pubtopic = <<"thing/", DeviceId/binary, "/post">>,
            Base64 = base64:encode(jsx:encode(Payload)),
            dgiot_mqtt:publish(self(), Pubtopic, Base64);
        _ ->
            pass
    end,
    {ok, State};

handle_message(Message, State) ->
    ?LOG(info,"channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

get_newenv(Args) ->
    maps:without([
        <<"behaviour">>,
        <<"MaxOverFlow">>,
        <<"Size">>,
        <<"applicationtText">>,
        <<"product">>], Args).
