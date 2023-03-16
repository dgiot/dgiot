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

-module(dgiot_location_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_location.hrl").
-define(TYPE, <<"LOCATION">>).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?FEATURE_CHL,
    title => #{
        zh => <<"LOCATION应用通道"/utf8>>
    },
    description => #{
        zh => <<"LOCATION应用通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"search">> => #{
        order => 2,
        type => enum,
        required => false,
        default => <<"IP"/utf8>>,
        enum => [<<"IP"/utf8>>, <<"BTS"/utf8>>, <<"WIFI"/utf8>>, <<"GPS"/utf8>>],
        title => #{
            zh => <<"定位模式"/utf8>>
        },
        description => #{
            zh => <<"定位模式:IP|BTS|WIFI|GPS"/utf8>>
        }
    },
    <<"geotype">> => #{
        order => 2,
        type => enum,
        required => false,
        default => <<"City"/utf8>>,
        enum => [<<"ASN"/utf8>>, <<"City"/utf8>>, <<"Country"/utf8>>],
        title => #{
            zh => <<"IP位置类型"/utf8>>
        },
        description => #{
            zh => <<"通过ip查找位置:ASN|City|Country"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/location.jpg">>,
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
init(?TYPE, ChannelId, #{
    <<"product">> := _Products,
    <<"geotype">> := Geotype,
    <<"search">> := Search}) ->
    dgiot_geoip:start(Geotype),
    dgiot_data:set_consumer(ChannelId, 20),
    State = #state{
        id = ChannelId,
        search = Search
    },
    {ok, State, []};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.


%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, State) ->
    ?LOG(error, "EventId ~p Event ~p", [EventId, Event]),
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
