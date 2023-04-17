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

-module(dgiot_dlink_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"DLINK">>).
-author("johnliu").
-record(state, {id}).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"Dlink采集通道"/utf8>>
    },
    description => #{
        zh => <<"Dlink采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"network">> => #{
        order => 1,
        type => enum,
        required => false,
        default => <<"mqtt"/utf8>>,
        enum => [
            #{<<"value">> => <<"grpc">>, <<"label">> => <<"grpc服务端"/utf8>>},
            #{<<"value">> => <<"mqtt">>, <<"label">> => <<"mqtt服务端"/utf8>>},
            #{<<"value">> => <<"tcp">>, <<"label">> => <<"tcp服务端"/utf8>>},
            #{<<"value">> => <<"udp">>, <<"label">> => <<"udp服务端"/utf8>>},
            #{<<"value">> => <<"http">>, <<"label">> => <<"http服务端"/utf8>>}
        ],
        title => #{
            zh => <<"网络组件"/utf8>>
        },
        description => #{
            zh => <<"网络组件"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 30051,
        title => #{
            zh => <<"设备接入端口"/utf8>>
        },
        description => #{
            zh => <<"设备接入端口"/utf8>>
        }
    },
    <<"url">> => #{
        order => 3,
        type => string,
        required => false,
        default => <<"tcp://127.0.0.1:30051"/utf8>>,
        title => #{
            zh => <<"设备协议解析"/utf8>>
        },
        description => #{
            zh => <<"设备协议解析: product | tcp://127.0.0.1:30051"/utf8>>
        }
    },
    <<"rule">> => #{
        order => 4,
        type => string,
        required => false,
        default => <<"rule:956cc06e"/utf8>>,
        title => #{
            zh => <<"消息流转规则"/utf8>>
        },
        description => #{
            zh => <<"消息流转规则"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/dgiot_dlink_channel.png">>,
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
%%<<"network">> => <<"grpc">>,<<"port">> => 30051,
%%<<"product">> => [],<<"rule">> => <<"rule:956cc06e">>,
%%<<"url">> => <<"tcp://127.0.0.1:30051">>
init(?TYPE, ChannelId, #{<<"network">> := _NetWork} = _ChannelArgs) ->
    State = #state{id = ChannelId},
%%    io:format("_ChannelArgs ~p ~n",[_ChannelArgs]),
    dgiot_grpc_client:login(ChannelId),
    {ok, State}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event('client.connected', {rule, #{clientid := _ClientId }, _Msg} = _Event, State) ->
    io:format("~s ~p _EventId ~p , _ClientId ~p ~n", [?FILE, ?LINE, 'client.connected', _ClientId]),
    {ok, State};

handle_event(_EventId, _Event, State) ->
    io:format("~s ~p _EventId ~p , _Event ~p ~n", [?FILE, ?LINE, _EventId, _Event]),
    {ok, State}.

handle_message({dlink_login, do_after, ProductId, DeviceAddr, Ip}, State) ->
    dgiot_device:create_device(ProductId, DeviceAddr, Ip),
    {ok, State};

handle_message({rule,#{clientid := _ClientId, payload := _Payload, topic := _Topic}, _Msg}, State) ->
%%    io:format("~s ~p _ClientId ~p , Payload ~p , Topic ~p ~n", [?FILE, ?LINE, _ClientId, _Payload, _Topic]),
    {ok, State};

handle_message(_Message, State) ->
%%    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(_ChannelType, ChannelId, _State) ->
    dgiot_grpc_client:logout(ChannelId),
    ok.


