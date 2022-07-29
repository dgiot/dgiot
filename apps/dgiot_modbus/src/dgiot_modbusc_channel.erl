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
-module(dgiot_modbusc_channel).
-behavior(dgiot_channelx).
-include("dgiot_modbus.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"MODBUSC">>).
%% API
-export([
    start/2
]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
-define(consumer(ChannelId), <<"modbus_consumer_", ChannelId/binary>>).

-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"MODBUSC资源通道"/utf8>>
    },
    description => #{
        zh => <<"MODBUSC资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ip">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1"/utf8>>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 502,
        title => #{
            zh => <<"服务器端口"/utf8>>
        },
        description => #{
            zh => <<"服务器端口"/utf8>>
        }
    },
    <<"file">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"modbustcp">>,
        title => #{
            zh => <<"文件名"/utf8>>
        },
        description => #{
            zh => <<"文件名"/utf8>>
        }
    },
    <<"freq">> => #{
        order => 4,
        type => integer,
        required => true,
        default => 60,
        title => #{
            zh => <<"采集频率/秒"/utf8>>
        },
        description => #{
            zh => <<"采集频率/秒"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/modbusc.png">>,
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
    <<"ip">> := Ip,
    <<"port">> := Port,
    <<"freq">> := Freq,
    <<"Size">> := Size
} = Args) ->
    {FileName, MinAddr, MaxAddr} =
        case maps:find(<<"file">>, Args) of
            {ok, FileName1} ->
                {MinAddr1, MaxAddr1} = dgiot_product_csv:read_csv(ChannelId, FileName1),
                %% modbus_tcp:set_addr(ChannelId, MinAddr1, MaxAddr1),
                {FileName1, MinAddr1, MaxAddr1};
            _ ->
                {<<>>, 0, 100}
        end,
    NewArgs = #{
        <<"ip">> => Ip,
        <<"port">> => Port,
        <<"mod">> => dgiot_modbusc_tcp,
        <<"child">> => #{
            filename => FileName,
            data => <<>>,
            freq => Freq,
            minaddr => MinAddr,
            maxaddr => MaxAddr}},
%%    dgiot_client:add_clock(ChannelId, Start_time, End_time),
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 5000, dgiot_datetime:now_secs() + 300000),
    {ok, #state{id = ChannelId, env = #{size => Size}}, dgiot_client:register(ChannelId, tcp_client_sup, NewArgs)}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event]),
    {ok, State}.

handle_message(start_client, #state{id = ChannelId, env = #{size := Size}} = State) ->
    case dgiot_data:get({start_client, ChannelId}) of
        not_find ->
            [dgiot_client:start(ChannelId, dgiot_utils:to_binary(I)) || I <- lists:seq(1, Size)];
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.
