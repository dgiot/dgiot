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

-module(dgiot_uav_udp_channel).
-behaviour(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-define(TYPE, <<"UAV_UDP">>).
-record(state, {
    id,
    multicast_group,
    ports,  % 改为端口列表
    interface,
    product_id = <<>>,
    env = #{},
    
    % 无人机ID到目标地址的映射
    uav_mapping = #{} :: #{binary() => #{
        target_ip => string(),
        target_port => integer(),
        last_seen => integer()
    }},
    
    % 统计信息
    stats = #{
        packets_received => 0,
        packets_forwarded => 0,
        packets_dropped => 0,
        bytes_received => 0,
        bytes_forwarded => 0
    },
    
    % 清理定时器
    cleanup_timer
}).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"无人机UDP多播转发通道"/utf8>>
    },
    description => #{
        zh => <<"无人机UDP多播转发通道，捕获多播报文，提取无人机ID，根据ID转发到目标地址"/utf8>>
    }
}).

%% 注册通道参数
-params(#{
    <<"multicast_group">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"226.0.0.80">>,
        title => #{
            zh => <<"多播组地址"/utf8>>
        },
        description => #{
            zh => <<"要监听的UDP多播组地址"/utf8>>
        }
    },
    <<"ports">> => #{
        order => 2,
        type => array,
        required => true,
        default => [8000, 8001, 8002],
        title => #{
            zh => <<"多播端口列表"/utf8>>
        },
        description => #{
            zh => <<"UDP多播端口列表，如[8000,8001,8002]"/utf8>>
        }
    },
    <<"interface">> => #{
        order => 3,
        type => string,
        required => false,
        default => <<"0.0.0.0">>,
        title => #{
            zh => <<"网络接口"/utf8>>
        },
        description => #{
            zh => <<"监听的网络接口IP地址，0.0.0.0表示所有接口"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/uav_udp_channel.png">>,
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

%% 启动通道
start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{
    <<"multicast_group">> := MulticastGroup,
    <<"ports">> := Ports,
    <<"interface">> := Interface
} = Args) ->
    
    % 调试输出 - 看看实际收到的是什么
    % io:format("~n🔍🔍🔍 [UDP_CHANNEL_INIT] 开始初始化UDP多播通道/ 🔍🔍🔍~n"),
    io:format("~s ~p [UDP_CHANNEL_INIT] ChannelId = ~p~n", [?FILE, ?LINE, ChannelId]),
    io:format("~s ~p [UDP_CHANNEL_INIT] MulticastGroup = ~p~n", [?FILE, ?LINE, MulticastGroup]),
    io:format("~s ~p [UDP_CHANNEL_INIT] Ports = ~p~n", [?FILE, ?LINE, Ports]),
    io:format("~s ~p [UDP_CHANNEL_INIT] Interface = ~p~n", [?FILE, ?LINE, Interface]),
    % io:format("~s ~p [UDP_CHANNEL_INIT] Args = ~p~n", [?FILE, ?LINE, Args]),
    
    % 转换IP地址进行验证
    case inet:parse_address(binary_to_list(MulticastGroup)) of
        {ok, MulticastIP} ->
            io:format("~s ~p [UDP_CHANNEL_INIT] ✅ ~ts: ~p~n", [?FILE, ?LINE, <<"成功解析多播IP"/utf8>>, MulticastIP]);
        {error, Reason} ->
            io:format("~s ~p [UDP_CHANNEL_INIT] ~ts   ~p, ~ts ~p~n", 
                     [?FILE, ?LINE, <<"失败解析多播IP"/utf8>>, MulticastGroup, <<"原因:"/utf8>>, Reason])
    end,
    
    State = #state{
        id = ChannelId,
        multicast_group = MulticastGroup,
        ports = Ports,
        interface = Interface,
        product_id = <<"ProductId">>,
        env = maps:get(<<"env">>, Args, #{})
    },
    
    ?LOG(info, "~ts: ~p", [<<"无人机UDP多播转发通道启动"/utf8>>, ChannelId]),
    ?LOG(info, "~ts: ~p, ~ts: ~p", [<<"监听多播组"/utf8>>, MulticastGroup, <<"端口列表"/utf8>>, Ports]),
    
    % 为每个端口创建worker
    ChildSpecs = lists:map(fun(Port) ->
        io:format("~s ~p [UDP_CHANNEL_INIT] 为端口 ~p 创建worker~n", [?FILE, ?LINE, Port]),
        dgiot_uav_udp_worker:child_spec(Port, State)
    end, Ports),
    
    io:format("✅✅✅ [UDP_CHANNEL_INIT] UDP多播通道初始化完成，监听 ~p 个端口 ✅✅✅~n~n", [length(Ports)]),
    
    {ok, State, ChildSpecs}.

handle_init(State) ->
    ?LOG(info, "~ts: ~p", [<<"无人机UDP多播转发通道初始化完成"/utf8>>, State#state.id]),
    {ok, State}.

%% 通道消息处理
handle_event(_EventId, Event, State) ->
    ?LOG(info, "~ts: ~p", [<<"通道事件"/utf8>>, Event]),
    {ok, State}.

%% 通道消息处理<<"uav_mapping">> := UavMapping
handle_message(Message, State) ->
    ?LOG(debug, "~ts: ~p", [<<"收到通道消息"/utf8>>, Message]),
    
    case Message of
        {get_stats, Pid} ->
            % 获取统计信息
            Pid ! {udp_stats, State#state.stats},
            {ok, State};
        
        {reset_stats, Pid} ->
            % 重置统计信息
            NewState = State#state{stats = #{
                packets_received => 0,
                packets_forwarded => 0,
                packets_dropped => 0,
                bytes_received => 0,
                bytes_forwarded => 0
            }},
            Pid ! {stats_reset, ok},
            {ok, NewState};
        
        _ ->
            {ok, State}
    end.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "~ts[~p,~p]", [<<"通道停止"/utf8>>, ChannelType, ChannelId]),
    ok.
