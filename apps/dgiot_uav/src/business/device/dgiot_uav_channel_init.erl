%%-------------------------------------------------------------------
%% @doc
%% dgiot_uav_channel_init - 无人机通道初始化模块
%% 根据超近距无人机测试产线设计模式，只监听20000端口
%% 设计原理：端口作为设备类型标识符，所有设备连接到统一入口20000端口
%% @end
%%-------------------------------------------------------------------
-module(dgiot_uav_channel_init).
-author("DGIOT Team").

-include_lib("dgiot/include/logger.hrl").

%% API
-export([start/0, start_all/0, stop_all/0]).

%% 根据超近距无人机测试产线核心设计模式：
%% 固定端口作为设备类型标识符，只监听20000端口
%% 所有设备都连接到统一入口20000端口，服务端根据端口号自动路由到对应处理器

%% UAV TCP主通道配置（只监听20000端口）
-define(UAV_TCP_MAIN_CHANNEL, {20000, <<"uav_main_channel">>, <<"无人机TCP主通道"/utf8>>}).

%% @doc 启动UAV TCP主通道
start() ->
    ?LOG(info, "========== 开始初始化UAV TCP通道 (仅20000端口) ==========", []),
    start_all(),
    ?LOG(info, "========== UAV TCP通道初始化完成 ==========", []),
    ok.

%% @doc 启动UAV TCP主通道（仅20000端口）
%% 根据超近距无人机测试产线设计模式：固定端口作为设备类型标识符
%% 所有设备都连接到统一入口20000端口，服务端根据端口号自动路由到对应处理器
start_all() ->
    ?LOG(info, "[Channel Init] 启动无人机TCP主通道 (端口: 20000)", []),
    
    {Port, ChannelId, ChannelName} = ?UAV_TCP_MAIN_CHANNEL,
    
    %% 检查通道是否已存在
    case dgiot_channelx:lookup(ChannelId) of
        {ok, _Channel} ->
            ?LOG(info, "[Channel Init] 通道已存在，跳过: ~p (~p)", [ChannelName, Port]);
        {error, not_found} ->
            %% 创建主通道
            Args = #{
                <<"port">> => Port,
                <<"product_id">> => <<"">>,
                <<"env">> => #{
                    <<"channel_name">> => ChannelName,
                    <<"description">> => <<"无人机TCP主通道，处理所有设备连接"/utf8>>
                }
            },
            
            case dgiot_channelx:add(<<"UAV_TCP">>, ChannelId, dgiot_uav_tcp_channel, Args) of
                {ok, _Pid} ->
                    ?LOG(info, "[Channel Init] ✓ 主通道启动成功: ~p (~p)", [ChannelName, Port]);
                {error, Reason} ->
                    ?LOG(error, "[Channel Init] ✗ 主通道启动失败: ~p (~p), 原因: ~p", 
                          [ChannelName, Port, Reason])
            end
    end,
    
    ok.

%% @doc 停止UAV TCP主通道
stop_all() ->
    ?LOG(info, "[Channel Init] 开始停止无人机TCP主通道", []),
    
    {Port, ChannelId, ChannelName} = ?UAV_TCP_MAIN_CHANNEL,
    
    case dgiot_channelx:delete(<<"UAV_TCP">>, ChannelId) of
        ok ->
            ?LOG(info, "[Channel Init] ✓ 主通道停止成功: ~p (~p)", [ChannelName, Port]);
        {error, Reason} ->
            ?LOG(warning, "[Channel Init] ✗ 主通道停止失败: ~p (~p), 原因: ~p", 
                   [ChannelName, Port, Reason])
    end,
    
    ok.
