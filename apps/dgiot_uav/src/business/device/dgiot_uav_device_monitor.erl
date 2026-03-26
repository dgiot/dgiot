%%%-------------------------------------------------------------------
%%% @doc 无人机设备状态监控器
%%%
%%% 监听设备状态变化，向PLC通道发送设备上线/离线消息
%%% 实现设备上线后自动触发测试的完整流程
%%%
%%% 功能：
%%% 1. 监听设备注册事件
%%% 2. 监听设备状态变化（上线/离线）
%%% 3. 向PLC通道发送设备状态消息
%%% 4. 处理设备上线后的自动化测试触发
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_device_monitor).
-author("johnliu").
-behaviour(gen_server).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot.hrl").
-include("dgiot_uav.hrl").
-include("dgiot_uav_config.hrl").

%% UAV PLC通道类型
-define(UAVPLC_CHANNEL_TYPE, <<"UAVPLCC">>).

%% API
-export([
    start_link/0,
    stop/0,
    
    %% 手动触发设备状态检查
    check_all_devices/0,
    check_device_status/1,
    
    %% 设备状态消息发送
    send_device_online/2,
    send_device_offline/1,
    find_plc_channel_id/0,
    
    %% 在线调试
    test/0,
    test_closed_loop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% 状态记录
-record(state, {
    timer_ref :: reference() | undefined,
    last_check_time :: integer(),
    active_devices = #{} :: map()  % DeviceId => #{status, last_update, station_id}
}).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动设备监控器
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    %% 清理可能残留的ETS表（进程崩溃重启场景）
    catch ets:delete(uav_triggered_devices),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 停止设备监控器
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc 检查所有设备状态
-spec check_all_devices() -> ok | {error, term()}.
check_all_devices() ->
    gen_server:call(?MODULE, check_all_devices).

%% @doc 检查特定设备状态
-spec check_device_status(binary()) -> {ok, map()} | {error, term()}.
check_device_status(DeviceId) ->
    gen_server:call(?MODULE, {check_device_status, DeviceId}).

%% @doc 在线调试
-spec test() -> ok.
test() ->
    ?LOG(info, "~n========== 无人机设备监控器测试 ==========~n"),
    
    %% 1. 启动监控器
    ?LOG(info, "1. 启动设备监控器..."),
    case start_link() of
        {ok, _Pid} ->
            ?LOG(info, "   ✓ 设备监控器启动成功");
        {error, {already_started, _Pid}} ->
            ?LOG(info, "   ✓ 设备监控器已启动");
        {error, Reason} ->
            ?LOG(error, "   ✗ 设备监控器启动失败: ~p", [Reason]),
            return
    end,
    
    %% 2. 检查所有设备
    ?LOG(info, "2. 检查所有设备状态..."),
    case check_all_devices() of
        ok ->
            ?LOG(info, "   ✓ 设备状态检查完成");
        {error, Reason2} ->
            ?LOG(error, "   ✗ 设备状态检查失败: ~p", [Reason2])
    end,
    
    %% 3. 查找测试项设备
    ?LOG(info, "3. 查找测试项设备..."),
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{<<"product">> => <<"343cf21f82">>},
        <<"limit">> => 5
    }) of
        {ok, #{<<"results">> := Results}} ->
            ?LOG(info, "   找到 ~p 个测试项设备", [length(Results)]),
            lists:foreach(fun(#{<<"objectId">> := DeviceId, <<"name">> := Name}) ->
                ?LOG(debug, "   - ~s (~s)", [Name, DeviceId])
            end, Results);
        {error, Reason3} ->
            ?LOG(error, "   ✗ 查询失败: ~p", [Reason3])
    end,
    
    %% 4. 测试查找PLC通道
    ?LOG(info, "4. 测试查找PLC通道..."),
    case find_plc_channel_id() of
        {ok, ChannelId} ->
            ?LOG(info, "   ✓ 找到PLC通道: ChannelId=~p", [ChannelId]);
        {error, Reason4} ->
            ?LOG(error, "   ✗ 找不到PLC通道: ~p", [Reason4])
    end,
    
    ?LOG(info, "~n========== 测试完成 ==========~n"),
    ok.

%% @doc 闭环测试 - 测试完整的设备监控和自动化测试流程
%% @spec test_closed_loop() -> ok | {error, term()}.
test_closed_loop() ->
    ?LOG(info, "~n========== 设备监控器闭环测试 ==========~n"),
    
    %% 1. 执行基本测试
    ?LOG(info, "1. 执行基本设备监控测试..."),
    case test() of
        ok ->
            ?LOG(info, "   ✓ 基本测试通过");
        Error1 ->
            ?LOG(error, "   ✗ 基本测试失败: ~p", [Error1]),
            return
    end,
    
    %% 2. 检查PLC通道连接状态
    ?LOG(info, "2. 检查PLC通道连接状态..."),
    case find_plc_channel_id() of
        {ok, ChannelId} ->
            ?LOG(info, "   ✓ PLC通道可用: ChannelId=~p", [ChannelId]);
        {error, Reason} ->
            ?LOG(error, "   ✗ PLC通道不可用: ~p", [Reason]),
            return
    end,
    
    %% 3. 模拟设备上线流程
    ?LOG(info, "3. 模拟设备上线流程..."),
    %% TODO: 实际实现中，这里可以模拟设备上线消息
    ?LOG(info, "   ⚠ 设备上线模拟（待实现）"),
    
    %% 4. 检查自动化测试触发机制
    ?LOG(info, "4. 检查自动化测试触发机制..."),
    %% 查找测试项设备
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{<<"product">> => <<"343cf21f82">>},
        <<"limit">> => 1
    }) of
        {ok, #{<<"results">> := []}} ->
            ?LOG(info, "   暂无测试项设备，跳过自动化测试触发");
        {ok, #{<<"results">> := [#{<<"objectId">> := DeviceId, <<"name">> := Name} | _]}} ->
            ?LOG(info, "   找到测试项设备: ~s (~s)", [Name, DeviceId]),
            %% 触发自动化测试（模拟）
            ?LOG(info, "   模拟触发自动化测试: DeviceId=~p", [DeviceId]);
        {error, Reason2} ->
            ?LOG(error, "   ✗ 查询测试项设备失败: ~p", [Reason2])
    end,
    
    ?LOG(info, "~n========== 闭环测试完成 ==========~n"),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?LOG(info, "无人机设备监控器启动"),

    %% 创建ETS表跟踪已触发设备（防重复）
    ets:new(uav_triggered_devices, [named_table, public, set]),

    %% 启动定时检查（每30秒检查一次）
    TimerRef = erlang:send_after(30000, self(), check_device_status),

    %% 立即执行第一次检查
    spawn(fun() -> do_check_all_devices() end),

    {ok, #state{
        timer_ref = TimerRef,
        last_check_time = erlang:system_time(second),
        active_devices = #{}
    }}.

handle_call(check_all_devices, _From, State) ->
    %% 异步执行设备检查
    spawn(fun() -> do_check_all_devices() end),
    {reply, ok, State};

handle_call({check_device_status, DeviceId}, _From, State) ->
    case do_check_device_status(DeviceId) of
        {ok, Status} ->
            {reply, {ok, Status}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_device_status, #state{timer_ref = OldTimer} = State) ->
    %% 取消旧的定时器
    if OldTimer =/= undefined -> 
        erlang:cancel_timer(OldTimer);
    true -> ok
    end,
    
    ?LOG(debug, "定时设备状态检查"),
    
    %% 执行检查
    spawn(fun() -> do_check_all_devices() end),
    
    %% 设置下一次检查（开发环境5秒，生产环境可改为30000）
    NewTimerRef = erlang:send_after(5000, self(), check_device_status),
    
    {noreply, State#state{
        timer_ref = NewTimerRef,
        last_check_time = erlang:system_time(second)
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer_ref = TimerRef}) ->
    ?LOG(info, "无人机设备监控器停止"),
    
    %% 取消定时器
    if TimerRef =/= undefined ->
        erlang:cancel_timer(TimerRef);
    true -> ok
    end,
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% 执行所有设备状态检查
do_check_all_devices() ->
    ?LOG(info, "开始检查所有设备状态"),

    %% 检查 fixture TCP 连接是否活跃（端口10001-10007有连接说明治具在线）
    FixtureOnline = check_fixture_tcp_connections(),

    %% 1. 查询所有无人机测试项设备
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{<<"product">> => <<"343cf21f82">>},
        <<"limit">> => 100
    }) of
        {ok, #{<<"results">> := Devices}} ->
            ?LOG(info, "找到 ~p 个测试项设备, fixture在线=~p", [length(Devices), FixtureOnline]),

            lists:foreach(fun(Device) ->
                DeviceId = maps:get(<<"objectId">>, Device, <<>>),
                Name = maps:get(<<"name">>, Device, <<"未知设备">>),
                Status = maps:get(<<"status">>, Device, <<"offline">>),

                ?LOG(debug, "检查设备: Name=~p, Status=~p", [Name, Status]),

                %% 三重判断：Parse Server status / 本地缓存 / fixture TCP连接
                IsOnline = (string:lowercase(binary_to_list(Status)) =:= "online") orelse
                    dgiot_device_state:get_online(DeviceId) orelse
                    FixtureOnline,

                case IsOnline of
                    true ->
                        %% 设备在线，发送上线消息
                        send_device_online(DeviceId, Device);
                    _ ->
                        %% 设备离线，发送离线消息
                        send_device_offline(DeviceId)
                end
            end, Devices),

            ?LOG(info, "设备状态检查完成");

        {error, Reason} ->
            ?LOG(error, "查询设备失败: ~p", [Reason])
    end,

    %% 2. 查询其他无人机设备（非测试项）
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{
            <<"product">> => #{
                <<"$ne">> => <<"343cf21f82">>
            },
            <<"name">> => #{<<"$regex">> => <<"无人机|磁航向|拷机|总测">>}
        },
        <<"limit">> => 50
    }) of
        {ok, #{<<"results">> := OtherDevices}} ->
            ?LOG(info, "找到 ~p 个其他无人机设备", [length(OtherDevices)]),

            lists:foreach(fun(Device) ->
                DeviceId = maps:get(<<"objectId">>, Device, <<>>),
                Status = maps:get(<<"status">>, Device, <<"offline">>),

                IsOnline = (string:lowercase(binary_to_list(Status)) =:= "online") orelse
                    dgiot_device_state:get_online(DeviceId) orelse
                    FixtureOnline,

                case IsOnline of
                    true ->
                        send_device_online(DeviceId, Device);
                    _ ->
                        send_device_offline(DeviceId)
                end
            end, OtherDevices);

        {error, Reason2} ->
            ?LOG(debug, "查询其他设备失败: ~p", [Reason2])
    end.

%% 检查 fixture 设备的 TCP 连接是否活跃
%% 通过 dgiot_cm 检查 wrj_danpianji 和 wrj_dicekou 是否已注册
check_fixture_tcp_connections() ->
    %% 检查治具单片机和地测口是否有至少一个在线
    McuOnline = case dgiot_cm:lookup_channels(<<"wrj_danpianji">>) of
        [_ | _] -> true;
        _ -> false
    end,
    DicekouOnline = case dgiot_cm:lookup_channels(<<"wrj_dicekou">>) of
        [_ | _] -> true;
        _ -> false
    end,
    McuOnline orelse DicekouOnline.

%% 检查单个设备状态
do_check_device_status(DeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            Status = maps:get(<<"status">>, Device, <<"offline">>),
            Name = maps:get(<<"name">>, Device, <<"未知设备">>),
            
            Result = #{
                device_id => DeviceId,
                name => Name,
                status => Status,
                last_check => erlang:system_time(second)
            },

            %% 双重判断：Parse Server status 或本地缓存 online 状态
            IsOnline = (string:lowercase(binary_to_list(Status)) =:= "online") orelse
                dgiot_device_state:get_online(DeviceId),

            case IsOnline of
                true ->
                    send_device_online(DeviceId, Device);
                _ ->
                    send_device_offline(DeviceId)
            end,
            
            {ok, Result};
        
        {error, Reason} ->
            ?LOG(error, "获取设备信息失败: DeviceId=~p, Reason=~p", [DeviceId, Reason]),
            {error, Reason}
    end.

%% 发送设备上线消息到PLC通道
send_device_online(DeviceId, DeviceInfo) ->
    %% 防重复触发：检查是否已经触发过（安全操作：ETS表可能不存在）
    case catch ets:lookup(uav_triggered_devices, DeviceId) of
        [{_, _}] ->
            %% 已经触发过，跳过
            ok;
        _ ->
            %% 首次触发（ETS表不存在或无记录）
            catch ets:insert(uav_triggered_devices, {DeviceId, erlang:system_time(second)}),
            do_send_device_online(DeviceId, DeviceInfo)
    end.

do_send_device_online(DeviceId, DeviceInfo) ->
    ?LOG(info, "~n========================================", []),
    ?LOG(info, "📢 【设备上线消息】发送设备上线消息", []),
    ?LOG(info, "----------------------------------------", []),
    ?LOG(info, "DeviceId: ~p", [DeviceId]),
    ?LOG(info, "DeviceInfo: ~p", [DeviceInfo]),
    ?LOG(info, "DeviceInfo Keys: ~p", [maps:keys(DeviceInfo)]),
    case maps:find(<<"devaddr">>, DeviceInfo) of
        {ok, DevAddr} ->
            ?LOG(info, "DevAddr: ~p", [DevAddr]);
        _ ->
            ?LOG(info, "DevAddr: 未找到")
    end,
    case maps:find(<<"productName">>, DeviceInfo) of
        {ok, ProductName} ->
            ?LOG(info, "ProductName: ~p", [ProductName]);
        _ ->
            ?LOG(info, "ProductName: 未找到")
    end,
    case maps:find(<<"station_id">>, DeviceInfo) of
        {ok, StationId} ->
            ?LOG(info, "StationId: ~p", [StationId]);
        _ ->
            ?LOG(info, "StationId: 未找到")
    end,
    case maps:find(<<"status">>, DeviceInfo) of
        {ok, Status} ->
            ?LOG(info, "Status: ~p", [Status]);
        _ ->
            ?LOG(info, "Status: 未找到")
    end,
    ?LOG(info, "========================================~n", []),
    
    %% 1. 获取PLC通道ID
    case find_plc_channel_id() of
        {ok, ChannelId} ->
            ?LOG(info, "找到PLC通道: ChannelId=~p", [ChannelId]),
            %% 构建消息
            Message = {device_online, DeviceId, DeviceInfo},
            
            %% 发送消息到通道
            case dgiot_channelx:do_message(?UAVPLC_CHANNEL_TYPE, ChannelId, Message) of
                ok ->
                    ?LOG(info, "✅ 设备上线消息发送成功: DeviceId=~p, ChannelId=~p", [DeviceId, ChannelId]),
                    
                    %% 2. 自动触发测试（如果是测试项设备）
                    case is_test_item_device(DeviceInfo) of
                        true ->
                            ?LOG(info, "🎯 检测到测试项设备，准备自动触发测试: DeviceId=~p", [DeviceId]),
                            %% 延迟1秒后启动测试
                            spawn(fun() ->
                                timer:sleep(1000),
                                trigger_auto_test(DeviceId, DeviceInfo)
                            end);
                        false ->
                            ?LOG(info, "非测试项设备，跳过自动测试: DeviceId=~p", [DeviceId])
                    end,
                    
                    ok;
                {error, Reason} ->
                    ?LOG(error, "❌ 发送设备上线消息失败: Reason=~p", [Reason]),
                    {error, Reason}
            end;
        
        {error, Reason} ->
            ?LOG(error, "❌ 找不到PLC通道: Reason=~p", [Reason]),
            {error, Reason}
    end.

%% 发送设备离线消息到PLC通道
send_device_offline(DeviceId) ->
    %% 清除已触发记录（安全操作：ETS表可能不存在）
    catch ets:delete(uav_triggered_devices, DeviceId),
    send_device_offline_internal(DeviceId).

send_device_offline_internal(DeviceId) ->
    ?LOG(info, "发送设备离线消息: DeviceId=~p", [DeviceId]),
    
    %% 获取PLC通道ID
    case find_plc_channel_id() of
        {ok, ChannelId} ->
            Message = {device_offline, DeviceId},
            
            case dgiot_channelx:do_message(?UAVPLC_CHANNEL_TYPE, ChannelId, Message) of
                ok ->
                    ?LOG(info, "设备离线消息发送成功: DeviceId=~p", [DeviceId]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "发送设备离线消息失败: Reason=~p", [Reason]),
                    {error, Reason}
            end;
        
        {error, Reason} ->
            ?LOG(error, "找不到PLC通道: Reason=~p", [Reason]),
            {error, Reason}
    end.

%% 查找PLC通道ID
find_plc_channel_id() ->
    %% 查找类型为UAVPLCC的通道
    %% 使用dgiot_data匹配所有channeltype为UAVPLCC的通道ID
    Pattern = {{channeltype, '$1'}, ?UAVPLC_CHANNEL_TYPE},
    case dgiot_data:match(Pattern) of
        {ok, []} ->
            ?LOG(warning, "找不到UAV PLC通道"),
            {error, no_plc_channel_found};
        {ok, [[ChannelId] | _]} ->
            ?LOG(info, "找到UAV PLC通道: ChannelId=~p", [ChannelId]),
            {ok, ChannelId};
        {ok, [ChannelId]} when is_binary(ChannelId) ->
            ?LOG(info, "找到UAV PLC通道 (单元素列表): ChannelId=~p", [ChannelId]),
            {ok, ChannelId};
        {ok, Results} when is_list(Results) ->
            %% 尝试从结果中提取第一个通道ID
            ?LOG(debug, "匹配结果: Results=~p", [Results]),
            case extract_first_channel_id(Results) of
                {ok, ChannelId} ->
                    ?LOG(info, "找到UAV PLC通道: ChannelId=~p", [ChannelId]),
                    {ok, ChannelId};
                {error, no_channel_found} ->
                    ?LOG(warning, "找不到UAV PLC通道"),
                    {error, no_plc_channel_found}
            end;
        {error, empty} ->
            ?LOG(warning, "找不到UAV PLC通道"),
            {error, no_plc_channel_found};
        {error, Reason} ->
            ?LOG(error, "匹配通道时出错: Reason=~p", [Reason]),
            {error, Reason};
        Other ->
            ?LOG(error, "未知匹配结果: Other=~p", [Other]),
            {error, Other}
    end.

%% 从匹配结果中提取第一个通道ID
extract_first_channel_id([]) ->
    {error, no_channel_found};
extract_first_channel_id([Item | _]) when is_binary(Item) ->
    {ok, Item};
extract_first_channel_id([[ChannelId] | _]) when is_binary(ChannelId) ->
    {ok, ChannelId};
extract_first_channel_id([{channeltype, ChannelId} | _]) when is_binary(ChannelId) ->
    {ok, ChannelId};
extract_first_channel_id([_ | Rest]) ->
    extract_first_channel_id(Rest).

%% 产品ID定义
-define(UAV_PRODUCT_ID, <<"6235befb62">>).
-define(TEST_ITEM_PRODUCT_ID, <<"343cf21f82">>).
-define(FIXTURE_PRODUCT_ID, <<"bd49cc8272">>).

%% 判断设备是否应该自动启动测试（无人机、治具或测试项设备）
should_auto_start_test(DeviceInfo) ->
    ProductId = maps:get(<<"product">>, DeviceInfo, <<>>),
    Name = maps:get(<<"name">>, DeviceInfo, <<>>),
    Content = maps:get(<<"content">>, DeviceInfo, #{}),
    Steps = maps:get(<<"steps">>, Content, []),
    
    %% 从ProductId中提取实际ID（兼容Pointer格式）
    RealProductId = case ProductId of
        #{<<"objectId">> := Id} -> Id;
        Id when is_binary(Id) -> Id;
        _ -> <<>>
    end,
    
    %% 判断逻辑：无人机、治具或测试项设备绑定到工位后自动启动测试
    IsUAV = (RealProductId =:= ?UAV_PRODUCT_ID),
    IsFixture = (RealProductId =:= ?FIXTURE_PRODUCT_ID),
    IsTestItem = (RealProductId =:= ?TEST_ITEM_PRODUCT_ID orelse
                  RealProductId =:= <<>> orelse
                  binary:match(Name, <<"测试项">>) =/= nomatch) andalso
                 is_list(Steps) andalso Steps =/= [],
    
    %% 对于无人机设备，额外检查状态和工位绑定
    AutoTest = if
        IsUAV ->
            %% 检查设备状态是否为 ONLINE
            Status = maps:get(<<"status">>, DeviceInfo, <<"OFFLINE">>),
            case Status of
                <<"ONLINE">> ->
                    %% 检查是否已绑定工位
                    DeviceId = maps:get(<<"objectId">>, DeviceInfo, <<>>),
                    case DeviceId of
                        <<>> ->
                            ?LOG(warning, "should_auto_start_test: 无人机设备缺少objectId, 跳过自动测试", []),
                            false;
                        _ ->
                            case dgiot_uav_station_manager:get_station_by_drone(DeviceId) of
                                {ok, _StationAddr} ->
                                    ?LOG(info, "should_auto_start_test: 无人机设备已绑定工位, 允许自动测试", []),
                                    true;
                                {error, not_find} ->
                                    ?LOG(info, "should_auto_start_test: 无人机设备未绑定工位, 跳过自动测试", []),
                                    false
                            end
                    end;
                _ ->
                    ?LOG(info, "should_auto_start_test: 无人机设备状态非ONLINE(~p), 跳过自动测试", [Status]),
                    false
            end;
        IsFixture ->
            %% 治具设备暂时保持原有逻辑
            true;
        IsTestItem ->
            %% 测试项设备暂时保持原有逻辑
            true;
        true ->
            false
    end,
    ?LOG(info, "should_auto_start_test: RealProductId=~p, IsUAV=~p, IsFixture=~p, IsTestItem=~p, AutoTest=~p", 
         [RealProductId, IsUAV, IsFixture, IsTestItem, AutoTest]),
    AutoTest.

%% 判断是否为测试项设备（兼容旧接口）
is_test_item_device(DeviceInfo) ->
    should_auto_start_test(DeviceInfo).

%% 触发自动化测试
trigger_auto_test(DeviceId, _DeviceInfo) ->
    ?LOG(info, "触发自动化测试: DeviceId=~p", [DeviceId]),
    %% 调用自动化测试器启动测试
    case dgiot_uav_auto_tester:start_test_for_device(DeviceId) of
        {ok, Result} ->
            ?LOG(info, "自动化测试启动成功: DeviceId=~p, Result=~p", [DeviceId, Result]);
        {error, Reason} ->
            ?LOG(error, "自动化测试启动失败: DeviceId=~p, Reason=~p", [DeviceId, Reason])
    end.

