-module(dgiot_uav_command_scheduler).
-behaviour(gen_server).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/dgiot_uav.hrl").

-compile([nowarn_unused_function, nowarn_unused_vars]).

%% 导入链路跟踪器
-import(dgiot_uav_command_tracer_simple, [
    log_command_start/5,
    log_command_send/6,
    log_command_complete/5,
    log_command_error/6
]).

%% API
-export([start_link/0, station_bind/2, station_unbind/1, send_command/4, send_command/5, send_command/6, send_command_array/3, parse_device_info/1]).

%% 内部函数导出（用于测试）
-export([handle_test_item_command/4, handle_single_command/6, map_test_item_to_address/1]).

%% 新增：target_type到slave_address映射
-export([target_to_slave_addr/1]).

%% 在线调试函数导出
-export([test/0, test_start/0, test_send_plc_command/0, test_send_fixture_command/0, 
         test_send_uav_command/0, test_send_command_array/0, test_station_bind/0, 
         test_station_unbind/0, run_all_tests/0, test_send_single_plc_command/0]).

%% 产品配置比对函数导出
-export([compare_product_configs/0, get_product_config/1, validate_command_mapping/0, 
         check_product_configs/0, export_product_configs/0,
         query_product_thing/1, explore_parse_tables/0,
         summarize_command_storage/0, validate_command_sets/0]).

%% 设备查询函数导出
-export([query_devices_by_product/0, query_device_config/1, find_workstation_devices/0, 
         find_test_item_devices/0, extract_commands_from_device/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

station_bind(StationId, DroneId) ->
    gen_server:cast(?MODULE, {station_bind, StationId, DroneId}).

station_unbind(StationId) ->
    gen_server:cast(?MODULE, {station_unbind, StationId}).

send_command_array(DevAddr, SlaveAddr, Instructions) when is_list(Instructions) ->
    gen_server:call(?MODULE, {send_commands, DevAddr, SlaveAddr, Instructions}).

%% 新接口（简洁版，根据DevAddr前缀自动推断SlaveAddr）
send_command(DevAddr, InstAddr, Value, TestItemId, StepIndex) ->
    ?LOG(info, "send_command/5 被调用: DevAddr=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p",
         [DevAddr, InstAddr, Value, TestItemId, StepIndex]),
    {_DeviceType, _StationId, SlaveAddr} = parse_device_info(DevAddr),
    gen_server:call(?MODULE, {send_command, DevAddr, SlaveAddr, InstAddr, Value, TestItemId, StepIndex}).

%% 兼容接口（6参数，带SlaveAddr）
send_command(DevAddr, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    ?LOG(info, "send_command/6 被调用: DevAddr=~p, SlaveAddr=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p",
         [DevAddr, SlaveAddr, InstAddr, Value, TestItemId, StepIndex]),
    gen_server:call(?MODULE, {send_command, DevAddr, SlaveAddr, InstAddr, Value, TestItemId, StepIndex}).

%% 旧接口（4参数，调用兼容接口，传递 undefined）
send_command(DevAddr, SlaveAddr, InstAddr, Value) ->
    send_command(DevAddr, SlaveAddr, InstAddr, Value, undefined, undefined).

init([]) ->
    ?LOG(info, "命令调度器启动"),
    {ok, #state{}}.

handle_cast({station_bind, StationId, DroneId}, State) ->
    case dgiot_uav_stub_functions:get_station_commands(StationId) of
        {ok, Commands, _Interval} ->
            ?LOG(info, "工位 ~p 绑定无人机 ~s，准备执行指令集", [StationId, DroneId]),
            spawn(fun() -> execute_commands(StationId, Commands) end);
        {error, not_find} ->
            ?LOG(info, "工位 ~p 未配置指令集，不执行测试", [StationId])
    end,
    {noreply, State};

handle_cast({station_unbind, _StationId}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({send_commands, DevAddr, SlaveAddr, Instructions}, _From, State) ->
    StationId = parse_station_id(DevAddr),
    Results = lists:map(fun(Instruction) ->
        case Instruction of
            {TestItem, Value} when is_binary(TestItem) ->
                handle_test_item_command(StationId, SlaveAddr, TestItem, Value);
            {InstAddr, Value} when is_integer(InstAddr) ->
                handle_single_command(StationId, SlaveAddr, InstAddr, Value, undefined, undefined);
            _ ->
                {error, invalid_instruction_format}
        end
    end, Instructions),
    {reply, Results, State};

handle_call({send_command, DevAddr, SlaveAddr, InstAddr, Value, TestItemId, StepIndex}, _From, State) ->
    StationId = parse_station_id(DevAddr),
    ?LOG(info, "命令调度器收到指令: StationId=~p, SlaveAddr=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p",
         [StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex]),
    Result = case SlaveAddr of
        10007 -> safe_handle_uav_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex);
        51    -> safe_handle_plc_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex);
        10006 -> safe_handle_fixture_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex);
        _     -> safe_handle_single_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex)
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 安全包装函数，捕获异常并返回错误
safe_handle_uav_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    try handle_uav_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
        Result -> Result
    catch
        Class:Reason:Stack ->
            ?LOG(error, "处理无人机指令异常: ~p:~p ~p", [Class, Reason, Stack]),
            {error, {internal_error, Reason}}
    end.

safe_handle_plc_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    try handle_plc_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
        Result -> Result
    catch
        Class:Reason:Stack ->
            ?LOG(error, "处理PLC指令异常: ~p:~p ~p", [Class, Reason, Stack]),
            {error, {internal_error, Reason}}
    end.

safe_handle_fixture_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    try handle_fixture_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
        Result -> Result
    catch
        Class:Reason:Stack ->
            ?LOG(error, "处理治具指令异常: ~p:~p ~p", [Class, Reason, Stack]),
            {error, {internal_error, Reason}}
    end.

safe_handle_single_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    try handle_single_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
        Result -> Result
    catch
        Class:Reason:Stack ->
            ?LOG(error, "处理通用指令异常: ~p:~p ~p", [Class, Reason, Stack]),
            {error, {internal_error, Reason}}
    end.

execute_commands(StationId, Commands) ->
    ?LOG(info, "工位 ~p 开始执行指令集: ~p", [StationId, Commands]),
    case dgiot_uav_plc_tcp_client:start_continuous_test(StationId, Commands) of
        ok ->
            ?LOG(info, "工位 ~p 指令集已启动", [StationId]);
        {error, Reason} ->
            ?LOG(error, "工位 ~p 启动指令集失败: ~p", [StationId, Reason])
    end.

%% 解析工位ID，支持多种前缀格式
%% P1200, F1200, U1200, D1200 都解析为 1200
parse_station_id(<<"P", Bin/binary>>) ->
    binary_to_integer(Bin);
parse_station_id(<<"F", Bin/binary>>) ->
    binary_to_integer(Bin);
parse_station_id(<<"U", Bin/binary>>) ->
    binary_to_integer(Bin);
parse_station_id(<<"D", Bin/binary>>) ->
    binary_to_integer(Bin);
parse_station_id(Bin) when is_binary(Bin) ->
    try
        binary_to_integer(Bin)
    catch
        _:_ ->
            ?LOG(error, "【命令调度器】无法解析工位ID: ~p, 返回默认值0", [Bin]),
            0
    end;
parse_station_id(Other) ->
    ?LOG(error, "【命令调度器】工位ID格式错误: ~p, 期望为binary, 返回默认值0", [Other]),
    0.

%% 解析设备信息，根据DevAddr前缀区分设备类型
%% P前缀: PLC设备 (SlaveAddr=51)
%% F前缀: 治具设备 (SlaveAddr=10006)
%% U前缀: 无人机设备 (SlaveAddr=10007)
%% D前缀: 兼容旧格式，默认为PLC设备 (SlaveAddr=51)
parse_device_info(<<"P", Bin/binary>>) ->
    StationId = binary_to_integer(Bin),
    {plc, StationId, 51};
parse_device_info(<<"F", Bin/binary>>) ->
    StationId = binary_to_integer(Bin),
    {fixture, StationId, 10006};
parse_device_info(<<"U", Bin/binary>>) ->
    StationId = binary_to_integer(Bin),
    {uav, StationId, 10007};
parse_device_info(<<"D", Bin/binary>>) ->
    %% 向后兼容：D前缀默认为PLC设备
    StationId = binary_to_integer(Bin),
    {plc, StationId, 51};
parse_device_info(Bin) when is_binary(Bin) ->
    %% 默认解析为PLC设备（兼容旧代码）
    StationId = binary_to_integer(Bin),
    {plc, StationId, 51};
parse_device_info(_) ->
    error(badarg).

%% 辅助函数：将二进制DroneId转换为整数地址
%% 支持格式：<<"drone_3">> -> 3, 直接整数 -> 整数, 其他 -> 默认0
drone_id_to_addr(DroneId) when is_binary(DroneId) ->
    case binary:split(DroneId, <<"_">>, [global]) of
        [<<"drone">>, NumBin] ->
            try binary_to_integer(NumBin) catch _:_ -> 16#0000 end;
        _ ->
            16#0000
    end;
drone_id_to_addr(DroneId) when is_integer(DroneId) ->
    DroneId;
drone_id_to_addr(_) ->
    16#0000.

%% 处理无人机指令（使用统一命令管理器，支持3发2空模式）
handle_uav_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    ?LOG(info, "处理无人机指令: StationId=~p, SlaveAddr=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p",
         [StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex]),
    case get_drone_by_slave(StationId, SlaveAddr) of
        {ok, DroneId} ->
            %% 将二进制DroneId转换为整数地址
            DestAddr = drone_id_to_addr(DroneId),
            %% 使用命令管理器发送无人机指令（支持3发2空模式）
            Params = #{
                dest_addr => DestAddr,
                src_addr => 16#0001,
                frame_no => 1,
                station_id => StationId,
                test_item_id => TestItemId,
                step_index => StepIndex
            },
            case dgiot_uav_command_manager:send_uav_single(InstAddr, Value, Params) of
                ok ->
                    ?LOG(info, "无人机指令发送成功: StationId=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p", 
                         [StationId, InstAddr, Value, TestItemId, StepIndex]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "无人机指令发送失败: StationId=~p, InstAddr=~p, Reason=~p", 
                         [StationId, InstAddr, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

send_udp_multicast(Frame) ->
    try
        case gen_udp:open(0, [binary]) of
            {ok, Socket} ->
                ok = gen_udp:send(Socket, {226,0,0,80}, 8002, Frame),
                gen_udp:close(Socket),
                ok;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:Exception:Stack ->
            ?LOG(error, "UDP发送异常: ~p:~p ~p", [Class, Exception, Stack]),
            {error, {udp_error, Exception}}
    end.

%% 处理PLC指令 (端口51) - 使用统一命令管理器，不支持3发2空模式
handle_plc_command(StationId, _SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    ?LOG(info, "处理PLC指令: StationId=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p",
         [StationId, InstAddr, Value, TestItemId, StepIndex]),
    
    %% 使用命令管理器发送PLC指令
    %% send_plc_command参数: Code(指令码), Value(指令值), Params(包含station_id等)
    %% 这里InstAddr是指令地址,Value是指令值
    Params = #{
        station_id => StationId,
        test_item_id => TestItemId,
        step_index => StepIndex
    },
    %% 将InstAddr作为Code参数传递,Value作为Value参数
    case dgiot_uav_command_manager:send_plc_command(InstAddr, Value, Params) of
        ok ->
            ?LOG(info, "PLC指令发送成功: StationId=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p", 
                 [StationId, InstAddr, Value, TestItemId, StepIndex]),
            ok;
        {error, Reason} ->
            ?LOG(error, "PLC指令发送失败: StationId=~p, InstAddr=~p, Reason=~p", 
                 [StationId, InstAddr, Reason]),
            {error, Reason}
    end.

%% 处理治具指令 - 使用统一命令管理器，支持3发2空模式
handle_fixture_command(StationId, _SlaveAddr, InstAddr, Value, _TestItemId, _StepIndex) ->
    ?LOG(info, "处理治具指令: StationId=~p, InstAddr=~p, Value=~p",
         [StationId, InstAddr, Value]),
    
    %% 使用统一命令管理器发送指令（支持3发2空模式）
    Params = #{
        station_id => StationId
    },
    case dgiot_uav_command_manager:send_fixture_command(InstAddr, Value, Params) of
        ok ->
            ?LOG(info, "治具指令发送成功: StationId=~p, InstAddr=~p, Value=~p", 
                 [StationId, InstAddr, Value]),
            ok;
        {error, Reason} ->
            ?LOG(error, "治具指令发送失败: StationId=~p, InstAddr=~p, Reason=~p", 
                 [StationId, InstAddr, Reason]),
            {error, Reason}
    end.

%% 处理测试项指令（旧接口，可能来自 send_command_array）
handle_test_item_command(StationId, SlaveAddr, TestItem, Value) ->
    InstAddr = map_test_item_to_address(TestItem),
    handle_single_command(StationId, SlaveAddr, InstAddr, Value, undefined, undefined).

%% 处理单个指令（通用，6参数版本）
handle_single_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) ->
    %% 确定命令类型
    CommandType = case SlaveAddr of
        10007 -> uav;
        51    -> plc;
        10006 -> fixture;
        _ -> unknown
    end,
    
    %% 记录命令开始
    TraceId = log_command_start(CommandType, StationId, SlaveAddr, InstAddr, #{
        value => Value,
        test_item_id => TestItemId,
        step_index => StepIndex
    }),
    
    %% 记录命令调度器节点
    log_command_send(TraceId, command_scheduler, ?MODULE, 0, tcp, #{
        station_id => StationId,
        slave_addr => SlaveAddr,
        inst_addr => InstAddr,
        value => Value
    }),
    
    %% 根据SlaveAddr分发到不同的处理函数
    Result = case SlaveAddr of
        10007 -> 
            log_command_send(TraceId, command_scheduler, ?MODULE, 0, udp, #{target => "uav"}),
            handle_uav_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex);
        51    -> 
            log_command_send(TraceId, command_scheduler, ?MODULE, 0, tcp, #{target => "plc"}),
            handle_plc_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex);
        10006 -> 
            log_command_send(TraceId, command_scheduler, ?MODULE, 0, tcp, #{target => "fixture"}),
            handle_fixture_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex);
        _ ->
            log_command_error(TraceId, command_scheduler, ?MODULE, {invalid_slave_address, SlaveAddr}, 0, #{}),
            {error, {invalid_slave_address, SlaveAddr}}
    end,
    
    %% 记录命令结果
    case Result of
        ok ->
            log_command_complete(TraceId, command_scheduler, ?MODULE, 0, #{result => "success"});
        {error, Reason} ->
            log_command_error(TraceId, command_scheduler, ?MODULE, Reason, 0, #{});
        _ ->
            ok
    end,
    
    Result.

map_test_item_to_address(TestItem) ->
    case TestItem of
        <<"控制大继电器上电"/utf8>> -> 16#0000;
        <<"控制大继电器断电"/utf8>> -> 16#0000;
        <<"启动无人机"/utf8>> -> 16#0001;
        <<"停止无人机"/utf8>> -> 16#0001;
        <<"关闭风筒"/utf8>> -> 16#0002;
        <<"打开风筒"/utf8>> -> 16#0002;
        <<"测试引信9_10点电阻"/utf8>> -> 16#0000;
        <<"测试引信7_8点电阻"/utf8>> -> 16#0002;
        <<"测试引信7翼钉电阻"/utf8>> -> 16#0004;
        <<"测试引信8翼钉电阻"/utf8>> -> 16#0006;
        <<"测试电池端口电阻"/utf8>> -> 16#0008;
        <<"测试引信5对地电压"/utf8>> -> 16#000A;
        <<"测试引信1对地电压"/utf8>> -> 16#0008;
        <<"读取工位信息"/utf8>> -> 16#000D;
        _ -> 16#0000
    end.

get_drone_by_slave(StationId, SlaveAddr) ->
    case SlaveAddr of
        10007 -> dgiot_uav_business_service:get_station_drone(StationId);
        10006 -> dgiot_uav_business_service:get_station_drone(StationId);
        _ when SlaveAddr >= 1100, SlaveAddr =< 1700 ->
            {ok, <<"drone_", (integer_to_binary(StationId))/binary>>};
        _ ->
            {error, unsupported_slave_address}
    end.

target_to_slave_addr(<<"plc">>) -> 51;
target_to_slave_addr(<<"fixture">>) -> 52;
target_to_slave_addr(<<"uav">>) -> 10007;
target_to_slave_addr(_) -> 0.

%% ===================================================================
%% 在线调试函数
%% ===================================================================

%% @doc 在线测试入口函数
test() ->
    io:format("=== DGIOT UAV 命令调度器在线测试 ===~n"),
    
    %% 检查进程状态
    io:format("1. 检查命令调度器进程状态...~n"),
    case whereis(?MODULE) of
        undefined ->
            io:format("   ✗ 命令调度器进程未启动~n"),
            io:format("   尝试启动命令调度器...~n"),
            case start_link() of
                {ok, Pid} ->
                    io:format("   ✓ 命令调度器启动成功，PID: ~p~n", [Pid]);
                {error, Reason} ->
                    io:format("   ✗ 命令调度器启动失败: ~p~n", [Reason])
            end;
        Pid ->
            io:format("   ✓ 命令调度器进程已启动，PID: ~p~n", [Pid]),
            io:format("   进程信息: ~p~n", [process_info(Pid)])
    end,
    
    %% 测试地址映射
    io:format("~n2. 测试地址映射功能...~n"),
    TestItems = [
        {<<"控制大继电器上电"/utf8>>, 16#0000},
        {<<"启动无人机"/utf8>>, 16#0001},
        {<<"打开风筒"/utf8>>, 16#0002},
        {<<"测试引信9_10点电阻"/utf8>>, 16#0000},
        {<<"读取工位信息"/utf8>>, 16#000D}
    ],
    
    lists:foreach(
        fun({TestItem, ExpectedAddr}) ->
            ActualAddr = map_test_item_to_address(TestItem),
            case ActualAddr of
                ExpectedAddr ->
                    io:format("   ✓ ~ts -> 0x~.4B (正确)~n", [TestItem, ActualAddr]);
                _ ->
                    io:format("   ✗ ~ts -> 0x~.4B (期望: 0x~.4B)~n", 
                             [TestItem, ActualAddr, ExpectedAddr])
            end
        end,
        TestItems
    ),
    
    %% 测试目标类型到从站地址映射
    io:format("~n3. 测试目标类型到从站地址映射...~n"),
    Targets = [
        {<<"plc">>, 51},
        {<<"fixture">>, 52},
        {<<"uav">>, 10007},
        {<<"unknown">>, 0}
    ],
    
    lists:foreach(
        fun({Target, ExpectedAddr}) ->
            ActualAddr = target_to_slave_addr(Target),
            case ActualAddr of
                ExpectedAddr ->
                    io:format("   ✓ ~ts -> ~p (正确)~n", [Target, ActualAddr]);
                _ ->
                    io:format("   ✗ ~ts -> ~p (期望: ~p)~n", 
                             [Target, ActualAddr, ExpectedAddr])
            end
        end,
        Targets
    ),
    
    %% 测试发送命令（模拟）
    io:format("~n4. 测试命令发送功能（模拟）...~n"),
    TestCommands = [
        {plc, 1, 51, 1700, 1, <<"test_item_1">>, 1},
        {fixture, 2, 10006, 16#0001, 1, <<"test_item_2">>, 2},
        {uav, 3, 10007, 16#0002, 1, <<"test_item_3">>, 3}
    ],
    
    lists:foreach(
        fun({Type, StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex}) ->
            io:format("   测试 ~s 命令: StationId=~p, SlaveAddr=~p, InstAddr=~p~n", 
                     [Type, StationId, SlaveAddr, InstAddr]),
            
            %% 模拟调用handle_single_command
            try
                Result = handle_single_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex),
                case Result of
                    ok ->
                        io:format("      ✓ 命令处理成功~n");
                    {error, ErrorReason} ->
                        io:format("      ✗ 命令处理失败: ~p~n", [ErrorReason]);
                    _ ->
                        io:format("      ? 未知结果: ~p~n", [Result])
                end
            catch
                Class:Exception:Stack ->
                    io:format("      ✗ 命令处理异常: ~p:~p~n", [Class, Exception]),
                    io:format("      Stack: ~p~n", [Stack])
            end
        end,
        TestCommands
    ),
    
    %% 测试进程注册表
    io:format("~n5. 检查相关进程注册...~n"),
    Registered = registered(),
    Schedulers = [Name || Name <- Registered, 
                         string:str(atom_to_list(Name), "scheduler") > 0 orelse
                         string:str(atom_to_list(Name), "command") > 0],
    
    case Schedulers of
        [] ->
            io:format("   未找到命令调度相关进程~n");
        _ ->
            io:format("   找到 ~p 个相关进程: ~p~n", [length(Schedulers), Schedulers])
    end,
    
    %% 测试结果汇总
    io:format("~n=== 测试完成 ===~n"),
    io:format("建议: ~n"),
    io:format("1. 确保命令调度器进程已启动~n"),
    io:format("2. 检查命令管理器是否正常运行~n"),
    io:format("3. 验证PLC、治具、UAV通道连接状态~n"),
    io:format("4. 使用命令跟踪器监控命令流~n"),
    
    {ok, test_completed}.

%% @doc 测试命令调度器启动
test_start() ->
    io:format("=== 测试命令调度器启动 ===~n"),
    case whereis(?MODULE) of
        undefined ->
            io:format("命令调度器未启动，正在启动...~n"),
            case start_link() of
                {ok, Pid} ->
                    io:format("✓ 命令调度器启动成功，PID: ~p~n", [Pid]),
                    {ok, Pid};
                {error, Reason} ->
                    io:format("✗ 命令调度器启动失败: ~p~n", [Reason]),
                    {error, Reason}
            end;
        Pid ->
            io:format("命令调度器已启动，PID: ~p~n", [Pid]),
            {ok, already_started, Pid}
    end.

%% @doc 测试发送PLC命令
test_send_plc_command() ->
    io:format("=== 测试发送PLC命令 ===~n"),
    StationId = 1,
    SlaveAddr = 51,
    InstAddr = 1700,
    Value = 1,
    TestItemId = <<"test_plc_command">>,
    StepIndex = 1,
    
    io:format("参数: StationId=~p, SlaveAddr=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p~n",
             [StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex]),
    
    case send_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
        ok ->
            io:format("✓ PLC命令发送成功~n"),
            ok;
        {error, Reason} ->
            io:format("✗ PLC命令发送失败: ~p~n", [Reason]),
            {error, Reason};
        Other ->
            io:format("? 未知响应: ~p~n", [Other]),
            Other
    end.

%% @doc 测试发送治具命令
test_send_fixture_command() ->
    io:format("=== 测试发送治具命令 ===~n"),
    StationId = 2,
    SlaveAddr = 10006,
    InstAddr = 16#0001,
    Value = 1,
    TestItemId = <<"test_fixture_command">>,
    StepIndex = 1,
    
    io:format("参数: StationId=~p, SlaveAddr=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p~n",
             [StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex]),
    
    case send_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
        ok ->
            io:format("✓ 治具命令发送成功~n"),
            ok;
        {error, Reason} ->
            io:format("✗ 治具命令发送失败: ~p~n", [Reason]),
            {error, Reason};
        Other ->
            io:format("? 未知响应: ~p~n", [Other]),
            Other
    end.

%% @doc 测试发送UAV命令
test_send_uav_command() ->
    io:format("=== 测试发送UAV命令 ===~n"),
    StationId = 3,
    SlaveAddr = 10007,
    InstAddr = 16#0002,
    Value = 1,
    TestItemId = <<"test_uav_command">>,
    StepIndex = 1,
    
    io:format("参数: StationId=~p, SlaveAddr=~p, InstAddr=~p, Value=~p, TestItemId=~p, StepIndex=~p~n",
             [StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex]),
    
    case send_command(StationId, SlaveAddr, InstAddr, Value, TestItemId, StepIndex) of
        ok ->
            io:format("✓ UAV命令发送成功~n"),
            ok;
        {error, Reason} ->
            io:format("✗ UAV命令发送失败: ~p~n", [Reason]),
            {error, Reason};
        Other ->
            io:format("? 未知响应: ~p~n", [Other]),
            Other
    end.

%% @doc 测试命令数组发送
test_send_command_array() ->
    io:format("=== 测试发送命令数组 ===~n"),
    DevAddr = 1,
    SlaveAddr = 51,
    Instructions = [
        {1700, 1},
        {1701, 2},
        {1702, 3}
    ],
    
    io:format("参数: DevAddr=~p, SlaveAddr=~p, Instructions=~p~n",
             [DevAddr, SlaveAddr, Instructions]),
    
    case send_command_array(DevAddr, SlaveAddr, Instructions) of
        ok ->
            io:format("✓ 命令数组发送成功~n"),
            ok;
        {error, Reason} ->
            io:format("✗ 命令数组发送失败: ~p~n", [Reason]),
            {error, Reason};
        Other ->
            io:format("? 未知响应: ~p~n", [Other]),
            Other
    end.

%% @doc 测试工位绑定
test_station_bind() ->
    io:format("=== 测试工位绑定 ===~n"),
    StationId = 1,
    DroneId = <<"drone_001">>,
    
    io:format("绑定工位 ~p 到无人机 ~p~n", [StationId, DroneId]),
    
    case station_bind(StationId, DroneId) of
        ok ->
            io:format("✓ 工位绑定成功~n"),
            ok;
        {error, Reason} ->
            io:format("✗ 工位绑定失败: ~p~n", [Reason]),
            {error, Reason};
        Other ->
            io:format("? 未知响应: ~p~n", [Other]),
            Other
    end.

%% @doc 测试工位解绑
test_station_unbind() ->
    io:format("=== 测试工位解绑 ===~n"),
    StationId = 1,
    
    io:format("解绑工位 ~p~n", [StationId]),
    
    case station_unbind(StationId) of
        ok ->
            io:format("✓ 工位解绑成功~n"),
            ok;
        {error, Reason} ->
            io:format("✗ 工位解绑失败: ~p~n", [Reason]),
            {error, Reason};
        Other ->
            io:format("? 未知响应: ~p~n", [Other]),
            Other
    end.

%% @doc 运行所有测试
run_all_tests() ->
    io:format("=== 运行所有命令调度器测试 ===~n~n"),
    
    Tests = [
        {"测试启动", fun test_start/0},
        {"测试地址映射", fun() -> test() end},
        {"测试PLC命令", fun test_send_plc_command/0},
        {"测试治具命令", fun test_send_fixture_command/0},
        {"测试UAV命令", fun test_send_uav_command/0},
        {"测试命令数组", fun test_send_command_array/0},
        {"测试工位绑定", fun test_station_bind/0},
        {"测试工位解绑", fun test_station_unbind/0},
        {"测试单步PLC命令", fun test_send_single_plc_command/0}
    ],
    
    Results = lists:map(
        fun({TestName, TestFun}) ->
            io:format("▶ 执行测试: ~s~n", [TestName]),
            try
                Result = TestFun(),
                io:format("  结果: ~p~n~n", [Result]),
                {TestName, Result}
            catch
                Class:Exception:Stack ->
                    io:format("  异常: ~p:~p~n", [Class, Exception]),
                    io:format("  堆栈: ~p~n~n", [Stack]),
                    {TestName, {error, {exception, Class, Exception}}}
            end
        end,
        Tests
    ),
    
    %% 统计结果
    Passed = [Name || {Name, Result} <- Results, 
                     Result =:= ok orelse 
                     (is_tuple(Result) andalso element(1, Result) =:= ok)],
    Failed = [Name || {Name, Result} <- Results, 
                     not (Result =:= ok orelse 
                          (is_tuple(Result) andalso element(1, Result) =:= ok))],
    
    io:format("=== 测试结果汇总 ===~n"),
    io:format("总共测试: ~p 个~n", [length(Tests)]),
    io:format("通过: ~p 个 (~s)~n", [length(Passed), string:join(Passed, ", ")]),
    io:format("失败: ~p 个 (~s)~n", [length(Failed), string:join(Failed, ", ")]),
    
    case Failed of
        [] ->
            io:format("✓ 所有测试通过~n"),
            {ok, all_tests_passed};
        _ ->
            io:format("✗ 部分测试失败~n"),
            {error, {some_tests_failed, Failed}}
    end.

%% @doc 在线调试 - 单步测试发送PLC指令
test_send_single_plc_command() ->
    ?LOG(info, "开始单步测试发送PLC指令"),
    
    %% 参数设置
    StationId = 1100,
    InstAddr = 0,
    Value = 1,
    TestItemId = <<"test_item_001">>,
    StepIndex = 1,
    
    ?LOG(info, "测试参数: StationId=~p, InstAddr=~p, Value=~p", [StationId, InstAddr, Value]),
    
    %% 步骤1: 检查PLC进程是否存在
    ?LOG(info, "步骤1: 检查PLC进程 ~p 是否存在", [StationId]),
    case global:whereis_name({plc, StationId}) of
        undefined ->
            ?LOG(error, "PLC进程 ~p 不存在!", [StationId]);
        Pid ->
            ?LOG(info, "PLC进程 ~p 存在: ~p", [StationId, Pid])
    end,
    
    %% 步骤2: 调用send_plc_command
    ?LOG(info, "步骤2: 调用dgiot_uav_command_manager:send_plc_command"),
    Params = #{
        station_id => StationId,
        test_item_id => TestItemId,
        step_index => StepIndex
    },
    
    case dgiot_uav_command_manager:send_plc_command(InstAddr, Value, Params) of
        ok ->
            ?LOG(info, "PLC指令发送成功"),
            ok;
        {error, Reason} ->
            ?LOG(error, "PLC指令发送失败: ~p", [Reason]),
            {error, Reason}
    end.

%% ===================================================================
%% 产品配置比对函数
%% ===================================================================

%% @doc 查询产品配置并比对指令集
compare_product_configs() ->
    io:format("=== 产品配置比对 ===~n~n"),
    
    %% 产品ID列表
    ProductIds = #{
        uav => <<"6235befb62">>,           %% 超近距无人机
        fixture => <<"bd49cc8272">>,       %% 超近距无人机治具
        test_item => <<"343cf21f82">>,     %% 超近距无人机测试项
        workstation => <<"2de1b3e1b8">>    %% 超近距无人机工位
    },
    
    %% 查询所有产品配置
    Results = maps:map(
        fun(Type, ProductId) ->
            io:format("查询 ~s 产品 (ID: ~s)...~n", [Type, ProductId]),
            case dgiot_parse:get_object(<<"Product">>, ProductId) of
                {ok, Product} ->
                    Name = maps:get(<<"name">>, Product, <<"未知">>),
                    Content = maps:get(<<"content">>, Product, #{}),
                    Config = maps:get(<<"config">>, Content, #{}),
                    
                    io:format("  产品名称: ~s~n", [Name]),
                    io:format("  配置内容: ~p~n", [Config]),
                    
                    %% 提取指令集
                    Commands = extract_commands_from_config(Type, Config),
                    io:format("  提取的指令集: ~p~n", [Commands]),
                    
                    {ok, #{name => Name, config => Config, commands => Commands}};
                {error, Reason} ->
                    io:format("  查询失败: ~p~n", [Reason]),
                    {error, Reason}
            end
        end,
        ProductIds
    ),
    
    %% 比对指令集
    io:format("~n=== 指令集比对结果 ===~n"),
    
    %% 获取无人机指令集
    UAVCommands = case maps:get(uav, Results) of
        {ok, #{commands := UAVCmd}} -> UAVCmd;
        _ -> #{}
    end,
    
    %% 获取治具指令集
    FixtureCommands = case maps:get(fixture, Results) of
        {ok, #{commands := FixtureCmd}} -> FixtureCmd;
        _ -> #{}
    end,
    
    %% 获取测试项指令集
    TestItemCommands = case maps:get(test_item, Results) of
        {ok, #{commands := TestCmd}} -> TestCmd;
        _ -> #{}
    end,
    
    %% 比对无人机和治具指令集
    io:format("1. 无人机 vs 治具指令集比对:~n"),
    compare_command_sets(UAVCommands, FixtureCommands, <<"无人机">>, <<"治具">>),
    
    %% 比对测试项指令集
    io:format("~n2. 测试项指令集分析:~n"),
    analyze_test_item_commands(TestItemCommands),
    
    %% 生成比对报告
    io:format("~n=== 比对报告 ===~n"),
    io:format("无人机产品: ~s~n", [get_product_name(Results, uav)]),
    io:format("治具产品: ~s~n", [get_product_name(Results, fixture)]),
    io:format("测试项产品: ~s~n", [get_product_name(Results, test_item)]),
    io:format("工位产品: ~s~n", [get_product_name(Results, workstation)]),
    
    {ok, Results}.

%% @doc 从配置中提取指令集
extract_commands_from_config(uav, Config) ->
    %% 从无人机配置中提取指令
    maps:get(<<"commands">>, Config, #{});
extract_commands_from_config(fixture, Config) ->
    %% 从治具配置中提取指令
    maps:get(<<"commands">>, Config, #{});
extract_commands_from_config(test_item, Config) ->
    %% 从测试项配置中提取指令
    maps:get(<<"test_items">>, Config, #{});
extract_commands_from_config(workstation, Config) ->
    %% 从工位配置中提取指令
    maps:get(<<"workstation_config">>, Config, #{});
extract_commands_from_config(_, _) ->
    #{}.

%% @doc 比对两个指令集
compare_command_sets(Set1, Set2, Name1, Name2) ->
    Keys1 = maps:keys(Set1),
    Keys2 = maps:keys(Set2),
    
    CommonKeys = lists:filter(fun(K) -> lists:member(K, Keys2) end, Keys1),
    OnlyInSet1 = lists:filter(fun(K) -> not lists:member(K, Keys2) end, Keys1),
    OnlyInSet2 = lists:filter(fun(K) -> not lists:member(K, Keys1) end, Keys2),
    
    io:format("  共同指令 (~p 个): ~p~n", [length(CommonKeys), CommonKeys]),
    io:format("  仅存在于 ~s (~p 个): ~p~n", [binary_to_list(Name1), length(OnlyInSet1), OnlyInSet1]),
    io:format("  仅存在于 ~s (~p 个): ~p~n", [binary_to_list(Name2), length(OnlyInSet2), OnlyInSet2]),
    
    %% 检查值是否一致
    Inconsistent = lists:filter(
        fun(Key) ->
            V1 = maps:get(Key, Set1),
            V2 = maps:get(Key, Set2),
            V1 =/= V2
        end,
        CommonKeys
    ),
    
    case Inconsistent of
        [] ->
            io:format("  所有共同指令的值一致~n");
        _ ->
            io:format("  不一致的指令 (~p 个): ~p~n", [length(Inconsistent), Inconsistent]),
            lists:foreach(
                fun(Key) ->
                    io:format("    ~s: ~s=~p, ~s=~p~n", 
                             [Key, binary_to_list(Name1), maps:get(Key, Set1), binary_to_list(Name2), maps:get(Key, Set2)])
                end,
                Inconsistent
            )
    end.

%% @doc 分析测试项指令集
analyze_test_item_commands(TestItems) when is_map(TestItems) ->
    TestItemCount = maps:size(TestItems),
    io:format("  测试项总数: ~p~n", [TestItemCount]),
    
    %% 统计测试项类型
    Types = lists:foldl(
        fun({_Key, Item}, Acc) ->
            Type = maps:get(<<"type">>, Item, <<"unknown">>),
            maps:update_with(Type, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        maps:to_list(TestItems)
    ),
    
    io:format("  测试项类型分布:~n"),
    maps:foreach(
        fun(Type, Count) ->
            io:format("    ~s: ~p 个~n", [Type, Count])
        end,
        Types
    ),
    
    %% 提取所有指令
    AllCommands = lists:foldl(
        fun({_Key, Item}, Acc) ->
            Commands = maps:get(<<"commands">>, Item, []),
            Acc ++ Commands
        end,
        [],
        maps:to_list(TestItems)
    ),
    
    UniqueCommands = lists:usort(AllCommands),
    io:format("  唯一指令数量: ~p~n", [length(UniqueCommands)]),
    io:format("  指令列表: ~p~n", [UniqueCommands]);
analyze_test_item_commands(_) ->
    io:format("  无测试项配置~n").

%% @doc 获取产品名称
get_product_name(Results, Type) ->
    case maps:get(Type, Results) of
        {ok, #{name := Name}} -> Name;
        _ -> <<"未找到">>
    end.

%% @doc 查询特定产品配置
get_product_config(ProductId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, Product} ->
            Name = maps:get(<<"name">>, Product, <<"未知">>),
            Content = maps:get(<<"content">>, Product, #{}),
            Config = maps:get(<<"config">>, Content, #{}),
            {ok, #{name => Name, config => Config}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 验证指令映射
validate_command_mapping() ->
    io:format("=== 验证指令映射 ===~n"),
    
    %% 测试项到地址的映射
    TestItemMappings = [
        {<<"控制大继电器上电"/utf8>>, 16#0000},
        {<<"控制大继电器断电"/utf8>>, 16#0000},
        {<<"启动无人机"/utf8>>, 16#0001},
        {<<"停止无人机"/utf8>>, 16#0001},
        {<<"关闭风筒"/utf8>>, 16#0002},
        {<<"打开风筒"/utf8>>, 16#0002},
        {<<"测试引信9_10点电阻"/utf8>>, 16#0000},
        {<<"测试引信7_8点电阻"/utf8>>, 16#0002},
        {<<"测试引信7翼钉电阻"/utf8>>, 16#0004},
        {<<"测试引信8翼钉电阻"/utf8>>, 16#0006},
        {<<"测试电池端口电阻"/utf8>>, 16#0008},
        {<<"测试引信5对地电压"/utf8>>, 16#000A},
        {<<"测试引信1对地电压"/utf8>>, 16#0008},
        {<<"读取工位信息"/utf8>>, 16#000D}
    ],
    
    io:format("测试项到地址映射 (~p 个):~n", [length(TestItemMappings)]),
    lists:foreach(
        fun({TestItem, Addr}) ->
            io:format("  ~ts -> 0x~.4B~n", [TestItem, Addr])
        end,
        TestItemMappings
    ),
    
    %% 验证映射函数
    io:format("~n验证 map_test_item_to_address 函数:~n"),
    lists:foreach(
        fun({TestItem, ExpectedAddr}) ->
            ActualAddr = map_test_item_to_address(TestItem),
            case ActualAddr of
                ExpectedAddr ->
                    io:format("  ✓ ~ts -> 0x~.4B (正确)~n", [TestItem, ActualAddr]);
                _ ->
                    io:format("  ✗ ~ts -> 0x~.4B (期望: 0x~.4B)~n", 
                             [TestItem, ActualAddr, ExpectedAddr])
            end
        end,
        TestItemMappings
    ),
    
    ok.

%% @doc 运行完整的产品配置检查
check_product_configs() ->
    io:format("=== 产品配置完整性检查 ===~n~n"),
    
    %% 1. 查询所有相关产品
    io:format("1. 查询产品配置...~n"),
    case compare_product_configs() of
        {ok, Results} ->
            io:format("✓ 产品查询成功~n~n"),
            
            %% 2. 验证指令映射
            io:format("2. 验证指令映射...~n"),
            validate_command_mapping(),
            
            %% 3. 检查配置完整性
            io:format("~n3. 配置完整性检查...~n"),
            check_config_completeness(Results),
            
            {ok, Results};
        {error, Reason} ->
            io:format("✗ 产品查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 检查配置完整性
check_config_completeness(Results) ->
    RequiredProducts = [uav, fixture, test_item, workstation],
    
    lists:foreach(
        fun(Type) ->
            case maps:get(Type, Results) of
                {ok, #{name := Name, config := Config}} ->
                    ConfigSize = maps:size(Config),
                    case ConfigSize > 0 of
                        true ->
                            io:format("  ✓ ~s: ~s (配置项: ~p)~n", [Type, Name, ConfigSize]);
                        false ->
                            io:format("  ⚠ ~s: ~s (配置为空)~n", [Type, Name])
                    end;
                {error, Reason} ->
                    io:format("  ✗ ~s: 查询失败 (~p)~n", [Type, Reason]);
                _ ->
                    io:format("  ✗ ~s: 未找到~n", [Type])
            end
        end,
        RequiredProducts
    ),
    
    io:format("~n配置完整性检查完成~n").

%% @doc 导出产品配置到文件（用于前后端比对）
export_product_configs() ->
    io:format("=== 导出产品配置 ===~n"),
    
    case compare_product_configs() of
        {ok, Results} ->
            %% 构建导出数据
            ExportData = maps:map(
                fun(_Type, {ok, Data}) ->
                    Data;
                (Type, {error, Reason}) ->
                    #{type => Type, error => Reason}
                end,
                Results
            ),
            
            %% 生成JSON文件路径
            Timestamp = integer_to_binary(erlang:system_time(second)),
            Filename = "product_configs_" ++ binary_to_list(Timestamp) ++ ".json",
            Filepath = "/tmp/" ++ Filename,
            
            %% 转换为JSON
            JsonData = jsx:encode(ExportData),
            
            %% 写入文件
            case file:write_file(Filepath, JsonData) of
                ok ->
                    io:format("✓ 配置已导出到: ~s~n", [Filepath]),
                    io:format("  文件大小: ~p bytes~n", [byte_size(JsonData)]),
                    {ok, Filepath};
                {error, Reason} ->
                    io:format("✗ 导出失败: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("✗ 查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 查询产品下的设备
query_devices_by_product() ->
    io:format("=== 查询产品下的设备 ===~n~n"),
    
    %% 产品ID列表
    ProductIds = [
        <<"6235befb62">>,  %% 超近距无人机
        <<"bd49cc8272">>,  %% 超近距无人机治具
        <<"343cf21f82">>,  %% 超近距无人机测试项
        <<"2de1b3e1b8">>   %% 超近距无人机工位
    ],
    
    lists:foreach(
        fun(ProductId) ->
            io:format("查询产品 ~s 下的设备...~n", [ProductId]),
            
            %% 构建查询条件
            Where = #{<<"product">> => #{<<"__type">> => <<"Pointer">>, 
                                         <<"className">> => <<"Product">>, 
                                         <<"objectId">> => ProductId}},
            Query = #{<<"where">> => Where, <<"limit">> => 10},
            
            case dgiot_parse:query_object(<<"Device">>, Query) of
                {ok, #{<<"results">> := Results}} ->
                    io:format("  找到 ~p 个设备~n", [length(Results)]),
                    lists:foreach(
                        fun(Device) ->
                            DeviceId = maps:get(<<"objectId">>, Device, <<"未知">>),
                            Name = maps:get(<<"name">>, Device, <<"未知">>),
                            Config = maps:get(<<"config">>, Device, #{}),
                            Content = maps:get(<<"content">>, Device, #{}),
                            
                            %% 解码名称（可能是二进制编码）
                            NameStr = case is_binary(Name) of
                                true -> 
                                    try 
                                        binary_to_list(Name)
                                    catch
                                        _:_ -> "二进制数据"
                                    end;
                                false -> "未知"
                            end,
                            
                            io:format("    - 设备ID: ~s, 名称: ~s~n", [DeviceId, NameStr]),
                            io:format("      配置字段大小: ~p~n", [maps:size(Config)]),
                            io:format("      内容字段大小: ~p~n", [maps:size(Content)]),
                            
                            %% 检查是否有指令相关字段
                            check_for_commands(Device)
                        end,
                        Results
                    );
                {error, Reason} ->
                    io:format("  查询失败: ~p~n", [Reason])
            end,
            io:format("~n")
        end,
        ProductIds
    ),
    
    ok.

%% @doc 检查设备中是否有指令相关字段
check_for_commands(Device) ->
    %% 检查常见的指令相关字段
    CommandFields = [
        <<"commands">>, <<"command">>, <<"instructions">>, <<"instruction">>,
        <<"test_items">>, <<"testItems">>, <<"test_commands">>, <<"testCommands">>,
        <<"workstation_config">>, <<"workstationConfig">>, <<"fixture_config">>, <<"fixtureConfig">>,
        <<"config">>, <<"content">>
    ],
    
    FoundFields = lists:filtermap(
        fun(Field) ->
            case maps:get(Field, Device, undefined) of
                undefined -> false;
                Value -> {true, {Field, Value}}
            end
        end,
        CommandFields
    ),
    
    case FoundFields of
        [] ->
            io:format("      未找到指令相关字段~n");
        Fields ->
            io:format("      找到指令相关字段 (~p 个):~n", [length(Fields)]),
            lists:foreach(
                fun({Field, Value}) ->
                    case is_map(Value) of
                        true ->
                            io:format("        ~s: 大小 ~p~n", [Field, maps:size(Value)]);
                        false when is_list(Value) ->
                            io:format("        ~s: 列表长度 ~p~n", [Field, length(Value)]);
                        _ ->
                            io:format("        ~s: ~p~n", [Field, Value])
                    end
                end,
                Fields
            )
    end.

%% @doc 查询特定设备的配置
query_device_config(DeviceId) ->
    io:format("=== 查询设备配置: ~s ===~n", [DeviceId]),
    
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            Name = maps:get(<<"name">>, Device, <<"未知">>),
            Config = maps:get(<<"config">>, Device, #{}),
            Content = maps:get(<<"content">>, Device, #{}),
            Product = maps:get(<<"product">>, Device, <<"未知">>),
            
            io:format("设备名称: ~p~n", [Name]),
            io:format("所属产品: ~p~n", [Product]),
            io:format("配置字段: ~p~n", [Config]),
            io:format("内容字段: ~p~n", [Content]),
            
            %% 检查其他可能包含配置的字段
            AllKeys = maps:keys(Device),
            io:format("所有字段: ~p~n", [AllKeys]),
            
            {ok, #{name => Name, config => Config, content => Content, product => Product}};
        {error, Reason} ->
            io:format("查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 查找所有工位设备
find_workstation_devices() ->
    io:format("=== 查找工位设备 ===~n"),
    
    %% 查询名称包含"工位"的设备
    Where = #{<<"name">> => #{<<"$regex">> => <<"工位">>}},
    Query = #{<<"where">> => Where, <<"limit">> => 20},
    
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            io:format("找到 ~p 个工位设备~n", [length(Results)]),
            lists:foreach(
                fun(Device) ->
                    DeviceId = maps:get(<<"objectId">>, Device, <<"未知">>),
                    Name = maps:get(<<"name">>, Device, <<"未知">>),
                    Product = maps:get(<<"product">>, Device, <<"未知">>),
                    io:format("  - ~s (ID: ~s, 产品: ~s)~n", [Name, DeviceId, Product])
                end,
                Results
            ),
            {ok, Results};
        {error, Reason} ->
            io:format("查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 查找所有测试项设备
find_test_item_devices() ->
    io:format("=== 查找测试项设备 ===~n"),
    
    %% 查询名称包含"测试"的设备
    Where = #{<<"name">> => #{<<"$regex">> => <<"测试">>}},
    Query = #{<<"where">> => Where, <<"limit">> => 20},
    
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            io:format("找到 ~p 个测试项设备~n", [length(Results)]),
            lists:foreach(
                fun(Device) ->
                    DeviceId = maps:get(<<"objectId">>, Device, <<"未知">>),
                    Name = maps:get(<<"name">>, Device, <<"未知">>),
                    Product = maps:get(<<"product">>, Device, <<"未知">>),
                    io:format("  - ~s (ID: ~s, 产品: ~s)~n", [Name, DeviceId, Product])
                end,
                Results
            ),
            {ok, Results};
        {error, Reason} ->
            io:format("查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 从设备中提取指令集
extract_commands_from_device(DeviceId) ->
    io:format("=== 从设备提取指令集: ~s ===~n", [DeviceId]),
    
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            Name = maps:get(<<"name">>, Device, <<"未知">>),
            Config = maps:get(<<"config">>, Device, #{}),
            Content = maps:get(<<"content">>, Device, #{}),
            
            io:format("设备名称: ~p~n", [Name]),
            
            %% 尝试从不同位置提取指令集
            CommandsFromConfig = maps:get(<<"commands">>, Config, #{}),
            CommandsFromContent = maps:get(<<"commands">>, Content, #{}),
            
            %% 检查其他可能的位置
            AllCommands = maps:merge(CommandsFromConfig, CommandsFromContent),
            
            case maps:size(AllCommands) > 0 of
                true ->
                    io:format("找到指令集 (~p 个):~n", [maps:size(AllCommands)]),
                    maps:foreach(
                        fun(Key, Value) ->
                            io:format("  ~s: ~p~n", [Key, Value])
                        end,
                        AllCommands
                    ),
                    {ok, AllCommands};
                false ->
                    io:format("未找到指令集，检查其他字段...~n"),
                    
                    %% 检查所有字段
                    AllKeys = maps:keys(Device),
                    io:format("设备所有字段: ~p~n", [AllKeys]),
                    
                    %% 特别检查可能包含指令的字段
                    PossibleCommandFields = [
                        <<"instruction">>, <<"instructions">>, <<"command">>, <<"commands">>,
                        <<"test_items">>, <<"testItems">>, <<"test_commands">>, <<"testCommands">>,
                        <<"workstation_config">>, <<"workstationConfig">>, <<"fixture_config">>, <<"fixtureConfig">>
                    ],
                    
                    FoundCommands = lists:foldl(
                        fun(Field, Acc) ->
                            case maps:get(Field, Device, undefined) of
                                undefined -> Acc;
                                Value -> maps:put(Field, Value, Acc)
                            end
                        end,
                        #{},
                        PossibleCommandFields
                    ),
                    
                    case maps:size(FoundCommands) > 0 of
                        true ->
                            io:format("在以下字段中找到可能的指令配置:~n"),
                            maps:foreach(
                                fun(Key, Value) ->
                                    io:format("  ~s: ~p~n", [Key, Value])
                                end,
                                FoundCommands
                            ),
                            {ok, FoundCommands};
                        false ->
                            io:format("未找到任何指令配置~n"),
                            {error, no_commands_found}
                    end
            end;
        {error, Reason} ->
            io:format("查询设备失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 查询产品物模型
query_product_thing(ProductId) ->
    io:format("=== 查询产品物模型: ~s ===~n", [ProductId]),
    
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, Product} ->
            Name = maps:get(<<"name">>, Product, <<"未知">>),
            Thing = maps:get(<<"thing">>, Product, #{}),
            
            io:format("产品名称: ~p~n", [Name]),
            io:format("物模型字段大小: ~p~n", [maps:size(Thing)]),
            
            case maps:size(Thing) > 0 of
                true ->
                    io:format("物模型字段: ~p~n", [maps:keys(Thing)]),
                    
                    %% 检查物模型中的属性、服务、事件
                    Properties = maps:get(<<"properties">>, Thing, []),
                    Services = maps:get(<<"services">>, Thing, []),
                    Events = maps:get(<<"events">>, Thing, []),
                    
                    io:format("属性数量: ~p~n", [length(Properties)]),
                    io:format("服务数量: ~p~n", [length(Services)]),
                    io:format("事件数量: ~p~n", [length(Events)]),
                    
                    %% 检查是否有指令相关定义
                    io:format("~n检查属性中的指令定义（显示前5个）...~n"),
                    lists:foreach(
                        fun(Prop) ->
                            try
                                case is_map(Prop) of
                                    true ->
                                        Identifier = maps:get(<<"identifier">>, Prop, <<"未知">>),
                                        PropName = maps:get(<<"name">>, Prop, <<"未知">>),
                                        io:format("  属性标识: ~p, 名称: ~p~n", [Identifier, PropName]);
                                    false ->
                                        io:format("  属性: ~p~n", [Prop])
                                end
                            catch
                                _:_ ->
                                    io:format("  属性解析错误: ~p~n", [Prop])
                            end
                        end,
                        lists:sublist(Properties, 1, 5)  %% 只显示前5个属性
                    ),
                    
                    %% 检查是否有测试相关的属性
                    TestProperties = lists:filtermap(
                        fun(Prop) ->
                            try
                                case is_map(Prop) of
                                    true ->
                                        PropName = maps:get(<<"name">>, Prop, <<>>),
                                        case binary:match(PropName, <<"测试">>) =/= nomatch orelse
                                             binary:match(PropName, <<"检查">>) =/= nomatch orelse
                                             binary:match(PropName, <<"控制">>) =/= nomatch of
                                            true ->
                                                Identifier = maps:get(<<"identifier">>, Prop, <<"未知">>),
                                                {true, {Identifier, PropName}};
                                            false ->
                                                false
                                        end;
                                    false ->
                                        false
                                end
                            catch
                                _:_ -> false
                            end
                        end,
                        Properties
                    ),
                    
                    io:format("~n找到 ~p 个测试相关属性:~n", [length(TestProperties)]),
                    lists:foreach(
                        fun({Identifier, PropName}) ->
                            io:format("  测试属性: ~p (~p)~n", [Identifier, PropName])
                        end,
                        lists:sublist(TestProperties, 1, 10)  %% 只显示前10个测试属性
                    ),
                    
                    io:format("~n检查服务中的指令定义...~n"),
                    lists:foreach(
                        fun(Service) ->
                            case is_map(Service) of
                                true ->
                                    Identifier = maps:get(<<"identifier">>, Service, <<"未知">>),
                                    Name = maps:get(<<"name">>, Service, <<"未知">>),
                                    InputData = maps:get(<<"inputData">>, Service, []),
                                    io:format("  服务: ~s (~s), 输入参数: ~p~n", [Identifier, Name, InputData]);
                                false ->
                                    io:format("  服务: ~p~n", [Service])
                            end
                        end,
                        Services
                    ),
                    
                    {ok, #{name => Name, thing => Thing, 
                          properties => Properties, services => Services, events => Events}};
                false ->
                    io:format("物模型为空~n"),
                    {ok, #{name => Name, thing => #{}}}
            end;
        {error, Reason} ->
            io:format("查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 查询所有相关Parse表
explore_parse_tables() ->
    io:format("=== 探索Parse表结构 ===~n~n"),
    
    Tables = [
        <<"Product">>, <<"Device">>, <<"TestItem">>, <<"Command">>, 
        <<"Instruction">>, <<"Workstation">>, <<"Fixture">>, <<"Channel">>
    ],
    
    lists:foreach(
        fun(Table) ->
            io:format("查询表 ~s 的结构...~n", [Table]),
            
            %% 查询第一条记录查看结构
            Query = #{<<"limit">> => 1},
            case dgiot_parse:query_object(Table, Query) of
                {ok, #{<<"results">> := [First|_]}} ->
                    io:format("  表 ~s 有数据，字段: ~p~n", [Table, maps:keys(First)]),
                    
                    %% 检查是否有指令相关字段
                    CommandFields = lists:filter(
                        fun(Key) ->
                            binary:match(Key, <<"command">>) =/= nomatch orelse
                            binary:match(Key, <<"instruction">>) =/= nomatch orelse
                            binary:match(Key, <<"test">>) =/= nomatch
                        end,
                        maps:keys(First)
                    ),
                    
                    case CommandFields of
                        [] ->
                            io:format("  未找到指令相关字段~n");
                        Fields ->
                            io:format("  找到指令相关字段: ~p~n", [Fields])
                    end;
                {ok, #{<<"results">> := []}} ->
                    io:format("  表 ~s 为空~n", [Table]);
                {error, Reason} ->
                    io:format("  查询失败: ~p~n", [Reason])
            end,
            io:format("~n")
        end,
        Tables
    ),
    
    ok.

%% @doc 总结指令集存储结构
summarize_command_storage() ->
    io:format("=== 指令集存储结构总结 ===~n~n"),
    
    %% 1. 治具指令集
    io:format("1. 治具指令集 (产品ID: bd49cc8272)~n"),
    io:format("   - 存储位置: 产品 content.command_sets.modbus 字段~n"),
    io:format("   - 管理模块: dgiot_uav_fixture_commands.erl~n"),
    io:format("   - 更新函数: dgiot_uav_fixture_commands:update/0~n"),
    io:format("   - 指令数量: 14个固定指令~n"),
    io:format("   - 指令类型: 电源控制、风管控制、测试命令等~n"),
    io:format("   - 前端加载: loadFixtureCommandsData() -> product.content.command_sets.modbus~n"),
    io:format("   - JSON文件: priv/json/fixture_commands.json~n~n"),
    
    %% 2. 无人机指令集
    io:format("2. 无人机指令集 (产品ID: 6235befb62)~n"),
    io:format("   - 存储位置: 产品 content.remote_commands 字段~n"),
    io:format("   - 管理模块: dgiot_uav_command_examples.erl (示例帧数据)~n"),
    io:format("   - 指令数量: 46个遥控指令示例帧~n"),
    io:format("   - 前端加载: loadUAVRemoteCommandsData() -> product.content.remote_commands~n"),
    io:format("   - JSON文件: priv/json/uav_command_sets.json~n~n"),
    
    %% 3. 工位PLC指令集
    io:format("3. 工位PLC指令集 (产品ID: 2de1b3e1b8)~n"),
    io:format("   - 存储位置: 设备 content.instructions[51] 字段~n"),
    io:format("   - 管理模块: dgiot_uav_plc_commands.erl~n"),
    io:format("   - 更新函数: dgiot_uav_plc_commands:update_all/0~n"),
    io:format("   - 工位数量: 6个工位 (1100, 1200, 1300, 1500, 1600, 1700)~n"),
    io:format("   - 指令结构: instructions[51].meanings 映射表~n"),
    io:format("   - 前端加载: getPLCCommandsForDevice() -> device.content.instructions[51].meanings~n"),
    io:format("   - JSON文件: priv/json/InstructionSet.json~n~n"),
    
    %% 4. 验证指令集存储
    io:format("4. 验证指令集存储状态~n"),
    
    %% 检查治具指令集
    case dgiot_parse:get_object(<<"Product">>, <<"bd49cc8272">>) of
        {ok, FixtureProduct} ->
            FixtureContent = maps:get(<<"content">>, FixtureProduct, #{}),
            FixtureCommandSets = maps:get(<<"command_sets">>, FixtureContent, #{}),
            FixtureModbusCommands = maps:get(<<"modbus">>, FixtureCommandSets, []),
            io:format("   治具指令集: ~p 个指令~n", [length(FixtureModbusCommands)]);
        {error, FixtureReason} ->
            io:format("   治具产品查询失败: ~p~n", [FixtureReason])
    end,
    
    %% 检查无人机指令集
    case dgiot_parse:get_object(<<"Product">>, <<"6235befb62">>) of
        {ok, UAVProduct} ->
            UAVContent = maps:get(<<"content">>, UAVProduct, #{}),
            UAVRemoteCommands = maps:get(<<"remote_commands">>, UAVContent, #{}),
            UAVFlightControl = maps:get(<<"flight_control">>, UAVRemoteCommands, []),
            UAVPayloadControl = maps:get(<<"payload_control">>, UAVRemoteCommands, []),
            io:format("   无人机指令集: 飞行控制 ~p 个, 载荷控制 ~p 个~n", 
                     [length(UAVFlightControl), length(UAVPayloadControl)]);
        {error, UAVReason} ->
            io:format("   无人机产品查询失败: ~p~n", [UAVReason])
    end,
    
    %% 检查工位设备指令集
    Where = #{
        <<"product">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Product">>, <<"objectId">> => <<"2de1b3e1b8">>},
        <<"devaddr">> => #{<<"$regex">> => <<"^D">>}
    },
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => Where, <<"limit">> => 10}) of
        {ok, #{<<"results">> := Stations}} ->
            StationsWithCommands = lists:filter(
                fun(Station) ->
                    StationContent = maps:get(<<"content">>, Station, #{}),
                    StationInstructions = maps:get(<<"instructions">>, StationContent, #{}),
                    maps:is_key(51, StationInstructions)
                end,
                Stations
            ),
            io:format("   工位设备: 共 ~p 个, 其中 ~p 个有PLC指令集~n", 
                     [length(Stations), length(StationsWithCommands)]);
        {error, StationReason} ->
            io:format("   工位设备查询失败: ~p~n", [StationReason])
    end,
    
    io:format("~n=== 总结完成 ===~n").

%% @doc 验证指令集完整性
validate_command_sets() ->
    io:format("=== 验证指令集完整性 ===~n~n"),
    
    %% 1. 验证治具指令集
    io:format("1. 验证治具指令集...~n"),
    case dgiot_uav_fixture_commands:get_commands() of
        FixtureCommands when is_list(FixtureCommands) ->
            io:format("   治具指令数量: ~p~n", [length(FixtureCommands)]),
            lists:foreach(
                fun(#{<<"code">> := Code, <<"name">> := Name}) ->
                    io:format("     ~p: ~s~n", [Code, Name])
                end,
                FixtureCommands
            );
        _ ->
            io:format("   治具指令集获取失败~n")
    end,
    io:format("~n"),
    
    %% 2. 验证PLC工位指令集
    io:format("2. 验证PLC工位指令集...~n"),
    StationIds = [1100, 1200, 1300, 1500, 1600, 1700],
    lists:foreach(
        fun(StationId) ->
            PLCCommands = dgiot_uav_plc_commands:get_station_commands(StationId),
            io:format("   工位 ~p: ~p 个指令~n", [StationId, length(PLCCommands)])
        end,
        StationIds
    ),
    io:format("~n"),
    
    %% 3. 验证无人机指令示例
    io:format("3. 验证无人机指令示例...~n"),
    TestCodes = [1, 2, 3, 4, 5, 10, 20, 30, 40, 46],
    ValidCount = lists:foldl(
        fun(Code, Acc) ->
            case dgiot_uav_command_examples:get_command_example(Code) of
                <<>> -> Acc;
                _ -> Acc + 1
            end
        end,
        0,
        TestCodes
    ),
    io:format("   测试 ~p 个指令码, 其中 ~p 个有示例数据~n", [length(TestCodes), ValidCount]),
    io:format("~n=== 验证完成 ===~n").