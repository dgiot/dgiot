%%%-------------------------------------------------------------------
%%% @doc DGIOT设备增删查改技巧示例 - 创建工位设备
%%% 演示如何使用最佳实践创建设备
%%% @end
%%%-------------------------------------------------------------------
-module(create_workstation_devices).
-author("DGIOT Team").
-export([
    create_all/0, 
    create_workstation/6, 
    batch_create/1, 
    monitor_status/1,
    query_fixture_workstation_info/0,
    merge_workstation_info/1,
    run_example/0
]).

%% 由于这是一个示例文件，我们简化日志记录
-define(LOG(Level, Format), io:format("[~s] " ++ Format ++ "~n", [Level])).
-define(LOG(Level, Format, Args), io:format("[~s] " ++ Format ++ "~n", [Level | Args])).

%% @doc 创建所有工位设备
create_all() ->
    ?LOG(info, "开始创建超近距无人机工位设备"),
    
    %% 产品ID: 2de1b3e1b8
    ProductId = <<"2de1b3e1b8">>,
    
    %% 步骤1: 查询治具的定位地址信息
    ?LOG(info, "步骤1: 查询治具的定位地址信息..."),
    case query_fixture_workstation_info() of
        {ok, FixtureWorkstations} ->
            ?LOG(info, "治具查询成功，获取到 ~p 个工位信息", [length(FixtureWorkstations)]),
            
            %% 步骤2: 合并治具查询结果和预设工位信息
            Workstations = merge_workstation_info(FixtureWorkstations),
            ?LOG(info, "合并后共有 ~p 个工位设备需要创建", [length(Workstations)]),
            
            %% 步骤3: 批量创建设备
            batch_create(Workstations);
        {error, Reason} ->
            ?LOG(warning, "治具查询失败: ~p，使用预设工位信息", [Reason]),
            
            %% 使用预设工位设备列表
            Workstations = [
                %% {设备类型, 设备地址, 设备名称, IP地址, 描述, 是否有PLC}
                {<<"uav_workstation_gantry">>, <<"D1100">>, <<"桁行架工位">>, <<"192.168.100.40">>, <<"桁行架工位 - 负责无人机桁行架测试">>, true},
                {<<"uav_workstation_kaoji1">>, <<"D1200">>, <<"拷机1工位">>, <<"192.168.100.40">>, <<"拷机1工位 - 负责无人机拷机测试1">>, true},
                {<<"uav_workstation_kaoji2">>, <<"D1300">>, <<"拷机2工位">>, <<"192.168.100.40">>, <<"拷机2工位 - 负责无人机拷机测试2">>, true},
                {<<"uav_workstation_digui_rgv">>, <<"D1400">>, <<"地柜RGV工位">>, <<"192.168.100.40">>, <<"地柜RGV工位 - 负责地柜RGV测试">>, true},
                {<<"uav_workstation_zongce1">>, <<"D1500">>, <<"总测1工位">>, <<"192.168.100.40">>, <<"总测1工位 - 负责无人机总测试1">>, true},
                {<<"uav_workstation_zongce2">>, <<"D1600">>, <<"总测2工位">>, <<"192.168.100.40">>, <<"总测2工位 - 负责无人机总测试2">>, true},
                {<<"uav_workstation_cihangxiang">>, <<"D1700">>, <<"磁航向工位">>, <<"192.168.100.20">>, <<"磁航向工位 - 负责无人机磁航向测试">>, true},
                {<<"uav_workstation_feeding_table">>, <<"FEEDING_TABLE">>, <<"上料台">>, <<"192.168.100.40">>, <<"上料台 - 负责无人机上料">>, false}
            ],
            
            %% 批量创建设备
            batch_create(Workstations)
    end.

%% @doc 批量创建设备
batch_create(Workstations) ->
    ProductId = <<"2de1b3e1b8">>,
    
    Results = lists:map(
        fun({DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC}) ->
            create_workstation(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC)
        end,
        Workstations
    ),
    
    %% 统计结果
    {SuccessCount, ErrorCount} = lists:foldl(
        fun
            ({ok, _}, {S, E}) -> {S + 1, E};
            ({error, _}, {S, E}) -> {S, E + 1}
        end,
        {0, 0},
        Results
    ),
    
    ?LOG(info, "工位设备创建完成: 成功 ~p 个, 失败 ~p 个", [SuccessCount, ErrorCount]),
    
    %% 打印详细结果
    lists:zipwith(fun({DeviceType, DeviceAddr, DeviceName, _, _, _}, Result) ->
            case Result of
                {ok, DeviceId} ->
                    ?LOG(info, "✓ 创建成功: ~s (~s) - ID: ~s", [DeviceName, DeviceAddr, DeviceId]);
                {error, Reason} ->
                    ?LOG(error, "✗ 创建失败: ~s (~s) - 原因: ~p", [DeviceName, DeviceAddr, Reason])
            end
        end, Workstations, Results),
    
    {ok, #{success => SuccessCount, error => ErrorCount, results => Results}}.

%% @doc 创建单个工位设备 - 演示最佳实践
create_workstation(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc) ->
    create_workstation(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc, true).

create_workstation(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC) ->
    %% 技巧1: 使用标准方式生成设备ID
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    
    %% 技巧2: 先检查设备是否已存在
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"status">> := Status}} ->
            ?LOG(info, "设备已存在: ~s (~s), 当前状态: ~s", [DeviceName, DeviceAddr, Status]),
            
            %% 技巧3: 更新设备状态为在线
            case dgiot_device:online(DeviceId) of
                ok ->
                    ?LOG(info, "设备状态更新为在线: ~s", [DeviceId]);
                pass ->
                    ?LOG(debug, "设备状态未变化: ~s", [DeviceId])
            end,
            
            {ok, DeviceId};
        _ ->
            %% 技巧4: 创建设备属性 - 完整数据结构
            DeviceProps = create_device_props(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC),
            
            %% 技巧5: 验证设备数据
            case validate_device_data(DeviceProps) of
                {ok, ValidatedProps} ->
                    %% 技巧6: 创建设备
                    case dgiot_device:create_device(ValidatedProps) of
                        {ok, #{<<"objectId">> := CreatedId}} ->
                            ?LOG(info, "创建成功: ~s (~s) - ID: ~s", [DeviceName, DeviceAddr, CreatedId]),
                            
                            %% 技巧7: 创建设备影子（用于存储实时数据）
                            create_device_shadow(CreatedId, DeviceType, HasPLC),
                            
                            %% 技巧8: 设置设备为在线状态
                            dgiot_device:online(CreatedId),
                            
                            {ok, CreatedId};
                        {error, Reason} ->
                            ?LOG(error, "创建失败: ~s (~s) - 原因: ~p", [DeviceName, DeviceAddr, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    ?LOG(error, "数据验证失败: ~s (~s) - 原因: ~p", [DeviceName, DeviceAddr, Reason]),
                    {error, Reason}
            end
    end.

%% @doc 创建设备属性 - 演示完整数据结构
create_device_props(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC) ->
    #{
        <<"devaddr">> => DeviceAddr,
        <<"productId">> => ProductId,
        <<"deviceType">> => DeviceType,
        <<"name">> => DeviceName,
        <<"status">> => <<"offline">>,  %% 初始状态为离线
        <<"ip">> => IP,
        <<"port">> => 0,
        <<"description">> => Desc,
        <<"is_virtual">> => true,  %% 虚拟设备，等待实际连接
        <<"createdAt">> => dgiot_datetime:now_secs(),
        <<"updatedAt">> => dgiot_datetime:now_secs(),
        <<"location">> => #{
            <<"workstation">> => DeviceAddr,
            <<"production_line">> => <<"超近距无人机生产线">>,
            <<"factory">> => <<"无人机工厂">>
        },
        <<"attributes">> => #{
            <<"workstation_type">> => DeviceType,
            <<"workstation_addr">> => DeviceAddr,
            <<"plc_ip">> => IP,
            <<"has_plc">> => HasPLC,
            <<"test_capabilities">> => get_test_capabilities(DeviceType)
        }
    }.

%% @doc 验证设备数据 - 演示数据验证技巧
validate_device_data(Device) ->
    RequiredFields = [<<"productId">>, <<"devaddr">>, <<"name">>, <<"deviceType">>],
    
    case validate_required_fields(Device, RequiredFields) of
        ok ->
            %% 验证IP地址格式
            case validate_ip_address(maps:get(<<"ip">>, Device, <<>>)) of
                true ->
                    {ok, Device};
                false ->
                    {error, invalid_ip_address}
            end;
        {error, MissingField} ->
            {error, {missing_field, MissingField}}
    end.

%% @doc 验证必需字段
validate_required_fields(Device, [Field | Rest]) ->
    case maps:is_key(Field, Device) of
        true -> 
            case maps:get(Field, Device) of
                <<>> -> {error, {empty_field, Field}};
                _ -> validate_required_fields(Device, Rest)
            end;
        false -> {error, {missing_field, Field}}
    end;
validate_required_fields(_Device, []) ->
    ok.

%% @doc 验证IP地址格式
validate_ip_address(IP) when is_binary(IP) ->
    case re:run(IP, "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$") of
        {match, _} -> true;
        _ -> false
    end;
validate_ip_address(_) -> false.

%% @doc 获取工位测试能力
get_test_capabilities(DeviceType) ->
    case DeviceType of
        <<"uav_workstation_gantry">> ->
            [<<"桁行架测试">>, <<"取料测试">>, <<"放料测试">>];
        <<"uav_workstation_kaoji1">> ->
            [<<"拷机测试">>, <<"温度测试">>, <<"振动测试">>];
        <<"uav_workstation_kaoji2">> ->
            [<<"拷机测试">>, <<"湿度测试">>, <<"压力测试">>];
        <<"uav_workstation_digui_rgv">> ->
            [<<"地柜测试">>, <<"RGV移动测试">>, <<"定位测试">>];
        <<"uav_workstation_zongce1">> ->
            [<<"总测试">>, <<"电气测试">>, <<"通信测试">>];
        <<"uav_workstation_zongce2">> ->
            [<<"总测试">>, <<"动力测试">>, <<"性能测试">>];
        <<"uav_workstation_cihangxiang">> ->
            [<<"磁航向测试">>, <<"校准测试">>, <<"精度测试">>];
        <<"uav_workstation_feeding_table">> ->
            [<<"上料测试">>, <<"定位测试">>, <<"安全测试">>];
        _ ->
            [<<"通用测试">>]
    end.

%% @doc 创建设备影子 - 演示设备数据管理技巧
create_device_shadow(DeviceId, DeviceType, HasPLC) ->
    ShadowData = #{
        <<"device_id">> => DeviceId,
        <<"device_type">> => DeviceType,
        <<"status">> => <<"initialized">>,
        <<"last_heartbeat">> => dgiot_datetime:now_secs(),
        <<"test_results">> => #{},
        <<"modbus_registers">> => get_default_modbus_registers(DeviceType),
        <<"plc_connection">> => #{
            <<"status">> => case HasPLC of
                true -> <<"disconnected">>;
                false -> <<"not_available">>
            end,
            <<"last_connect_time">> => 0,
            <<"error_count">> => 0
        }
    },
    
    %% 使用dgiot_device:post保存设备影子数据
    dgiot_device:post(ShadowData),
    ok.

%% @doc 获取默认Modbus寄存器配置
get_default_modbus_registers(DeviceType) ->
    case DeviceType of
        <<"uav_workstation_gantry">> ->
            #{
                <<"D1100">> => #{<<"address">> => 16#044C, <<"value">> => 0, <<"description">> => <<"等待命令寄存器">>},
                <<"D1150">> => #{<<"address">> => 16#047E, <<"value">> => 0, <<"description">> => <<"发送指令区">>},
                <<"D1151">> => #{<<"address">> => 16#047F, <<"value">> => 0, <<"description">> => <<"动作指令">>}
            };
        <<"uav_workstation_zongce2">> ->
            #{
                <<"D1600">> => #{<<"address">> => 16#06E7, <<"value">> => 0, <<"description">> => <<"总测2工位寄存器1">>},
                <<"D1601">> => #{<<"address">> => 16#06E8, <<"value">> => 0, <<"description">> => <<"总测2工位寄存器2">>}
            };
        <<"uav_workstation_cihangxiang">> ->
            #{
                <<"D1700">> => #{<<"address">> => 16#06E9, <<"value">> => 0, <<"description">> => <<"磁航向工位寄存器">>}
            };
        _ ->
            #{
                <<"base">> => #{<<"address">> => 16#0000, <<"value">> => 0, <<"description">> => <<"基础寄存器">>}
            }
    end.

%% @doc 监控设备状态 - 演示设备状态管理技巧
monitor_status(DeviceId) ->
    %% 技巧: 使用缓存查询提高性能
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"updatedAt">> := UpdatedAt, <<"name">> := Name}} ->
            %% 检查是否超时
            Now = dgiot_datetime:now_secs(),
            TimeDiff = Now - UpdatedAt,
            
            if
                TimeDiff > 300 ->  %% 5分钟无更新
                    ?LOG(warning, "设备 ~s (~s) 超时, 最后更新: ~p 秒前", [Name, DeviceId, TimeDiff]),
                    dgiot_device:offline(DeviceId),
                    {offline, timeout};
                Status == <<"online">> ->
                    ?LOG(debug, "设备 ~s (~s) 在线, 最后更新: ~p 秒前", [Name, DeviceId, TimeDiff]),
                    {online, normal};
                true ->
                    ?LOG(info, "设备 ~s (~s) 状态: ~s, 最后更新: ~p 秒前", [Name, DeviceId, Status, TimeDiff]),
                    {Status, normal}
            end;
        {error, Reason} ->
            ?LOG(error, "查询设备状态失败: ~s - 原因: ~p", [DeviceId, Reason]),
            {error, Reason}
    end.

%% @doc 查询治具的工位信息
query_fixture_workstation_info() ->
    ?LOG(info, "开始查询治具工位信息..."),
    
    %% 模拟治具查询 - 在实际应用中，这里会通过Modbus/TCP等方式查询治具
    %% 这里使用模拟数据来演示功能
    
    try
        %% 模拟查询治具的工位代码
        %% 在实际应用中，这里会调用 dgiot_uav_fixture_protocol:read_workstation_info/0
        SimulatedFixtureData = simulate_fixture_query(),
        
        %% 解析治具返回的工位信息
        FixtureWorkstations = parse_fixture_workstation_data(SimulatedFixtureData),
        
        ?LOG(info, "治具查询完成，解析到 ~p 个工位", [length(FixtureWorkstations)]),
        {ok, FixtureWorkstations}
    catch
        Class:Reason:Stack ->
            ?LOG(error, "治具查询异常: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%% @doc 模拟治具查询
simulate_fixture_query() ->
    %% 模拟治具返回的工位代码数据
    %% 在实际应用中，这里会返回真实的Modbus响应数据
    [
        #{code => 1, name => <<"总测2">>, ip => <<"192.168.100.40">>, has_plc => true},
        #{code => 2, name => <<"总测2-动力">>, ip => <<"192.168.100.40">>, has_plc => true},
        #{code => 5, name => <<"桁行架">>, ip => <<"192.168.100.40">>, has_plc => true},
        #{code => 6, name => <<"拷机2">>, ip => <<"192.168.100.40">>, has_plc => true},
        #{code => 7, name => <<"拷机1">>, ip => <<"192.168.100.40">>, has_plc => true},
        #{code => 16#FF, name => <<"上料台">>, ip => <<"">>, has_plc => false}
    ].

%% @doc 解析治具工位数据
parse_fixture_workstation_data(FixtureData) ->
    lists:map(
        fun(#{code := Code, name := Name, ip := IP, has_plc := HasPLC}) ->
            %% 将治具工位代码映射到设备类型和地址
            {DeviceType, DeviceAddr} = map_fixture_code_to_device(Code, Name, HasPLC),
            {DeviceType, DeviceAddr, Name, IP, <<"治具查询工位">>, HasPLC}
        end,
        FixtureData
    ).

%% @doc 将治具工位代码映射到设备类型和地址
map_fixture_code_to_device(Code, Name, HasPLC) ->
    case Code of
        1 -> {<<"uav_workstation_zongce2">>, <<"D1600">>};
        2 -> {<<"uav_workstation_zongce2">>, <<"D1601">>};  %% 总测2-动力
        5 -> {<<"uav_workstation_gantry">>, <<"D1100">>};
        6 -> {<<"uav_workstation_kaoji2">>, <<"D1300">>};
        7 -> {<<"uav_workstation_kaoji1">>, <<"D1200">>};
        16#FF -> {<<"uav_workstation_feeding_table">>, <<"FEEDING_TABLE">>};
        _ ->
            %% 未知工位，使用默认映射
            DefaultAddr = <<"D", (integer_to_binary(1000 + Code))/binary>>,
            {<<"uav_workstation_unknown">>, DefaultAddr}
    end.

%% @doc 合并治具查询结果和预设工位信息
merge_workstation_info(FixtureWorkstations) ->
    %% 预设工位信息
    PresetWorkstations = [
        {<<"uav_workstation_gantry">>, <<"D1100">>, <<"桁行架工位">>, <<"192.168.100.40">>, <<"桁行架工位 - 负责无人机桁行架测试">>, true},
        {<<"uav_workstation_kaoji1">>, <<"D1200">>, <<"拷机1工位">>, <<"192.168.100.40">>, <<"拷机1工位 - 负责无人机拷机测试1">>, true},
        {<<"uav_workstation_kaoji2">>, <<"D1300">>, <<"拷机2工位">>, <<"192.168.100.40">>, <<"拷机2工位 - 负责无人机拷机测试2">>, true},
        {<<"uav_workstation_digui_rgv">>, <<"D1400">>, <<"地柜RGV工位">>, <<"192.168.100.40">>, <<"地柜RGV工位 - 负责地柜RGV测试">>, true},
        {<<"uav_workstation_zongce1">>, <<"D1500">>, <<"总测1工位">>, <<"192.168.100.40">>, <<"总测1工位 - 负责无人机总测试1">>, true},
        {<<"uav_workstation_zongce2">>, <<"D1600">>, <<"总测2工位">>, <<"192.168.100.40">>, <<"总测2工位 - 负责无人机总测试2">>, true},
        {<<"uav_workstation_cihangxiang">>, <<"D1700">>, <<"磁航向工位">>, <<"192.168.100.20">>, <<"磁航向工位 - 负责无人机磁航向测试">>, true},
        {<<"uav_workstation_feeding_table">>, <<"FEEDING_TABLE">>, <<"上料台">>, <<"192.168.100.40">>, <<"上料台 - 负责无人机上料">>, false}
    ],
    
    %% 创建设备地址到工位信息的映射
    FixtureMap = lists:foldl(
        fun({DeviceType, DeviceAddr, Name, IP, Desc, HasPLC}, Acc) ->
            maps:put(DeviceAddr, {DeviceType, DeviceAddr, Name, IP, Desc, HasPLC}, Acc)
        end,
        #{},
        FixtureWorkstations
    ),
    
    %% 合并策略：优先使用治具查询结果，缺失的使用预设信息
    Merged = lists:map(
        fun({PresetType, PresetAddr, PresetName, PresetIP, PresetDesc, PresetHasPLC}) ->
            case maps:get(PresetAddr, FixtureMap, undefined) of
                undefined ->
                    %% 治具中没有该工位，使用预设信息
                    {PresetType, PresetAddr, PresetName, PresetIP, PresetDesc, PresetHasPLC};
                {FixtureType, FixtureAddr, FixtureName, FixtureIP, FixtureDesc, FixtureHasPLC} ->
                    %% 治具中有该工位，使用治具信息（但保留预设的设备类型）
                    %% 注意：设备类型使用预设的，因为治具可能返回不同的类型
                    {PresetType, FixtureAddr, FixtureName, FixtureIP, FixtureDesc, FixtureHasPLC}
            end
        end,
        PresetWorkstations
    ),
    
    %% 添加治具中有但预设中没有的工位
    AdditionalWorkstations = lists:filter(
        fun({_Type, Addr, _Name, _IP, _Desc, _HasPLC}) ->
            %% 检查这个地址是否已经在预设中
            not lists:any(fun({_, PresetAddr, _, _, _, _}) -> PresetAddr == Addr end, PresetWorkstations)
        end,
        FixtureWorkstations
    ),
    
    Merged ++ AdditionalWorkstations.

%% @doc 在Erlang shell中运行示例
run_example() ->
    ?LOG(info, "运行DGIOT设备增删查改技巧示例..."),
    
    %% 1. 创建所有工位设备
    case create_all() of
        {ok, #{success := Success, error := Error}} ->
            ?LOG(info, "设备创建完成: 成功 ~p 个, 失败 ~p 个", [Success, Error]),
            
            %% 2. 监控设备状态
            ProductId = <<"2de1b3e1b8">>,
            DeviceAddr = <<"D1600">>,  %% 总测2工位
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
            
            case monitor_status(DeviceId) of
                {Status, MonitorReason} ->
                    ?LOG(info, "设备状态监控结果: 状态=~s, 原因=~p", [Status, MonitorReason]);
                {error, MonitorError} ->
                    ?LOG(error, "状态监控失败: ~p", [MonitorError])
            end,
            ok;
        {error, CreateReason} ->
            ?LOG(error, "设备创建失败: ~p", [CreateReason]),
            {error, CreateReason}
    end.
