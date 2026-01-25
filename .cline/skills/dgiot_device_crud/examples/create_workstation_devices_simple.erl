%%%-------------------------------------------------------------------
%%% @doc DGIOT设备增删查改技巧示例 - 创建工位设备（简化版）
%%% 演示如何使用最佳实践创建设备，包含治具定位地址查询关联
%%% @end
%%%-------------------------------------------------------------------
-module(create_workstation_devices_simple).
-author("DGIOT Team").
-export([
    create_all/0, 
    run_example/0,
    query_fixture_workstation_info/0,
    merge_workstation_info/1
]).

%% 简化日志记录
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
            
            %% 步骤3: 模拟创建设备
            simulate_create_devices(Workstations);
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
            
            %% 模拟创建设备
            simulate_create_devices(Workstations)
    end.

%% @doc 模拟创建设备
simulate_create_devices(Workstations) ->
    ProductId = <<"2de1b3e1b8">>,
    
    Results = lists:map(
        fun({DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC}) ->
            simulate_create_workstation(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC)
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

%% @doc 模拟创建单个工位设备
simulate_create_workstation(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc, HasPLC) ->
    %% 模拟设备ID生成
    DeviceId = <<ProductId/binary, "_", DeviceAddr/binary>>,
    
    %% 模拟检查设备是否已存在
    case random:uniform(10) > 2 of  %% 80%概率设备不存在
        true ->
            %% 模拟创建设备属性
            DeviceProps = #{
                <<"devaddr">> => DeviceAddr,
                <<"productId">> => ProductId,
                <<"deviceType">> => DeviceType,
                <<"name">> => DeviceName,
                <<"status">> => <<"offline">>,
                <<"ip">> => IP,
                <<"description">> => Desc,
                <<"has_plc">> => HasPLC,
                <<"createdAt">> => erlang:system_time(second)
            },
            
            %% 模拟数据验证
            case simulate_validate_device_data(DeviceProps) of
                {ok, ValidatedProps} ->
                    %% 模拟创建设备成功
                    ?LOG(debug, "模拟创建设备: ~s", [DeviceName]),
                    {ok, DeviceId};
                {error, Reason} ->
                    ?LOG(error, "数据验证失败: ~s - 原因: ~p", [DeviceName, Reason]),
                    {error, Reason}
            end;
        false ->
            %% 模拟设备已存在
            ?LOG(info, "设备已存在: ~s (~s)", [DeviceName, DeviceAddr]),
            {ok, DeviceId}
    end.

%% @doc 模拟数据验证
simulate_validate_device_data(Device) ->
    RequiredFields = [<<"productId">>, <<"devaddr">>, <<"name">>, <<"deviceType">>],
    
    case simulate_validate_required_fields(Device, RequiredFields) of
        ok ->
            %% 验证IP地址格式
            case simulate_validate_ip_address(maps:get(<<"ip">>, Device, <<>>)) of
                true ->
                    {ok, Device};
                false ->
                    {error, invalid_ip_address}
            end;
        {error, MissingField} ->
            {error, {missing_field, MissingField}}
    end.

%% @doc 模拟验证必需字段
simulate_validate_required_fields(Device, [Field | Rest]) ->
    case maps:is_key(Field, Device) of
        true -> 
            case maps:get(Field, Device) of
                <<>> -> {error, {empty_field, Field}};
                _ -> simulate_validate_required_fields(Device, Rest)
            end;
        false -> {error, {missing_field, Field}}
    end;
simulate_validate_required_fields(_Device, []) ->
    ok.

%% @doc 模拟验证IP地址格式
simulate_validate_ip_address(IP) when is_binary(IP) ->
    case re:run(IP, "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$") of
        {match, _} -> true;
        _ -> false
    end;
simulate_validate_ip_address(_) -> false.

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
    ?LOG(info, "运行DGIOT设备增删查改技巧示例（包含治具定位地址查询关联）..."),
    
    %% 1. 创建所有工位设备
    case create_all() of
        {ok, #{success := Success, error := Error}} ->
            ?LOG(info, "设备创建完成: 成功 ~p 个, 失败 ~p 个", [Success, Error]),
            
            %% 2. 显示治具查询结果
            ?LOG(info, "=== 治具定位地址查询关联演示 ==="),
            
            %% 3. 演示治具查询功能
            case query_fixture_workstation_info() of
                {ok, FixtureWorkstations} ->
                    ?LOG(info, "治具查询结果:"),
                    lists:foreach(
                        fun({DeviceType, DeviceAddr, Name, IP, _Desc, HasPLC}) ->
                            ?LOG(info, "  - ~s (~s): IP=~s, PLC=~s", 
                                [Name, DeviceAddr, IP, 
                                 case HasPLC of true -> "有"; false -> "无" end])
                        end,
                        FixtureWorkstations
                    );
                {error, Reason} ->
                    ?LOG(error, "治具查询失败: ~p", [Reason])
            end,
            
            %% 4. 演示合并功能
            case query_fixture_workstation_info() of
                {ok, FixtureWorkstations2} ->
                    Merged = merge_workstation_info(FixtureWorkstations2),
                    ?LOG(info, "=== 合并后工位信息 ==="),
                    ?LOG(info, "共 ~p 个工位:", [length(Merged)]),
                    lists:foreach(
                        fun({DeviceType, DeviceAddr, Name, IP, Desc, HasPLC}) ->
                            Source = case lists:member({DeviceType, DeviceAddr, Name, IP, Desc, HasPLC}, FixtureWorkstations2) of
                                true -> "治具查询";
                                false -> "预设信息"
                            end,
                            ?LOG(info, "  - ~s (~s): ~s [来源: ~s]", 
                                [Name, DeviceAddr, Desc, Source])
                        end,
                        Merged
                    );
                {error, Reason2} ->
                    ?LOG(error, "无法演示合并功能: ~p", [Reason2])
            end,
            
            ok;
        {error, CreateReason} ->
            ?LOG(error, "设备创建失败: ~p", [CreateReason]),
            {error, CreateReason}
    end.
