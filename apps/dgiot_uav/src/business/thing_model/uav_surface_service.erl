%%%-------------------------------------------------------------------
%%% @doc
%%% uav_surface_service - 无人机业务层处理（舵面数据等）
%%% 实现双重存储：
%%%   1. 舵面数据存入自身物模型（必须）
%%%   2. 若关联到无人机，则将舵面数据（带位置前缀）直接发送 aggregate 给无人机进程
%%% 增强：添加白名单过滤，仅保留合法的舵面测量字段
%%% @end
%%%-------------------------------------------------------------------
-module(uav_surface_service).

-export([
    handle_surface_data/3,
    test/0
]).

-include_lib("dgiot/include/logger.hrl").

%% 舵面设备ID到位置缩写的映射（与端口映射一致）
-define(SURFACE_POSITION_MAP, #{
    <<"wrj_dm_zqy">> => <<"zqy">>,
    <<"wrj_dm_yqy">> => <<"yqy">>,
    <<"wrj_dm_zcw">> => <<"zcw">>,
    <<"wrj_dm_ycw">> => <<"ycw">>,
    <<"wrj_dm_zhj">> => <<"zhj">>
}).

%% 合法的舵面测量字段（原始字段名，无前缀）
-define(LEGAL_SURFACE_FIELDS, [
    <<"acceleration_x">>,
    <<"acceleration_y">>,
    <<"acceleration_z">>,
    <<"angular_x">>,
    <<"angular_y">>,
    <<"angular_z">>,
    <<"roll">>,
    <<"pitch">>,
    <<"yaw">>,
    <<"temperature">>
]).

%% @doc 处理舵面传感器原始 Modbus 数据
-spec handle_surface_data(binary(), binary(), binary()) -> ok.
handle_surface_data(ProductId, DevAddr, RawData) ->
    % 1. 解析原始数据为物理量
    Parsed = parse_surface_data(RawData),

    % 2. 存储到舵面自身物模型（必须存储）
    try
        uav_thing_model:save_thing_model_data(ProductId, DevAddr, Parsed)
    catch
        _:Error:StackTrace ->
            ?LOG(error, "[SURFACE] 存储物模型失败: Error=~p", [Error]),
            ?LOG(debug, "[SURFACE] StackTrace=~p", [StackTrace])
    end,

    % 3. 直接发送 aggregate 给无人机进程（通过 IP+10007）
    ?LOG(debug, "[SURFACE] 调用 extract_ip_and_device_type: DevAddr=~s", [DevAddr]),
    case extract_ip_and_device_type(DevAddr) of
        {ok, IpBin, DeviceTypeId} ->
            ?LOG(debug, "[SURFACE] extract_ip_and_device_type 成功: IpBin=~s, DeviceTypeId=~s", [IpBin, DeviceTypeId]),
            case maps:find(DeviceTypeId, ?SURFACE_POSITION_MAP) of
                {ok, PositionPrefix} ->
                    PrefixedData = add_position_prefix(PositionPrefix, Parsed),
                    FilteredData = filter_legal_surface_fields(PositionPrefix, PrefixedData),

                    % 尝试通过 IP + 10007 端口发送 aggregate（简化链路）
                    case dgiot_uav_business_service:get_pid_by_ip_port(IpBin, 10007) of
                        {ok, DronePid} ->
                            DronePid ! {aggregate, FilteredData};
                        {error, _Reason} ->
                            ok
                    end,
                    
                    % 4. 同时调用聚合器进行数据汇聚
                    Timestamp = erlang:system_time(millisecond),
                    case whereis(dgiot_uav_aggregator) of
                        undefined ->
                            ?LOG(warning, "[SURFACE] 聚合器未启动，尝试启动"),
                            dgiot_uav_aggregator:start_link(),
                            timer:sleep(100);
                        _ -> ok
                    end,
                    ?LOG(debug, "[SURFACE] 准备调用聚合器: DevAddr=~s, FilteredData keys=~p", [DevAddr, maps:keys(FilteredData)]),
                    dgiot_uav_aggregator:aggregate(DevAddr, ProductId, FilteredData, Timestamp),
                    ?LOG(info, "[SURFACE] 舵面数据已提交到聚合器: DevAddr=~s, 字段数=~p", [DevAddr, maps:size(FilteredData)]);
                error ->
                    ?LOG(warning, "[SURFACE] 舵面设备 ID ~s 不在位置映射表中", [DeviceTypeId])
            end;
        {error, Reason} ->
            ?LOG(warning, "[SURFACE] 从 DevAddr ~s 提取 IP/设备类型失败: ~p", [DevAddr, Reason])
    end,
    ok.

%% 内部解析函数
parse_surface_data(<<_SlaveId:8, 3:8, _ByteCount:8, Payload:96/binary, _Crc:16>>) ->
    Regs = [ (H bsl 8) bor L || <<H:8, L:8>> <= Payload ],
    #{
        <<"acceleration_x">> => reg_to_float(Regs, 16#34, 32768, 16),
        <<"acceleration_y">> => reg_to_float(Regs, 16#35, 32768, 16),
        <<"acceleration_z">> => reg_to_float(Regs, 16#36, 32768, 16),
        <<"angular_x">> => reg_to_float(Regs, 16#37, 32768, 2000),
        <<"angular_y">> => reg_to_float(Regs, 16#38, 32768, 2000),
        <<"angular_z">> => reg_to_float(Regs, 16#39, 32768, 2000),
        <<"roll">> => reg_to_float(Regs, 16#3D, 32768, 180),
        <<"pitch">> => reg_to_float(Regs, 16#3E, 32768, 180),
        <<"yaw">> => reg_to_float(Regs, 16#3F, 32768, 180),
        <<"temperature">> => reg_to_float(Regs, 16#40, 100, 1)
    };
parse_surface_data(_) ->
    #{}.

reg_to_float(Regs, Addr, Scale, Factor) ->
    Index = Addr - 16#34 + 1,
    case Index >= 1 andalso Index =< length(Regs) of
        true ->
            Val = lists:nth(Index, Regs),
            Signed = if Val > 32767 -> Val - 65536; true -> Val end,
            Signed / Scale * Factor;
        false ->
            0.0
    end.

%% 从 DevAddr 中提取 IP 和设备类型ID（格式如 "192.168.100.45_10001_wrj_dm_zqy"）
%% 注意：设备类型ID可能包含下划线，所以只能分割前两个下划线
extract_ip_and_device_type(DevAddr) ->
    % 分割第一个下划线：得到 IP 和剩余部分
    case binary:split(DevAddr, <<"_">>) of
        [IpBin, Rest] ->
            % 分割第二个下划线：得到端口和设备类型
            case binary:split(Rest, <<"_">>) of
                [_PortBin, DeviceTypeId] ->
                    {ok, IpBin, DeviceTypeId};
                [_PortBin] ->
                    ?LOG(debug, "[SURFACE] DevAddr缺少设备类型ID: ~s，默认使用 wrj_dm_zqy", [DevAddr]),
                    {ok, IpBin, <<"wrj_dm_zqy">>}
            end;
        [IpBin] ->
            % 没有下划线，只有IP
            ?LOG(debug, "[SURFACE] DevAddr只有IP: ~s，默认使用 wrj_dm_zqy", [DevAddr]),
            {ok, IpBin, <<"wrj_dm_zqy">>}
    end.

%% 为舵面数据添加位置前缀
add_position_prefix(Prefix, Data) when is_binary(Prefix), is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
        NewKey = <<Prefix/binary, "_", K/binary>>,
        Acc#{NewKey => V}
    end, #{}, Data).

%% 过滤只保留合法的舵面字段（防止意外字段污染）
filter_legal_surface_fields(Prefix, PrefixedData) ->
    LegalKeys = [<<Prefix/binary, "_", Field/binary>> || Field <- ?LEGAL_SURFACE_FIELDS],
    maps:with(LegalKeys, PrefixedData).

%% @doc 测试函数 - 检查舵面数据汇聚状态
test() ->
    ?LOG(info, "========================================", []),
    ?LOG(info, "[SURFACE] ========== 测试开始 ==========", []),
    
    % 1. 检查ETS表状态
    try ets:info(uav_aggregate_state) of
        Info when is_list(Info) ->
            ?LOG(info, "[SURFACE] ETS表存在，状态: ~p", [proplists:get_value(size, Info)]);
        undefined ->
            ?LOG(warning, "[SURFACE] ETS表不存在，调用初始化"),
            dgiot_uav_business_service:init_ets()
    catch
        _:Error ->
            ?LOG(error, "[SURFACE] ETS表检查失败: ~p", [Error])
    end,
    
    % 2. 查看当前汇聚状态
    try
        AllStates = ets:tab2list(uav_aggregate_state),
        ?LOG(info, "[SURFACE] 当前汇聚状态数: ~p", [length(AllStates)]),
        lists:foreach(fun({DroneId, Data}) ->
            ?LOG(info, "[SURFACE] 无人机ID: ~s, 字段数: ~p, 字段: ~p", 
                  [DroneId, maps:size(Data), maps:keys(Data)])
        end, AllStates)
    catch
        _:Error2 ->
            ?LOG(error, "[SURFACE] 查询汇聚状态失败: ~p", [Error2])
    end,
    
    ?LOG(info, "[SURFACE] ========== 测试完成 ==========", []),
    ?LOG(info, "========================================", []),
    ok.