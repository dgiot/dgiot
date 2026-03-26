%%%-------------------------------------------------------------------
%%% @doc
%%% uav_extra_commands.erl - 额外命令数据解析模块
%%%
%%% 包含以下命令的解析：
%%% - 电池状态 (0xA1)
%%% - 飞控版本号 (0xC1)
%%% - 航点查询回复 (0xB4)
%%% - 舵面参数查询回复 (0xDE)
%%%
%%% 协议对应：D1.docx、D2.docx、D3.docx 中的额外命令帧。
%%%
%%% 主要功能：
%%% - parse_battery/1: 解析电池数据
%%% - format_battery/1: 格式化电池状态
%%% - parse_version/1: 解析版本数据
%%% - format_version/1: 格式化版本信息
%%% - parse_waypoint/1: 解析航点数据
%%% - format_waypoint/1: 格式化航点信息
%%% - parse_surface_calibration/1: 解析舵面校准数据
%%% - format_surface_calibration/1: 格式化舵面校准信息
%%% - channel_to_text/1: 将通道号转换为中文名称
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(uav_extra_commands).

%% 公共API
-export([
    parse_battery/1,
    format_battery/1,
    parse_version/1,
    format_version/1,
    parse_waypoint/1,
    format_waypoint/1,
    parse_surface_calibration/1,
    format_surface_calibration/1,
    channel_to_text/1
]).

%% 电池状态记录
-record(battery_status, {
    status_byte        :: integer(),          % 字节0：电池状态信息
    voltage            :: float(),             % 字节1-2：电压（0.01V）
    activate_state     :: integer(),           % 字节3：激活状态（0x1A/0xA1）
    temperature1       :: integer() | invalid, % 字节4：温度1（℃）
    temp2_or_count     :: integer(),           % 字节5：温度2 或 飞行次数
    sequence           :: integer(),           % 字节6：通信序列编号
    cmd_result         :: integer()            % 字节7：指令执行结果
}).

%% 版本记录
-record(version_info, {
    frame_length :: integer(),
    drone_model :: integer(),
    drone_id :: integer(),
    version_string :: binary(),
    crc :: integer()
}).

%% 航点记录
-record(waypoint_info, {
    latitude :: float(),
    longitude :: float(),
    altitude :: integer(),
    total_count :: integer(),
    waypoint_index :: integer(),
    crc :: integer()
}).

%% 舵面校准记录
-record(surface_calibration, {
    channel :: integer(),
    pwm_center :: float(),
    up_ratio :: float(),
    down_ratio :: float(),
    crc :: integer()
}).

%%%===================================================================
%%% 电池数据解析 (0xA1)
%%%===================================================================

-spec parse_battery(binary()) -> {ok, #battery_status{}} | {error, term()}.
parse_battery(<<
    StatusByte:8,
    VoltageHigh:8,
    VoltageLow:8,
    Activate:8,
    Temp1:8/signed,
    Temp2OrCount:8,
    Seq:8,
    Result:8
>>) ->
    Voltage = (VoltageHigh * 256 + VoltageLow) / 100.0,
    Temperature1 = case Temp1 of -128 -> invalid; _ -> Temp1 end,
    {ok, #battery_status{
        status_byte = StatusByte,
        voltage = Voltage,
        activate_state = Activate,
        temperature1 = Temperature1,
        temp2_or_count = Temp2OrCount,
        sequence = Seq,
        cmd_result = Result
    }};
parse_battery(_) ->
    {error, invalid_battery_data}.

-spec format_battery(#battery_status{}) -> binary().
format_battery(#battery_status{
    status_byte = StatusByte,
    voltage = Voltage,
    activate_state = Activate,
    temperature1 = Temp1,
    temp2_or_count = Temp2OrCount,
    sequence = Seq,
    cmd_result = Result
}) ->
    %% 解析状态字节
    BatteryType = case (StatusByte bsr 7) band 1 of
        0 -> <<"一次电池"/utf8>>;
        1 -> <<"二次电池"/utf8>>
    end,
    UnderVoltProtect = case (StatusByte bsr 6) band 1 of 1 -> <<"关断"/utf8>>; 0 -> <<"开启"/utf8>> end,
    MosfetStatus = case (StatusByte bsr 5) band 1 of 1 -> <<"关断"/utf8>>; 0 -> <<"开启"/utf8>> end,
    ActivateAbnormal = case (StatusByte bsr 4) band 1 of 1 -> <<"异常"/utf8>>; 0 -> <<"正常"/utf8>> end,
    OverTempAlarm = case (StatusByte bsr 3) band 1 of 1 -> <<"超温"/utf8>>; 0 -> <<"正常"/utf8>> end,
    VoltageAbnormal = case (StatusByte bsr 2) band 1 of 1 -> <<"异常"/utf8>>; 0 -> <<"正常"/utf8>> end,
    TempSensor1 = case (StatusByte bsr 1) band 1 of 1 -> <<"故障"/utf8>>; 0 -> <<"正常"/utf8>> end,
    TempSensor2 = case StatusByte band 1 of 1 -> <<"故障"/utf8>>; 0 -> <<"正常"/utf8>> end,

    ActivateText = case Activate of
        16#1A -> <<"激活已开启"/utf8>>;
        16#A1 -> <<"激活已关闭"/utf8>>;
        _ -> <<"未知"/utf8>>
    end,

    ResultText = case Result of
        16#77 -> <<"激活成功"/utf8>>;
        16#99 -> <<"激活未执行"/utf8>>;
        _ -> <<"其他"/utf8>>
    end,

    FormatString = "电池状态:~n"
                   "  类型: ~ts, 欠压保护: ~ts, MOS管: ~ts~n"
                   "  激活异常: ~ts, 超温报警: ~ts, 电压异常: ~ts~n"
                   "  温度传感器1: ~ts, 温度传感器2: ~ts~n"
                   "  电压: ~.2f V, 激活状态: ~ts (0x~2.16.0B)~n"
                   "  温度1: ~p ℃, 温度2/飞行次数: ~p~n"
                   "  序列号: ~p, 指令结果: ~ts (0x~2.16.0B)~n",
    FormattedString = io_lib:format(FormatString,
                  [BatteryType, UnderVoltProtect, MosfetStatus,
                   ActivateAbnormal, OverTempAlarm, VoltageAbnormal,
                   TempSensor1, TempSensor2,
                   Voltage, ActivateText, Activate,
                   Temp1, Temp2OrCount,
                   Seq, ResultText, Result]),
    unicode:characters_to_binary(FormattedString, utf8, utf8).

%%%===================================================================
%%% 版本数据解析 (0xC1)
%%%===================================================================

-spec parse_version(binary()) -> {ok, #version_info{}} | {error, term()}.
parse_version(<<
    FrameLen:8,
    DroneModel:8,
    DroneId:16/little,
    _CmdId:8,
    VersionData/binary
>>) ->
    ExpectedDataLen = FrameLen - 7,
    case VersionData of
        <<VerStr:ExpectedDataLen/binary, CRC:16/little, 16#AA>> ->
            CleanVer = filter_ascii(VerStr),
            {ok, #version_info{
                frame_length = FrameLen,
                drone_model = DroneModel,
                drone_id = DroneId,
                version_string = CleanVer,
                crc = CRC
            }};
        _ ->
            {error, invalid_version_data}
    end;
parse_version(_) ->
    {error, invalid_format}.

-spec format_version(#version_info{}) -> iolist().
format_version(#version_info{version_string = VerStr, drone_id = DroneId}) ->
    io_lib:format("飞控版本信息: 飞机ID=~p, 版本号=~s", [DroneId, VerStr]).

%% 过滤ASCII字符串
filter_ascii(Bin) -> filter_ascii(Bin, <<>>).

filter_ascii(<<>>, Acc) -> Acc;
filter_ascii(<<0, Rest/binary>>, Acc) -> filter_ascii(Rest, Acc);
filter_ascii(<<C, Rest/binary>>, Acc) when C >= 32, C =< 126 ->
    filter_ascii(Rest, <<Acc/binary, C>>);
filter_ascii(<<_C, Rest/binary>>, Acc) ->
    filter_ascii(Rest, Acc).

%%%===================================================================
%%% 航点数据解析 (0xB4)
%%%===================================================================

-spec parse_waypoint(binary()) -> {ok, #waypoint_info{}} | {error, term()}.
parse_waypoint(<<
    _FrameHeader:16,
    _FrameLen:8,
    _DroneModel:8,
    _DroneId:16/little,
    _CmdId:8,
    LatitudeRaw:32/little-signed,
    LongitudeRaw:32/little-signed,
    _Reserved:4/binary,
    Altitude:16/little-signed,
    TotalCount:8,
    WaypointIndex:8,
    _Reserved2:8,
    CRC:16/little,
    16#AA
>>) ->
    {ok, #waypoint_info{
        latitude = LatitudeRaw / 10000000.0,
        longitude = LongitudeRaw / 10000000.0,
        altitude = Altitude,
        total_count = TotalCount,
        waypoint_index = WaypointIndex,
        crc = CRC
    }};
parse_waypoint(_) ->
    {error, invalid_waypoint_data}.

-spec format_waypoint(#waypoint_info{}) -> iolist().
format_waypoint(#waypoint_info{latitude = Lat, longitude = Lon, altitude = Alt,
                               total_count = Total, waypoint_index = Idx}) ->
    io_lib:format("航点 ~p/~p: 纬度=~.7f, 经度=~.7f, 高度=~p m",
                  [Idx, Total, Lat, Lon, Alt]).

%%%===================================================================
%%% 舵面校准数据解析 (0xDE)
%%%===================================================================

-spec parse_surface_calibration(binary()) -> {ok, #surface_calibration{}} | {error, term()}.
parse_surface_calibration(<<
    _FrameHeader:16,
    _FrameLen:8,
    _DroneModel:8,
    _DroneId:16/little,
    _CmdId:8,
    Channel:8,
    PwmCenterRaw:16/little-signed,
    UpRatioRaw:16/little-signed,
    DownRatioRaw:16/little-signed,
    _Reserved:3/binary,
    CRC:16/little,
    16#AA
>>) ->
    {ok, #surface_calibration{
        channel = Channel,
        pwm_center = PwmCenterRaw / 1000.0,
        up_ratio = UpRatioRaw / 1000.0,
        down_ratio = DownRatioRaw / 1000.0,
        crc = CRC
    }};
parse_surface_calibration(_) ->
    {error, invalid_surface_calibration_data}.

-spec format_surface_calibration(#surface_calibration{}) -> iolist().
format_surface_calibration(#surface_calibration{channel = Ch, pwm_center = Pwm,
                                                up_ratio = Up, down_ratio = Down}) ->
    io_lib:format("舵面 ~ts: PWM中位=~.3f, 上偏比例=~.3f, 下偏比例=~.3f",
                  [channel_to_text(Ch), Pwm, Up, Down]).

-spec channel_to_text(integer()) -> binary().
channel_to_text(1) -> <<"左前舵"/utf8>>;
channel_to_text(2) -> <<"右前舵"/utf8>>;
channel_to_text(3) -> <<"左垂尾"/utf8>>;
channel_to_text(4) -> <<"右垂尾"/utf8>>;
channel_to_text(_) -> <<"未知通道"/utf8>>.