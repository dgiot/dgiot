%%%-------------------------------------------------------------------
%%% @doc
%%% link_data.erl - 链路遥测数据解析模块
%%%
%%% 对应协议文档表5-4和表5-5，完整解析0DH～7FH的115字节数据
%%% 包含0DH～70H的基础字段，以及34H～70H的空中节点链路遥测数据
%%%
%%% 主要功能：
%%% - parse/1: 解析完整的链路遥测数据（从0DH开始），返回 #link_status_full 记录
%%% - format/1: 格式化输出链路状态
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(link_data).

-export([parse/1, format/1]).

-include("link_data.hrl").
-include_lib("dgiot/include/logger.hrl").

%%%===================================================================
%%% 主解析函数：解析完整的链路遥测数据（从0DH开始，共115字节）
%%%===================================================================

-spec parse(binary()) -> {ok, #link_status_full{}} | {error, term()}.
parse(<<
        %% 0DH～70H 基础字段
        UpLinkBER:8,                         % 0DH 上行接收信道误码率
        AirStatus:8,                         % 0EH 空中节点状态
        AirAGC:8,                             % 0FH 空中节点接收AGC
        WorkChannel:8,                        % 10H 工作频道
        AirSetChannel:8,                      % 11H 空中节点设置频道
        AirSetAddrHigh:8, AirSetAddrLow:8,    % 12H-13H 空中节点设置地址
        DownLinkBER:8,                        % 14H 下行接收信道误码率
        GroundStatus:8,                       % 15H 地面状态
        GroundAGC1:8,                         % 16H 地面直收1AGC
        GroundAGC2:8,                         % 17H 地面直收2AGC
        GroundAGC3:8,                         % 18H 下行转发AGC
        GroundWorkChannel:8,                  % 19H 地面工作频道
        GroundSetChannel:8,                    % 1AH 地面设置频道
        GroundSetAddrHigh:8, GroundSetAddrLow:8, % 1BH-1CH 地面设置地址
        GroundPower:8,                         % 1DH 地面功率状态
        GroundWorkAddrHigh:8, GroundWorkAddrLow:8, % 1EH-1FH 地面工作地址
        _Backup20_25:6/binary,                 % 20H-25H 备用
        RangeHigh:8, RangeLow:8,                % 26H-27H 测距值
        AirTemp:8,                              % 28H 空中节点温度
        _Backup29_33:11/binary,                 % 29H-33H 备用
        AirLinkExtData:61/binary,               % 34H-70H 空中节点链路遥测数据
        %% 71H～7FH 网络管理字段
        NetworkAccessFlag:8,                    % 71H 入网申请标志
        NodeAddrHigh:8, NodeAddrLow:8,          % 72H-73H 本节点地址
        GrantDenyByte:8,                        % 74H 获得许可及拒绝节点个数
        Online1High:8, Online1Low:8,            % 75H-76H 在线节点1地址
        Online2High:8, Online2Low:8,            % 77H-78H 在线节点2地址
        _Reserved79:8,                          % 79H 备用
        DeniedHigh:8, DeniedLow:8,              % 7AH-7BH 最近拒绝节点地址
        Crc1High:8, Crc1Low:8,                  % 7CH-7DH CRC1
        Crc2High:8, Crc2Low:8                    % 7EH-7FH CRC2
>>) ->
    AirSetAddr = (AirSetAddrHigh bsl 8) bor AirSetAddrLow,
    GroundSetAddr = (GroundSetAddrHigh bsl 8) bor GroundSetAddrLow,
    GroundWorkAddr = (GroundWorkAddrHigh bsl 8) bor GroundWorkAddrLow,
    Range = (RangeHigh bsl 8) bor RangeLow,
    NodeAddress = (NodeAddrHigh bsl 8) bor NodeAddrLow,
    GrantedCount = (GrantDenyByte bsr 4) band 16#0F,
    DeniedCount = GrantDenyByte band 16#0F,
    Online1 = (Online1High bsl 8) bor Online1Low,
    Online2 = (Online2High bsl 8) bor Online2Low,
    LatestDenied = (DeniedHigh bsl 8) bor DeniedLow,
    Crc1 = (Crc1High bsl 8) bor Crc1Low,
    Crc2 = (Crc2High bsl 8) bor Crc2Low,

    %% 解析 34H～70H 的空中节点链路遥测数据
    AirLinkExt = parse_air_link_ext(AirLinkExtData),

    %% 组装在线节点列表
    OnlineNodes = lists:filter(fun(X) -> X =/= 0 end, [Online1, Online2]),

    {ok, #link_status_full{
        up_link_ber = UpLinkBER,
        air_status = AirStatus,
        air_agc = AirAGC,
        work_channel = WorkChannel,
        air_set_channel = AirSetChannel,
        air_set_addr = AirSetAddr,
        down_link_ber = DownLinkBER,
        ground_status = GroundStatus,
        ground_agc1 = GroundAGC1,
        ground_agc2 = GroundAGC2,
        ground_agc3 = GroundAGC3,
        ground_work_channel = GroundWorkChannel,
        ground_set_channel = GroundSetChannel,
        ground_set_addr = GroundSetAddr,
        ground_power = GroundPower,
        ground_work_addr = GroundWorkAddr,
        range = Range,
        air_temp = AirTemp,
        air_link_ext = AirLinkExt,
        network_access_flag = NetworkAccessFlag,
        node_address = NodeAddress,
        granted_count = GrantedCount,
        denied_count = DeniedCount,
        online_nodes = OnlineNodes,
        latest_denied = LatestDenied,
        crc1 = Crc1,
        crc2 = Crc2
    }};
parse(_) ->
    {error, invalid_link_data}.

%%%===================================================================
%%% 内部函数：解析空中节点链路遥测数据（34H～70H，61字节）
%%%===================================================================

-spec parse_air_link_ext(binary()) -> #air_link_ext{}.
parse_air_link_ext(<<
        JammingDetection:8,                    % 02H: 机载干扰检测回报
        AirSubAddrByte:8,                      % 03H: 空中节点子地址及传输速率
        LinkSwitchReturn:8,                    % 04H: 链路开关指令回报
        TerminalDeviceStatus:8,                 % 05H: 空中节点终端设备状态
        TerminalWorkStatus:8,                   % 06H: 空中节点终端工作状态
        WorkChannel:8,                          % 07H: 工作频道
        TransceiverStatus:8,                    % 08H: 收发信机状态
        ReceiveAGC:8,                           % 09H: 接收AGC
        PhysAddrHigh:8, PhysAddrLow:8,          % 0AH-0BH: 物理地址装订结果
        ChannelWorkMode:8,                      % 0CH: 频道工作模式
        ChannelResult:8,                         % 0DH: 频道装订结果
        Channel1Freq:8,                          % 0EH: 1频道频点
        ConstantTransmitResult:8,                % 0FH: 常发功能装订结果
        %% 遥控直收状态 (10H-14H)
        DirectRecvStatus:8,                      % 10H: 接收状态
        DirectRecvAGC:8,                         % 11H: AGC
        DirectBER1:8, DirectBER2:8, DirectBER3:8,% 12H-14H: 信道误码率（24位）
        DirectSysBER1:8, DirectSysBER2:8, DirectSysBER3:8, % 15H-17H: 系统误码率（24位）
        %% 转发接收状态 (18H-1CH)
        ForwardRecvStatus:8,                     % 18H: 接收状态
        ForwardRecvAGC:8,                        % 19H: AGC
        ForwardBER1:8, ForwardBER2:8, ForwardBER3:8, % 1AH-1CH: 信道误码率（24位）
        ForwardSysBER1:8, ForwardSysBER2:8, ForwardSysBER3:8, % 1DH-1FH: 系统误码率（24位）
        %% 下行转发接收状态 (20H-24H)
        DownlinkRecvStatus:8,                    % 20H: 接收状态
        DownlinkRecvAGC:8,                       % 21H: AGC
        DownlinkBER1:8, DownlinkBER2:8, DownlinkBER3:8, % 22H-24H: 信道误码率（24位）
        DownlinkSysBER1:8, DownlinkSysBER2:8, DownlinkSysBER3:8, % 25H-27H: 系统误码率（24位）
        _Reserved28_2A:3/binary,                 % 28H-2AH: 备用
        %% 频谱感知AGC (2BH-33H)
        Ch1AGC:8, Ch2AGC:8, Ch3AGC:8, Ch4AGC:8, Ch5AGC:8,
        Ch6AGC:8, Ch7AGC:8, Ch8AGC:8, Ch9AGC:8,
        SpectrumChecksum:8,                      % 34H: 频谱感知校验和
        %% 频道参数装订结果 (35H-3CH)
        Ch2Result:8, Ch3Result:8, Ch4Result:8, Ch5Result:8,
        Ch6Result:8, Ch7Result:8, Ch8Result:8, Ch9Result:8,
        _Rest/binary
>>) when byte_size(_Rest) >= 61-50 ->
    PhysicalAddress = (PhysAddrHigh bsl 8) bor PhysAddrLow,

    AirSubAddr = (AirSubAddrByte bsr 2) band 16#03,
    TransRate = AirSubAddrByte band 16#03,

    DirectBER = (DirectBER1 bsl 16) bor (DirectBER2 bsl 8) bor DirectBER3,
    DirectSysBER = (DirectSysBER1 bsl 16) bor (DirectSysBER2 bsl 8) bor DirectSysBER3,
    ForwardBER = (ForwardBER1 bsl 16) bor (ForwardBER2 bsl 8) bor ForwardBER3,
    ForwardSysBER = (ForwardSysBER1 bsl 16) bor (ForwardSysBER2 bsl 8) bor ForwardSysBER3,
    DownlinkBER = (DownlinkBER1 bsl 16) bor (DownlinkBER2 bsl 8) bor DownlinkBER3,
    DownlinkSysBER = (DownlinkSysBER1 bsl 16) bor (DownlinkSysBER2 bsl 8) bor DownlinkSysBER3,

    #air_link_ext{
        jamming_detection = JammingDetection,
        air_sub_address = AirSubAddr,
        transmission_rate = TransRate,
        link_switch_return = LinkSwitchReturn,
        terminal_device_status = TerminalDeviceStatus,
        terminal_work_status = TerminalWorkStatus,
        work_channel = WorkChannel,
        transceiver_status = TransceiverStatus,
        receive_agc = ReceiveAGC,
        physical_address_result = PhysicalAddress,
        channel_work_mode = ChannelWorkMode,
        channel_result = ChannelResult,
        channel_1_freq = Channel1Freq,
        constant_transmit_result = ConstantTransmitResult,
        direct_receive_status = DirectRecvStatus,
        direct_receive_agc = DirectRecvAGC,
        direct_channel_ber = DirectBER,
        direct_system_ber = DirectSysBER,
        forward_receive_status = ForwardRecvStatus,
        forward_receive_agc = ForwardRecvAGC,
        forward_channel_ber = ForwardBER,
        forward_system_ber = ForwardSysBER,
        downlink_receive_status = DownlinkRecvStatus,
        downlink_receive_agc = DownlinkRecvAGC,
        downlink_channel_ber = DownlinkBER,
        downlink_system_ber = DownlinkSysBER,
        channel_1_agc = Ch1AGC,
        channel_2_agc = Ch2AGC,
        channel_3_agc = Ch3AGC,
        channel_4_agc = Ch4AGC,
        channel_5_agc = Ch5AGC,
        channel_6_agc = Ch6AGC,
        channel_7_agc = Ch7AGC,
        channel_8_agc = Ch8AGC,
        channel_9_agc = Ch9AGC,
        spectrum_checksum = SpectrumChecksum,
        channel_2_result = Ch2Result,
        channel_3_result = Ch3Result,
        channel_4_result = Ch4Result,
        channel_5_result = Ch5Result,
        channel_6_result = Ch6Result,
        channel_7_result = Ch7Result,
        channel_8_result = Ch8Result,
        channel_9_result = Ch9Result
    }.

%%%===================================================================
%%% 格式化输出函数
%%%===================================================================

-spec format(#link_status_full{}) -> binary().
format(#link_status_full{
    up_link_ber = UpLinkBER,
    air_status = AirStatus,
    air_agc = AirAGC,
    work_channel = WorkChannel,
    air_set_channel = AirSetChannel,
    air_set_addr = AirSetAddr,
    down_link_ber = DownLinkBER,
    ground_status = GroundStatus,
    ground_agc1 = GroundAGC1,
    ground_agc2 = GroundAGC2,
    ground_agc3 = GroundAGC3,
    ground_work_channel = GroundWorkChannel,
    ground_set_channel = GroundSetChannel,
    ground_set_addr = GroundSetAddr,
    ground_power = GroundPower,
    ground_work_addr = GroundWorkAddr,
    range = Range,
    air_temp = AirTemp,
    air_link_ext = AirLinkExt,
    network_access_flag = AccessFlag,
    node_address = NodeAddr,
    granted_count = Granted,
    denied_count = Denied,
    online_nodes = Online,
    latest_denied = LatestDenied,
    crc1 = Crc1,
    crc2 = Crc2
}) ->
    AccessText = case AccessFlag of
        16#AA -> <<"有效申请"/utf8>>;
        16#00 -> <<"无效"/utf8>>;
        _ -> <<"未知"/utf8>>
    end,
    OnlineStr = list_to_binary(lists:join(<<", "/utf8>>, [integer_to_binary(Addr) || Addr <- Online])),

    ExtStr = format_air_link_ext(AirLinkExt),

    FormatString = "完整链路状态:~n"
                   "  上行接收信道误码率: ~p~n"
                   "  空中节点状态字节: 0x~2.16.0B~n"
                   "  空中节点接收AGC: ~p (0~5V)~n"
                   "  工作频道: ~p~n"
                   "  空中节点设置频道: ~p, 设置地址: 0x~4.16.0B~n"
                   "  下行接收信道误码率: ~p~n"
                   "  地面状态字节: 0x~2.16.0B~n"
                   "  地面直收1AGC: ~p, 直收2AGC: ~p, 下行转发AGC: ~p~n"
                   "  地面工作频道: ~p, 设置频道: ~p, 设置地址: 0x~4.16.0B~n"
                   "  地面功率状态: ~p (1=小,2=中,3=大)~n"
                   "  地面工作地址: 0x~4.16.0B~n"
                   "  测距值: ~p m (分辨率2.17m)~n"
                   "  空中节点温度: ~p °C~n"
                   "空中节点链路遥测数据:~n~ts"
                   "  入网申请标志: ~ts (0x~2.16.0B)~n"
                   "  本节点地址: 0x~4.16.0B~n"
                   "  获得许可节点数: ~p, 被拒绝节点数: ~p~n"
                   "  在线节点地址: [~ts]~n"
                   "  最近被拒绝节点: 0x~4.16.0B~n"
                   "  CRC1: 0x~4.16.0B, CRC2: 0x~4.16.0B~n",
    Formatted = io_lib:format(FormatString,
        [UpLinkBER, AirStatus, AirAGC, WorkChannel, AirSetChannel, AirSetAddr,
         DownLinkBER, GroundStatus, GroundAGC1, GroundAGC2, GroundAGC3,
         GroundWorkChannel, GroundSetChannel, GroundSetAddr,
         GroundPower, GroundWorkAddr,
         Range, AirTemp,
         ExtStr,
         AccessText, AccessFlag,
         NodeAddr,
         Granted, Denied,
         OnlineStr,
         LatestDenied,
         Crc1, Crc2]),
    unicode:characters_to_binary(Formatted, utf8, utf8).

%% 内部格式化函数：格式化空中节点链路遥测数据
-spec format_air_link_ext(#air_link_ext{}) -> iolist().
format_air_link_ext(#air_link_ext{
    jamming_detection = Jamming,
    air_sub_address = SubAddr,
    transmission_rate = Rate,
    link_switch_return = LinkSwitch,
    terminal_device_status = DevStatus,
    terminal_work_status = WorkStatus,
    work_channel = WorkCh,
    transceiver_status = TransStatus,
    receive_agc = RecAGC,
    physical_address_result = PhysAddr,
    channel_work_mode = ChWorkMode,
    channel_result = ChResult,
    channel_1_freq = Ch1Freq,
    constant_transmit_result = ConstTrans,
    direct_receive_status = DirStatus,
    direct_receive_agc = DirAGC,
    direct_channel_ber = DirBER,
    direct_system_ber = DirSysBER,
    forward_receive_status = FwdStatus,
    forward_receive_agc = FwdAGC,
    forward_channel_ber = FwdBER,
    forward_system_ber = FwdSysBER,
    downlink_receive_status = DnStatus,
    downlink_receive_agc = DnAGC,
    downlink_channel_ber = DnBER,
    downlink_system_ber = DnSysBER,
    channel_1_agc = Ch1AGC,
    channel_2_agc = Ch2AGC,
    channel_3_agc = Ch3AGC,
    channel_4_agc = Ch4AGC,
    channel_5_agc = Ch5AGC,
    channel_6_agc = Ch6AGC,
    channel_7_agc = Ch7AGC,
    channel_8_agc = Ch8AGC,
    channel_9_agc = Ch9AGC,
    spectrum_checksum = SpecSum,
    channel_2_result = Ch2Res,
    channel_3_result = Ch3Res,
    channel_4_result = Ch4Res,
    channel_5_result = Ch5Res,
    channel_6_result = Ch6Res,
    channel_7_result = Ch7Res,
    channel_8_result = Ch8Res,
    channel_9_result = Ch9Res
}) ->
    JammingText = case Jamming of
        16#55 -> <<"干扰"/utf8>>;
        16#AA -> <<"无干扰"/utf8>>;
        _ -> <<"未知"/utf8>>
    end,
    RateText = case Rate of
        0 -> <<"2M"/utf8>>;
        1 -> <<"8M"/utf8>>;
        2 -> <<"4M"/utf8>>;
        3 -> <<"0M"/utf8>>;
        _ -> <<"未知"/utf8>>
    end,

    FormatString =
        "    机载干扰检测: ~ts (0x~2.16.0B)~n"
        "    空中节点子地址: ~p, 传输速率: ~ts~n"
        "    链路开关指令回报: 0x~2.16.0B~n"
        "    终端设备状态: 0x~2.16.0B, 工作状态: 0x~2.16.0B~n"
        "    工作频道: ~p, 收发信机状态: 0x~2.16.0B, 接收AGC: ~p~n"
        "    物理地址装订结果: 0x~4.16.0B, 频道工作模式: 0x~2.16.0B~n"
        "    频道装订结果: ~p, 1频道频点: ~p, 常发功能装订结果: 0x~2.16.0B~n"
        "    遥控直收状态: 0x~2.16.0B, AGC: ~p, 信道误码率: ~p, 系统误码率: ~p~n"
        "    转发接收状态: 0x~2.16.0B, AGC: ~p, 信道误码率: ~p, 系统误码率: ~p~n"
        "    下行转发接收状态: 0x~2.16.0B, AGC: ~p, 信道误码率: ~p, 系统误码率: ~p~n"
        "    频谱感知AGC(1~9): ~p,~p,~p,~p,~p,~p,~p,~p,~p, 校验和: ~p~n"
        "    频道参数装订结果(2~9): ~p,~p,~p,~p,~p,~p,~p,~p~n",
    Formatted = io_lib:format(FormatString,
        [JammingText, Jamming, SubAddr, RateText,
         LinkSwitch, DevStatus, WorkStatus,
         WorkCh, TransStatus, RecAGC,
         PhysAddr, ChWorkMode, ChResult, Ch1Freq, ConstTrans,
         DirStatus, DirAGC, DirBER, DirSysBER,
         FwdStatus, FwdAGC, FwdBER, FwdSysBER,
         DnStatus, DnAGC, DnBER, DnSysBER,
         Ch1AGC, Ch2AGC, Ch3AGC, Ch4AGC, Ch5AGC, Ch6AGC, Ch7AGC, Ch8AGC, Ch9AGC, SpecSum,
         Ch2Res, Ch3Res, Ch4Res, Ch5Res, Ch6Res, Ch7Res, Ch8Res, Ch9Res]),
    Formatted.