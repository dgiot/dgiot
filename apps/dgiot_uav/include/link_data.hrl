%%%-------------------------------------------------------------------
%%% @doc
%%% link_data.hrl - 链路遥测数据完整记录定义
%%%
%%% 对应协议文档表5-4 和 表5-5。
%%% 包含 air_link_ext 和 link_status_full 两个记录。
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(LINK_DATA_HRL).
-define(LINK_DATA_HRL, true).

%% 空中节点链路遥测数据（表5-5）记录
-record(air_link_ext, {
    jamming_detection :: integer(),          % 02H: 机载干扰检测回报 (55H/AAH)
    air_sub_address :: integer(),            % 03H D3D2: 空中节点子地址
    transmission_rate :: integer(),          % 03H D1D0: 传输速率回报
    link_switch_return :: integer(),         % 04H: 链路开关指令回报
    terminal_device_status :: integer(),     % 05H: 空中节点终端设备状态
    terminal_work_status :: integer(),       % 06H: 空中节点终端工作状态
    work_channel :: integer(),               % 07H: 工作频道
    transceiver_status :: integer(),         % 08H: 收发信机状态
    receive_agc :: integer(),                % 09H: 接收AGC
    physical_address_result :: integer(),    % 0AH-0BH: 物理地址装订结果
    channel_work_mode :: integer(),          % 0CH: 频道工作模式
    channel_result :: integer(),              % 0DH: 频道装订结果
    channel_1_freq :: integer(),              % 0EH: 1频道
    constant_transmit_result :: integer(),   % 0FH: 常发功能装订结果
    %% 直收状态（遥控直收）
    direct_receive_status :: integer(),      % 10H: 接收状态
    direct_receive_agc :: integer(),         % 11H: AGC
    direct_channel_ber :: integer(),         % 12H-14H: 信道误码率（24位）
    direct_system_ber :: integer(),          % 15H-17H: 系统误码率（24位）
    %% 转发接收状态
    forward_receive_status :: integer(),     % 18H: 接收状态
    forward_receive_agc :: integer(),        % 19H: AGC
    forward_channel_ber :: integer(),        % 1AH-1CH: 信道误码率（24位）
    forward_system_ber :: integer(),         % 1DH-1FH: 系统误码率（24位）
    %% 下行转发接收状态
    downlink_receive_status :: integer(),    % 20H: 接收状态
    downlink_receive_agc :: integer(),       % 21H: AGC
    downlink_channel_ber :: integer(),       % 22H-24H: 信道误码率（24位）
    downlink_system_ber :: integer(),        % 25H-27H: 系统误码率（24位）
    %% 频谱感知AGC
    channel_1_agc :: integer(),              % 2BH: 1频道AGC
    channel_2_agc :: integer(),              % 2CH: 2频道AGC
    channel_3_agc :: integer(),              % 2DH: 3频道AGC
    channel_4_agc :: integer(),              % 2EH: 4频道AGC
    channel_5_agc :: integer(),              % 2FH: 5频道AGC
    channel_6_agc :: integer(),              % 30H: 6频道AGC
    channel_7_agc :: integer(),              % 31H: 7频道AGC
    channel_8_agc :: integer(),              % 32H: 8频道AGC
    channel_9_agc :: integer(),              % 33H: 9频道AGC
    spectrum_checksum :: integer(),          % 34H: 频谱感知校验和
    %% 频道参数装订结果上报
    channel_2_result :: integer(),           % 35H: 2频道
    channel_3_result :: integer(),           % 36H: 3频道
    channel_4_result :: integer(),           % 37H: 4频道
    channel_5_result :: integer(),           % 38H: 5频道
    channel_6_result :: integer(),           % 39H: 6频道
    channel_7_result :: integer(),           % 3AH: 7频道
    channel_8_result :: integer(),           % 3BH: 8频道
    channel_9_result :: integer()            % 3CH: 9频道
}).

%% 完整链路状态记录（表5-4 + 表5-5）
-record(link_status_full, {
    %% 0DH～70H
    up_link_ber :: integer(),                % 0DH: 上行接收信道误码率
    air_status :: integer(),                 % 0EH: 空中节点状态字节
    air_agc :: integer(),                    % 0FH: 空中节点接收AGC
    work_channel :: integer(),                % 10H: 工作频道
    air_set_channel :: integer(),             % 11H: 空中节点设置频道
    air_set_addr :: integer(),                % 12H-13H: 空中节点设置地址
    down_link_ber :: integer(),               % 14H: 下行接收信道误码率
    ground_status :: integer(),               % 15H: 地面状态字节
    ground_agc1 :: integer(),                 % 16H: 地面直收1AGC
    ground_agc2 :: integer(),                 % 17H: 地面直收2AGC
    ground_agc3 :: integer(),                 % 18H: 下行转发AGC
    ground_work_channel :: integer(),         % 19H: 地面工作频道
    ground_set_channel :: integer(),          % 1AH: 地面设置频道
    ground_set_addr :: integer(),             % 1BH-1CH: 地面设置地址
    ground_power :: integer(),                % 1DH: 地面功率状态
    ground_work_addr :: integer(),            % 1EH-1FH: 地面工作地址
    range :: integer(),                       % 26H-27H: 测距值
    air_temp :: integer(),                    % 28H: 空中节点温度
    air_link_ext :: #air_link_ext{},          % 34H-70H: 空中节点链路遥测数据（表5-5）
    %% 71H～7FH
    network_access_flag :: integer(),         % 71H: 入网申请有效标志
    node_address :: integer(),                % 72H-73H: 本节点地址
    granted_count :: integer(),               % 74H高4位: 获得许可节点数
    denied_count :: integer(),                 % 74H低4位: 被拒绝节点数
    online_nodes :: list(integer()),          % 75H-...: 在网节点地址列表
    latest_denied :: integer(),                % 7AH-7BH: 最近被拒绝节点
    crc1 :: integer(),                         % 7CH-7DH: CRC1
    crc2 :: integer()                           % 7EH-7FH: CRC2
}).

-endif.