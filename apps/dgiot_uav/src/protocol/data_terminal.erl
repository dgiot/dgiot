%%%-------------------------------------------------------------------
%%% @doc
%%% data_terminal.erl - 数据终端协议处理模块
%%%
%%% 本模块负责编码和解码数据终端帧，包括复合数据帧和图像数据帧。
%%% 协议对应：Payload.docx 中的“任务载荷与机载数据终端通讯协议”
%%% 帧格式为 EB 90 开头，后跟数据类型、帧计数、密钥、CRC、有效数据。
%%% 复合数据帧包含载荷参数（46字节）和飞机参数（37字节），
%%% 图像帧包含H.264编码数据（最多115字节）。
%%%
%%% 主要功能：
%%% - encode_frame/4: 编码数据终端帧（可填充预留字段）
%%% - decode_frame/1: 解码数据终端帧
%%% - encode_composite/2: 编码复合数据帧
%%% - decode_composite/1: 解码复合数据帧
%%% - encode_image/3: 编码图像帧
%%% - decode_image/1: 解码图像帧
%%% - default_payload_params/0: 返回默认的载荷参数二进制（46字节）
%%% - default_aircraft_params_binary/0: 返回默认的飞机参数二进制（37字节）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(data_terminal).

-export([
    encode_frame/4, decode_frame/1,
    encode_composite/2, decode_composite/1,
    encode_image/3, decode_image/1,
    get_data_type_name/1,
    default_payload_params/0,
    default_aircraft_params_binary/0,
    test/0
]).

-include_lib("dgiot_uav/include/dgiot_uav.hrl").
-include_lib("dgiot/include/logger.hrl").

%% 基本类型定义
-define(UINT8, 8/unsigned-little-integer).
-define(UINT16, 16/unsigned-little-integer).
-define(INT16, 16/signed-little-integer).
-define(INT32, 32/signed-little-integer).

%% 同步字节
-define(FC_SYNC_BYTES, <<16#EB, 16#90>>).
-define(PL_SYNC_BYTES, <<16#AA, 16#55>>).

%% 错误码
-define(ERR_INVALID_SYNC, invalid_sync).
-define(ERR_CRC_MISMATCH, crc_mismatch).
-define(ERR_INVALID_COMMAND, invalid_command).

%% 载荷类型
-define(PAYLOAD_TYPE_VISIBLE, 0).

%% 跟踪标志
-define(TRACKING_OFF, 0).

%% 命令类型
-define(CMD_NULL, 16#00).

%% 数据类型
-define(DATA_TYPE_VISIBLE_IMG, 16#14).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @spec encode_frame(DataType, FrameNumber, PayloadData, FillZeros) -> binary()
%% 编码数据终端帧。如果 FillZeros 为 true，则填充地址、密钥、CRC 为 0（用于测试）。
encode_frame(DataType, FrameNumber, PayloadData, true) when
      is_binary(PayloadData), byte_size(PayloadData) =:= 115 ->
    <<
        ?FC_SYNC_BYTES/binary,
        0:16,            % 目标地址（填充）
        0:16,            % 源地址（填充）
        DataType:?UINT8,
        FrameNumber:?UINT8,
        0:24,            % 密钥（填充）
        0:16,            % CRC（填充）
        PayloadData/binary
    >>;
encode_frame(DataType, FrameNumber, PayloadData, false) ->
    <<
        ?FC_SYNC_BYTES/binary,
        DataType:?UINT8,
        FrameNumber:?UINT8,
        PayloadData/binary
    >>.

%% @spec decode_frame(Data::binary()) -> {ok, #data_terminal_frame{}} | {error, Reason}
decode_frame(<<16#EB, 16#90, _:2/binary, _:2/binary,
               DataType:?UINT8, FrameNumber:?UINT8, _:3/binary,
               _:2/binary, PayloadData:115/binary>>) ->
    Record = #data_terminal_frame{
        data_type = DataType,
        frame_number = FrameNumber,
        payload_data = PayloadData
    },
    {ok, Record};
decode_frame(_) ->
    {error, ?ERR_INVALID_SYNC}.

%% @spec encode_composite(PayloadData::binary(), AircraftData::binary()) -> binary()
%% 编码复合数据帧。PayloadData 为46字节的载荷参数，AircraftData 为37字节的飞机参数。
encode_composite(PayloadData, AircraftData) when
      is_binary(PayloadData), byte_size(PayloadData) =:= 46,
      is_binary(AircraftData), byte_size(AircraftData) =:= 37 ->
    Data = <<
        ?PL_SYNC_BYTES/binary,
        PayloadData/binary,
        AircraftData/binary,
        0:104  % 13字节保留
    >>,
    <<_:2/binary, CheckData:96/binary, _/binary>> = Data,
    CRC16 = uav_payload_checksum:calculate_crc16_big(CheckData),
    <<Data/binary, CRC16:?UINT16>>.

%% @spec decode_composite(Data::binary()) -> {ok, #composite_data{}} | {error, Reason}
decode_composite(<<16#AA, 16#55, PayloadData:46/binary,
                   AircraftData:37/binary, _:13/binary,
                   CRC16:?UINT16>>) ->
    CheckData = <<16#AA, 16#55, PayloadData/binary, AircraftData/binary>>,
    case uav_payload_checksum:calculate_crc16_big(CheckData) of
        CRC16 ->
            Record = #composite_data{
                payload_data = PayloadData,
                aircraft_data = AircraftData,
                crc16 = CRC16
            },
            {ok, Record};
        _ ->
            {error, ?ERR_CRC_MISMATCH}
    end;
decode_composite(_) ->
    {error, ?ERR_INVALID_SYNC}.

%% @spec encode_image(DataType, FrameNumber, ImageData) -> binary()
%% 编码图像帧。ImageData 长度不超过115字节，不足补零。
encode_image(DataType, FrameNumber, ImageData) when
      is_binary(ImageData), byte_size(ImageData) =< 115 ->
    DataLength = byte_size(ImageData),
    Header = <<
        ?FC_SYNC_BYTES/binary,
        0:16,
        0:16,
        DataType:?UINT8,
        FrameNumber:?UINT8,
        0:24,
        0:16
    >>,
    PaddingSize = 115 - DataLength,
    PaddedData = <<ImageData/binary, 0:(PaddingSize*8)>>,
    <<Header/binary, PaddedData/binary>>.

%% @spec decode_image(Data::binary()) -> {ok, DataType, FrameNumber, ImageData} | {error, Reason}
decode_image(<<16#EB, 16#90, _:32, DataType:?UINT8,
               FrameNumber:?UINT8, _:40, PayloadData:115/binary>>) ->
    case DataType band 16#0F of
        16#1D ->  % 复合数据
            <<ImageData:100/binary, _/binary>> = PayloadData,
            {ok, DataType, FrameNumber, ImageData};
        16#14 ->  % 可见光图像
            {ok, DataType, FrameNumber, PayloadData};
        16#15 ->  % 红外图像
            {ok, DataType, FrameNumber, PayloadData};
        16#10 ->  % 空数据
            {ok, DataType, FrameNumber, <<>>};
        _ ->
            {error, ?ERR_INVALID_COMMAND}
    end;
decode_image(_) ->
    {error, ?ERR_INVALID_SYNC}.

%% @spec get_data_type_name(DataType::integer()) -> string()
get_data_type_name(DataType) ->
    case DataType band 16#0F of
        16#1D -> "Composite Data";
        16#14 -> "Visible Image";
        16#15 -> "Infrared Image";
        16#10 -> "Null";
        _ -> "Unknown"
    end.

%% @spec default_payload_params() -> binary()
%% 返回默认的46字节载荷参数，按表格14构造。
default_payload_params() ->
    <<
        ((5 bsl 5) bor 1):?UINT8,        % 状态0：平台式可见光，手动调节
        0:?UINT8,                         % 状态1：默认值
        ((?PAYLOAD_TYPE_VISIBLE bsl 4) bor 1):?UINT8, % 载荷类型可见光，变倍1
        0:?INT16,                          % 俯仰角
        0:?UINT16,                         % 方位角
        0:?INT16,                          % 目标偏移X
        0:?INT16,                          % 目标偏移Y
        0:?INT16,                          % 目标俯仰
        0:?UINT16,                         % 目标方位
        0:?INT16,                          % 飞机俯仰
        0:?INT16,                          % 飞机横滚
        0:?UINT16,                         % 飞机偏航
        ?TRACKING_OFF:?UINT8,              % 跟踪标志
        0:184,                             % 23字节保留
        ?CMD_NULL:?UINT8                   % 接收命令
    >>.

%% @spec default_aircraft_params_binary() -> binary()
%% 返回默认的37字节飞机参数，按表格15构造。
default_aircraft_params_binary() ->
    <<
        0:?INT16,                    % ID
        0:?UINT8,                    % 导航状态
        0:56,                        % 7字节保留
        0:?INT32,                    % 纬度
        0:?INT32,                    % 经度
        0:?INT16,                    % GPS高度
        0:?INT16,                    % 地速
        0:?INT16,                    % 俯仰
        0:?INT16,                    % 横滚
        0:?UINT16,                   % 磁航向
        0:?UINT16,                   % 空速
        0:?INT16,                    % 气压高度
        0:?UINT16,                   % 航迹角
        0:?UINT8,                    % 卫星数
        0:16                         % 2字节保留
    >>.

%%%===================================================================
%%% 测试
%%%===================================================================

test() ->
    PayloadParams = default_payload_params(),
    AircraftParams = default_aircraft_params_binary(),
    CompositeFrame = encode_composite(PayloadParams, AircraftParams),
    ?LOG(info, "Composite frame size: ~p bytes", [byte_size(CompositeFrame)]),
    case decode_composite(CompositeFrame) of
        {ok, _CompositeData} ->
            ?LOG(info, "Decoded composite frame OK", []);
        {error, Reason} ->
            ?LOG(error, "Decode composite error: ~p", [Reason])
    end,
    TestImage = <<0,0,0,1, 16#67, 16#42, 16#C0, 16#1E>>,
    ImageFrame = encode_image(?DATA_TYPE_VISIBLE_IMG, 1, TestImage),
    ?LOG(info, "Image frame size: ~p bytes", [byte_size(ImageFrame)]),
    case decode_image(ImageFrame) of
        {ok, DataType, FrameNumber, ImageData} ->
            ?LOG(info, "Decoded image: type=~s, frame=~p, size=~p",
                 [get_data_type_name(DataType), FrameNumber, byte_size(ImageData)]);
        {error, Reason2} ->
            ?LOG(error, "Decode image error: ~p", [Reason2])
    end,
    ok.