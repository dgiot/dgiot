%%%-------------------------------------------------------------------
%%% @doc
%%% frame_decoder.erl - UAV协议帧解码器（框架层）
%%%
%%% 本模块负责UAV协议帧的框架层解码，只处理协议帧的通用结构，
%%% 不涉及具体的业务逻辑解析。业务层解析由专门的业务模块处理。
%%%
%%% 主要功能：
%%% 1. 单帧解码：解析单个协议帧。
%%% 2. 多帧解码：解析连续的多帧数据。
%%% 3. 帧验证：验证帧的完整性和CRC校验。
%%% 4. 缓冲区管理：处理不完整帧。
%%%
%%% 协议对应：通用帧格式（同步头 EB 90 + 地址 + 数据类型 + 帧计数 + 密钥 + CRC + 载荷）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(frame_decoder).

-export([
    decode_frame/1,
    decode_frames/1,
    validate_frame/1,
    get_frame_info/1,
    extract_payload/1
]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/types.hrl").

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 解码单个协议帧
%% 返回 {ok, #uav_frame{}, 剩余数据} | {incomplete, 原始缓冲区} | {error, Reason}
-spec decode_frame(binary()) -> {ok, #uav_frame{}, binary()} | {incomplete, binary()} | {error, term()}.
decode_frame(<<?SYNC_HEADER:16, DestAddr:16, SrcAddr:16, DataType:8,
               FrameCount:8, Key:24, ReceivedCRC:16, Payload/binary>> = Buffer) ->
    
    PayloadSize = byte_size(Payload),
    FrameSize = ?MIN_FRAME_SIZE + PayloadSize,   % 总帧大小 = 最小帧 + 载荷
    
    case byte_size(Buffer) >= FrameSize of
        true ->
            FrameRecord = #uav_frame{
                sync_header = ?SYNC_HEADER,
                dest_addr = DestAddr,
                src_addr = SrcAddr,
                data_type = DataType,
                frame_count = FrameCount,
                key = Key,
                crc = ReceivedCRC,
                payload = Payload,
                frame_size = FrameSize,
                timestamp = erlang:system_time(millisecond),
                frame_type = identify_frame_type(DataType)
            },
            % 分离剩余数据
            <<_:FrameSize/binary, Rest/binary>> = Buffer,
            {ok, FrameRecord, Rest};
        false ->
            {incomplete, Buffer}
    end;

decode_frame(Buffer) when byte_size(Buffer) < ?MIN_FRAME_SIZE ->
    {incomplete, Buffer};

decode_frame(_Buffer) ->
    {error, invalid_sync_header}.

%% @doc 解码多个协议帧（尾递归）
%% 返回 {ok, 帧列表, 剩余数据}
-spec decode_frames(binary()) -> {ok, [#uav_frame{}], binary()}.
decode_frames(Binary) ->
    decode_frames(Binary, []).

decode_frames(Buffer, Acc) ->
    case decode_frame(Buffer) of
        {ok, Frame, Rest} ->
            decode_frames(Rest, [Frame | Acc]);
        {incomplete, _IncompleteBuffer} ->
            {ok, lists:reverse(Acc), Buffer};  % 返回当前缓冲区
        {error, _Reason} ->
            % 跳过1字节继续尝试
            case Buffer of
                <<_, Rest/binary>> ->
                    ?LOG(warning, "解码错误，跳过1字节继续", []),
                    decode_frames(Rest, Acc);
                <<>> ->
                    {ok, lists:reverse(Acc), <<>>}
            end
    end.

%% @doc 验证帧（CRC校验）
-spec validate_frame(#uav_frame{}) -> {ok, #uav_frame{}} | {error, term()}.
validate_frame(#uav_frame{
        dest_addr = DestAddr,
        src_addr = SrcAddr,
        data_type = DataType,
        frame_count = FrameCount,
        key = Key,
        crc = ReceivedCRC,
        payload = Payload
    } = Frame) ->
    
    % 计算CRC的数据部分：从目标地址到载荷结束（不含同步头）
    CRCData = <<DestAddr:16, SrcAddr:16, DataType:8, FrameCount:8, Key:24, Payload/binary>>,
    CalculatedCRC = uav_payload_checksum:calculate_crc16(CRCData),
    
    case ReceivedCRC =:= CalculatedCRC of
        true ->
            {ok, Frame};
        false ->
            {error, {crc_mismatch, ReceivedCRC, CalculatedCRC}}
    end.

%% @doc 获取帧信息（用于日志、调试）
-spec get_frame_info(#uav_frame{}) -> map().
get_frame_info(#uav_frame{
        dest_addr = DestAddr,
        src_addr = SrcAddr,
        data_type = DataType,
        frame_count = FrameCount,
        frame_size = FrameSize,
        payload = Payload,
        timestamp = Timestamp,
        frame_type = FrameType
    }) ->
    #{
        dest_addr => DestAddr,
        src_addr => SrcAddr,
        data_type => DataType,
        frame_type => FrameType,
        frame_count => FrameCount,
        frame_size => FrameSize,
        payload_size => byte_size(Payload),
        timestamp => Timestamp
    }.

%% @doc 提取载荷数据（并返回基本信息）
-spec extract_payload(#uav_frame{}) -> {ok, map()} | {error, term()}.
extract_payload(#uav_frame{payload = Payload} = Frame) ->
    % 这里只提取载荷，不解析业务数据
    {ok, #{
        payload => Payload,
        frame_info => get_frame_info(Frame)
    }}.

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================

%% @private 根据数据类型识别帧类型
identify_frame_type(DataType) ->
    DataTypeLow = DataType band 16#0F,
    case DataTypeLow of
        ?TELEMETRY_DATA_TYPE_LOW -> telemetry;
        ?CONTROL_DATA_TYPE_LOW -> control;
        ?PAYLOAD_DATA_TYPE_LOW -> payload;
        ?LINK_CONTROL_TYPE_LOW -> link_control;
        _ -> unknown
    end.