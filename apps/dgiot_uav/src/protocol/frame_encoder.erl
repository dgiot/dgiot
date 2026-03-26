%%%-------------------------------------------------------------------
%%% @doc
%%% frame_encoder.erl - UAV协议帧编码器（框架层）
%%%
%%% 本模块负责UAV协议帧的框架层编码，根据上层提供的参数构建完整的协议帧。
%%% 只处理协议帧的通用结构，不涉及具体的业务逻辑编码。
%%%
%%% 主要功能：
%%% 1. 帧编码：根据参数构建完整的协议帧。
%%% 2. 帧头构建：构建协议帧头部。
%%% 3. 载荷附加：将业务载荷附加到帧中。
%%% 4. CRC计算：计算并添加CRC校验。
%%%
%%% 协议对应：通用帧格式（同步头 EB 90 + 地址 + 数据类型 + 帧计数 + 密钥 + CRC + 载荷）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(frame_encoder).

-export([
    encode_frame/1,
    build_frame_header/1,
    calculate_crc/1,
    attach_payload/2,
    get_default_params/0
]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/types.hrl").

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 编码完整的协议帧
%% 参数可以是 #encode_params 记录或包含相同字段的 map。
-spec encode_frame(#encode_params{} | map()) -> {ok, binary()} | {error, term()}.
encode_frame(Params) when is_map(Params) ->
    try
        EncodeParams = map_to_encode_params(Params),
        encode_frame(EncodeParams)
    catch
        _:Exception ->
            {error, {params_conversion_error, Exception}}
    end;

encode_frame(#encode_params{
        dest_addr = _DestAddr,
        src_addr = _SrcAddr,
        platform_type = _PlatformType,
        data_type_low = _DataTypeLow,
        frame_count = _FrameCount,
        key = _Key,
        payload = Payload
    } = Params) ->
    try
        % 构建帧头
        Header = build_frame_header(Params),
        
        % 组装不含CRC的帧
        FrameWithoutCRC = attach_payload(Header, Payload),
        
        % 计算CRC
        CRC = calculate_crc(FrameWithoutCRC),
        
        % 构建完整帧
        FullFrame = <<FrameWithoutCRC/binary, CRC:16/little>>,
        {ok, FullFrame}
    catch
        _:Exception ->
            {error, {encoding_exception, Exception}}
    end;

encode_frame(_) ->
    {error, invalid_params_type}.

%% @doc 构建帧头部（不含CRC）
-spec build_frame_header(#encode_params{}) -> binary().
build_frame_header(#encode_params{
        dest_addr = DestAddr,
        src_addr = SrcAddr,
        platform_type = PlatformType,
        data_type_low = DataTypeLow,
        frame_count = FrameCount,
        key = Key
    }) ->
    % 数据类型字节：高4位=平台类型，低4位=数据类型
    DataType = ((PlatformType band 16#0F) bsl 4) bor (DataTypeLow band 16#0F),
    
    <<?SYNC_HEADER:16, DestAddr:16, SrcAddr:16, DataType:8, FrameCount:8, Key:24>>.

%% @doc 计算帧的CRC（输入为不含CRC的完整帧）
-spec calculate_crc(binary()) -> integer().
calculate_crc(FrameWithoutCRC) when is_binary(FrameWithoutCRC) ->
    uav_payload_checksum:calculate_crc16(FrameWithoutCRC).

%% @doc 将载荷附加到头部
-spec attach_payload(binary(), binary()) -> binary().
attach_payload(Header, Payload) when is_binary(Header), is_binary(Payload) ->
    <<Header/binary, Payload/binary>>.

%% @doc 获取默认编码参数
-spec get_default_params() -> #encode_params{}.
get_default_params() ->
    #encode_params{
        dest_addr = ?DEFAULT_DEST_ADDR,
        src_addr = ?DEFAULT_SRC_ADDR,
        platform_type = ?PLATFORM_206,
        data_type_low = ?TELEMETRY_DATA_TYPE_LOW,
        frame_count = 1,
        key = 0,
        payload = <<>>,
        timestamp = erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================

%% @private 将映射转换为编码参数记录
-spec map_to_encode_params(map()) -> #encode_params{}.
map_to_encode_params(Map) ->
    #encode_params{
        dest_addr = maps:get(dest_addr, Map, ?DEFAULT_DEST_ADDR),
        src_addr = maps:get(src_addr, Map, ?DEFAULT_SRC_ADDR),
        platform_type = maps:get(platform_type, Map, ?PLATFORM_206),
        data_type_low = maps:get(data_type_low, Map, ?TELEMETRY_DATA_TYPE_LOW),
        frame_count = maps:get(frame_count, Map, 1),
        key = maps:get(key, Map, 0),
        payload = maps:get(payload, Map, <<>>),
        timestamp = maps:get(timestamp, Map, erlang:system_time(millisecond))
    }.

%%%===================================================================
%%% 单元测试（可选）
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Params = #encode_params{
        dest_addr = 16#0024,
        src_addr = 16#0000,
        platform_type = 16#01,
        data_type_low = 16#0F,
        frame_count = 1,
        key = 16#123456,
        payload = <<"test">>
    },
    {ok, Frame} = encode_frame(Params),
    ?assert(byte_size(Frame) >= ?MIN_FRAME_SIZE + 4),
    <<?SYNC_HEADER:16, _/binary>> = Frame,
    ok.

-endif.