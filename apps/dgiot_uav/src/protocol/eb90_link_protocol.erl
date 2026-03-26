%%%-------------------------------------------------------------------
%%% @doc
%%% eb90_link_protocol.erl - EB90 链路层协议处理模块
%%%
%%% 负责 EB90 帧的提取、解析和构建，根据低四位数据类型区分：
%%% - 0x0: 遥控帧
%%% - 0x1: 飞控遥测帧（需进一步解析载荷）
%%% - 0xE: 链路遥测帧（包含入网申请、网络状态等）
%%% - 0xF: 扩展数据类型
%%%
%%% 协议对应：数据链协议.doc 中的上行/下行帧格式，
%%% 以及超近距无人机链路协议.docx 中的链路遥测帧格式。
%%%
%%% 主要功能：
%%% - parse_link_frame/1: 解析 EB90 帧，返回帧类型和解析后的映射
%%% - build_remote_control_frame/4: 构建遥控指令帧（66字节）
%%% - build_telemetry_frame/1: 构建遥测帧（简化版）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eb90_link_protocol).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/types.hrl").

-export([
    parse_link_frame/1,
    parse_telemetry_frame/1,
    parse_remote_control_frame/1,
    build_remote_control_frame/1,
    build_remote_control_frame/4,
    build_telemetry_frame/1
]).

%% 数据类型低四位宏已在 types.hrl 中定义，这里直接引用

%% @doc 解析链路帧（自动识别遥控帧或遥测帧）
%% 返回 {ok, FrameBinary, ParsedMap, Rest} | {error, term()} | {more, non_neg_integer()}
-spec parse_link_frame(binary()) -> {ok, binary(), map(), binary()} | {error, term()} | {more, non_neg_integer()}.
parse_link_frame(Data) when byte_size(Data) < 8 ->
    {more, 8 - byte_size(Data)};
parse_link_frame(<<?SYNC_HEADER:16, DestHigh:8, DestLow:8, SrcHigh:8, SrcLow:8,
                   PlatformType:8, FrameNo:8, Rest/binary>>) ->
    DestAddr = (DestHigh bsl 8) bor DestLow,
    SrcAddr = (SrcHigh bsl 8) bor SrcLow,
    DataTypeLow = PlatformType band 16#0F,
    FullHeader = <<?SYNC_HEADER:16, DestHigh:8, DestLow:8, SrcHigh:8, SrcLow:8,
                   PlatformType:8, FrameNo:8>>,
    
    % 根据数据类型估算帧长度，并检查是否有足够的数据
    {MinPayloadSize, _FrameTypeName} = case DataTypeLow of
        ?CONTROL_DATA_TYPE_LOW ->   % 遥控帧低四位为 0x0
            {58, remote_control};   % 66字节 - 8字节头部 = 58字节
        ?TELEMETRY_DATA_TYPE_LOW ->  % 飞控遥测低四位为 0x1
            {120, fc_telemetry};   % 128字节 - 8字节头部 = 120字节
        ?TELEMETRY_DATA_TYPE_LOW_ALT ->  % 飞控遥测低四位为 0xC
            {120, fc_telemetry_alt};
        ?LINK_CONTROL_TYPE_LOW ->    % 链路遥测低四位为 0xE
            {120, link_telemetry};
        ?EXTENDED_DATA_TYPE_LOW ->   % 扩展数据类型低四位为 0xF
            {5, extended};           % 至少需要5字节（3字节密钥 + 2字节CRC）
        _ ->
            {0, unknown}
    end,
    
    % 检查是否有足够的载荷数据
    case byte_size(Rest) >= MinPayloadSize of
        true ->
            case DataTypeLow of
                ?CONTROL_DATA_TYPE_LOW ->   % 遥控帧低四位为 0x0
                    parse_remote_control_frame(FullHeader, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
                ?TELEMETRY_DATA_TYPE_LOW ->  % 飞控遥测低四位为 0x1
                    parse_fc_telemetry_frame(FullHeader, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
                ?TELEMETRY_DATA_TYPE_LOW_ALT ->  % 飞控遥测低四位为 0xC
                    parse_fc_telemetry_frame(FullHeader, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
                ?LINK_CONTROL_TYPE_LOW ->    % 链路遥测低四位为 0xE
                    parse_link_telemetry_frame(FullHeader, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
                ?EXTENDED_DATA_TYPE_LOW ->   % 扩展数据类型低四位为 0xF
                    parse_extended_data_frame(FullHeader, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
                _ ->
                    ?LOG(warning, "[EB90] 未知数据类型: ~p (0x~.2B)，平台类型=~p，尝试通用解析", 
                         [DataTypeLow, DataTypeLow, PlatformType]),
                    % 通用解析：返回头部信息和原始载荷
                    ParsedMap = #{
                        frame_type => unknown,
                        dest_addr => DestAddr,
                        src_addr => SrcAddr,
                        platform_type => PlatformType,
                        frame_no => FrameNo,
                        data_type_low => DataTypeLow,
                        raw_payload => Rest,
                        raw_payload_length => byte_size(Rest)
                    },
                    FullFrame = <<FullHeader/binary, Rest/binary>>,
                    {ok, FullFrame, ParsedMap, <<>>}
            end;
        false ->
            % 数据不完整，返回需要更多字节
            {more, MinPayloadSize - byte_size(Rest)}
    end;
parse_link_frame(Data) ->
    % 查找同步头位置，跳过无效数据
    case find_sync_header(Data) of
        {ok, Offset} ->
            ?LOG(debug, "[EB90] 跳过 ~p 字节无效数据，找到同步头", [Offset]),
            NewData = binary:part(Data, Offset, byte_size(Data) - Offset),
            parse_link_frame(NewData);
        {error, not_found} ->
            {error, invalid_sync_word}
    end.

%% @doc 在二进制数据中查找EB90同步头
%% 返回 {ok, Offset} | {error, not_found}
-spec find_sync_header(binary()) -> {ok, non_neg_integer()} | {error, not_found}.
find_sync_header(Data) ->
    find_sync_header(Data, 0).

find_sync_header(<<>>, _Offset) ->
    {error, not_found};
find_sync_header(<<?SYNC_HEADER:16, _Rest/binary>>, Offset) ->
    {ok, Offset};
find_sync_header(<<_Byte, Rest/binary>>, Offset) ->
    find_sync_header(Rest, Offset + 1).

%% 解析遥控帧（66字节）
parse_remote_control_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    case byte_size(Rest) of
        Size when Size >= 58 ->  % 66 - 8 = 58
            <<Key:3/binary, Switch1:8, Switch2:8, Switch3:8,
              AdjustAddr1:8, AdjustAddr2:8, AdjustData1:8, AdjustData2:8,
              Padding:46/binary, CRC:16/little>> = Rest,
            DataForCRC = <<(DestAddr bsr 8):8, (DestAddr band 16#FF):8,
                           (SrcAddr bsr 8):8, (SrcAddr band 16#FF):8,
                           PlatformType:8, FrameNo:8, Key/binary,
                           Switch1:8, Switch2:8, Switch3:8,
                           AdjustAddr1:8, AdjustAddr2:8, AdjustData1:8, AdjustData2:8,
                           Padding/binary>>,
            ParsedMap = #{
                frame_type => remote_control,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                key => Key,
                switch_commands => parse_switch_commands(Switch1, Switch2, Switch3),
                adjust_command => parse_adjust_command(AdjustAddr1, AdjustAddr2, AdjustData1, AdjustData2),
                crc => CRC,
                crc_valid => (calculate_crc16(DataForCRC) =:= CRC)
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        _ ->
            {error, incomplete}
    end.

%% 解析飞控遥测帧（128字节）—— 只提取载荷部分，不解析具体内容
parse_fc_telemetry_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    case byte_size(Rest) of
        Size when Size >= 122 ->  % 3字节密钥 + 2字节CRC1 + 115字节载荷 + 2字节CRC2 = 122
            <<Key:3/binary, CRC1:16/big, Payload:115/binary, CRC2:16/big>> = Rest,
            DataForCRC = <<(DestAddr bsr 8):8, (DestAddr band 16#FF):8,
                           (SrcAddr bsr 8):8, (SrcAddr band 16#FF):8,
                           PlatformType:8, FrameNo:8, Key/binary>>,
            % 验证 CRC1 (从02H到0AH)
            Crc1Valid = (calculate_crc16(DataForCRC) =:= CRC1),
            % 验证 CRC2 (从0DH到7DH? 但这里只取到7DH？需要计算)
            % 暂时忽略 CRC2 验证
            ParsedMap = #{
                frame_type => fc_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                key => Key,
                crc1 => CRC1,
                payload => Payload,
                crc2 => CRC2,
                crc1_valid => Crc1Valid
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        Size when Size >= 120 ->
            % 可能缺少CRC2，只取前120字节
            <<Key:3/binary, CRC1:16/big, Payload:115/binary, _/binary>> = Rest,
            DataForCRC = <<(DestAddr bsr 8):8, (DestAddr band 16#FF):8,
                           (SrcAddr bsr 8):8, (SrcAddr band 16#FF):8,
                           PlatformType:8, FrameNo:8, Key/binary>>,
            Crc1Valid = (calculate_crc16(DataForCRC) =:= CRC1),
            ParsedMap = #{
                frame_type => fc_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                key => Key,
                crc1 => CRC1,
                payload => Payload,
                crc2 => 0,
                crc1_valid => Crc1Valid
            },
            FullFrame = <<Header/binary, (binary:part(Rest, 0, 120))/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        _ ->
            {error, incomplete}
    end.

%% 解析链路遥测帧（128字节）—— 按照表5-4解析
parse_link_telemetry_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    case byte_size(Rest) of
        Size when Size >= 120 ->  % 128 - 8 = 120
            <<Key:3/binary, CRC1:16/big, _Skip0:100/binary,  % 从0DH到70H共100字节（0DH-70H）
              NetworkApplyValid:8, NetworkApplyAddrHigh:8, NetworkApplyAddrLow:8,
              NetworkStatus:8,
              Online1High:8, Online1Low:8,
              Online2High:8, Online2Low:8,
              _Reserved:8,
              RejectAddrHigh:8, RejectAddrLow:8,
              CRC2:16/big, CRC3:16/big>> = Rest,
            
            NetworkApplyAddr = (NetworkApplyAddrHigh bsl 8) bor NetworkApplyAddrLow,
            AllowedCount = (NetworkStatus bsr 4) band 16#0F,
            RejectedCount = NetworkStatus band 16#0F,
            Online1 = (Online1High bsl 8) bor Online1Low,
            Online2 = (Online2High bsl 8) bor Online2Low,
            RejectAddr = (RejectAddrHigh bsl 8) bor RejectAddrLow,
            
            % 构建在线节点列表（可根据AllowedCount动态解析更多，但这里只取前两个）
            OnlineNodes = lists:filter(fun(X) -> X =/= 0 end, [Online1, Online2]),
            
            ParsedMap = #{
                frame_type => link_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                key => Key,
                crc1 => CRC1,
                network_apply => #{
                    valid => (NetworkApplyValid == 16#AA),
                    address => NetworkApplyAddr
                },
                network_status => #{
                    allowed_count => AllowedCount,
                    rejected_count => RejectedCount
                },
                online_nodes => OnlineNodes,
                reject_node => RejectAddr,
                crc2 => CRC2,
                crc3 => CRC3
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        _ ->
            {error, incomplete}
    end.

%% 构建遥控指令帧（66字节）
-spec build_remote_control_frame(DestAddr :: integer(), SrcAddr :: integer(),
                                 CmdCode :: integer(), Value :: integer()) -> binary() | {error, term()}.
build_remote_control_frame(DestAddr, SrcAddr, CmdCode, Value) ->
    case uav_protocol_utils:cmd_code_to_bytes(CmdCode) of
        {ok, {CmdH, _CmdL}} ->
            DestHigh = (DestAddr bsr 8) band 16#FF,
            DestLow = DestAddr band 16#FF,
            SrcHigh = (SrcAddr bsr 8) band 16#FF,
            SrcLow = SrcAddr band 16#FF,
            PlatformType = ?REMOTE_CONTROL_TYPE bor 0,
            FrameNo = 0,
            Header = <<?SYNC_HEADER:16, DestHigh:8, DestLow:8, SrcHigh:8, SrcLow:8,
                       PlatformType:8, FrameNo:8>>,
            Key = <<0,0,0>>,
            Switch1 = 0, Switch2 = 0, Switch3 = 0,
            AdjustAddr1 = CmdH,
            AdjustAddr2 = CmdH,
            AdjustData1 = (Value bsr 8) band 16#FF,
            AdjustData2 = Value band 16#FF,
            Padding = binary:copy(<<0>>, 46),
            DataForCRC = <<DestHigh:8, DestLow:8, SrcHigh:8, SrcLow:8,
                           PlatformType:8, FrameNo:8, Key/binary,
                           Switch1:8, Switch2:8, Switch3:8,
                           AdjustAddr1:8, AdjustAddr2:8, AdjustData1:8, AdjustData2:8,
                           Padding/binary>>,
            CRC = calculate_crc16(DataForCRC),
            <<Header/binary, Key/binary,
              Switch1:8, Switch2:8, Switch3:8,
              AdjustAddr1:8, AdjustAddr2:8, AdjustData1:8, AdjustData2:8,
              Padding/binary, CRC:16/little>>;
        {error, Reason} ->
            {error, Reason}
    end.

%% 解析开关指令（三个字节）
parse_switch_commands(S1, S2, S3) ->
    Power = case (S1 band 1) of 1 -> on; 0 -> off end,
    Video = case (S1 band 2) of 2 -> on; 0 -> off end,
    Datalink = case (S2 band 1) of 1 -> on; 0 -> off end,
    Navigation = case (S2 band 2) of 2 -> on; 0 -> off end,
    Communication = case (S3 band 1) of 1 -> on; 0 -> off end,
    Mission = case (S3 band 2) of 2 -> on; 0 -> off end,
    Emergency = case (S3 band 4) of 4 -> true; 0 -> false end,
    [{power, Power}, {video, Video}, {datalink, Datalink},
     {navigation, Navigation}, {communication, Communication},
     {mission, Mission}, {emergency, Emergency}].

%% 解析遥调指令（二判二）
parse_adjust_command(Addr1, Addr2, Data1, Data2) ->
    case Addr1 =:= Addr2 of
        true ->
            Value = (Data1 bsl 8) bor Data2,
            case Addr1 of
                16#80 -> {altitude, Value};
                16#81 -> {speed, Value};
                16#82 -> {heading, Value};
                16#85 -> {pitch, Value};
                16#86 -> {roll, Value};
                16#9E -> {network_allow, Value};
                16#9F -> {network_reject, Value};
                _ -> {unknown, Addr1, Value}
            end;
        false ->
            {error, address_mismatch}
    end.

%% 构建遥控指令帧（单参数版本，接收map）
-spec build_remote_control_frame(Params :: map()) -> binary() | {error, term()}.
build_remote_control_frame(Params) ->
    %% 从参数中提取必要字段
    DestAddr = maps:get(dest_addr, Params, 16#0000),
    SrcAddr = maps:get(src_addr, Params, 16#0001),
    _FrameNo = maps:get(frame_no, Params, 1),
    
    %% 提取测试项信息（用于闭环跟踪）
    CommandId = maps:get(command_id, Params, undefined),
    TestItemId = maps:get(test_item_id, Params, undefined),
    StepIndex = maps:get(step_index, Params, undefined),
    StationId = maps:get(station_id, Params, 0),
    
    %% 记录命令映射信息（用于地测口闭环跟踪）
    case CommandId of
        undefined -> ok;
        _ ->
            ?LOG(info, "【EB90协议】命令映射: CommandId=~p, TestItemId=~p, StepIndex=~p, StationId=~p", 
                 [CommandId, TestItemId, StepIndex, StationId])
    end,
    
    %% 根据 adjust_command 生成对应的命令码
    case maps:get(adjust_command, Params, {network_allow, 0}) of
        {network_allow, DeviceAddr} ->
            %% 入网许可命令
            CmdCode = 16#9E,  %% 网络允许命令码
            Value = DeviceAddr;
        {network_reject, Reason} ->
            %% 入网拒绝命令
            CmdCode = 16#9F,  %% 网络拒绝命令码
            Value = Reason;
        {Cmd, CmdValue} ->
            %% 其他调节命令
            CmdCode = cmd_to_code(Cmd),
            Value = CmdValue;
        _ ->
            %% 默认命令
            CmdCode = 16#00,
            Value = 0
    end,
    
    %% 调用四参数版本
    build_remote_control_frame(DestAddr, SrcAddr, CmdCode, Value).

%% 命令到命令码的映射
cmd_to_code(altitude) -> 16#80;
cmd_to_code(speed) -> 16#81;
cmd_to_code(heading) -> 16#82;
cmd_to_code(pitch) -> 16#85;
cmd_to_code(roll) -> 16#86;
cmd_to_code(network_allow) -> 16#9E;
cmd_to_code(network_reject) -> 16#9F;
cmd_to_code(_) -> 16#00.

%% 构建遥测帧（简化，用于测试）
build_telemetry_frame(_Params) ->
    <<>>.

%% 解析扩展数据帧（数据类型 0x0F）
parse_extended_data_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    ?LOG(debug, "[EB90] 解析扩展数据帧，平台类型=~p (高4位=~p，低4位=~p)，剩余长度=~p",
         [PlatformType, (PlatformType bsr 4) band 16#0F, PlatformType band 16#0F,
          byte_size(Rest)]),
    
    % 根据平台类型的高4位确定扩展子类型
    ExtendedType = (PlatformType bsr 4) band 16#0F,
    
    case ExtendedType of
        0 ->  % 类型0：标准扩展数据
            parse_standard_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
        1 ->  % 类型1：加密扩展数据
            parse_encrypted_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
        2 ->  % 类型2：压缩扩展数据
            parse_compressed_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
        3 ->  % 类型3：大文件扩展数据
            parse_largefile_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest);
        _ ->  % 其他类型：通用解析
            parse_generic_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest)
    end.

%% 解析标准扩展数据帧
parse_standard_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    case byte_size(Rest) of
        Size when Size >= 5 ->  % 至少需要3字节密钥 + 2字节CRC
            <<Key:3/binary, CRC1:16/big, Payload/binary>> = Rest,
            DataForCRC = <<(DestAddr bsr 8):8, (DestAddr band 16#FF):8,
                           (SrcAddr bsr 8):8, (SrcAddr band 16#FF):8,
                           PlatformType:8, FrameNo:8, Key/binary>>,
            Crc1Valid = (calculate_crc16(DataForCRC) =:= CRC1),
            
            ParsedMap = #{
                frame_type => extended,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                key => Key,
                crc1 => CRC1,
                payload => Payload,
                crc1_valid => Crc1Valid,
                data_type => extended,
                extended_type => 0,
                extended_subtype => standard
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        _ ->
            ?LOG(warning, "[EB90] 标准扩展数据帧长度不足: ~p字节，需要至少5字节", [byte_size(Rest)]),
            {error, {incomplete_standard_extended, byte_size(Rest)}}
    end.

%% 解析加密扩展数据帧
parse_encrypted_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    ?LOG(debug, "[EB90] 解析加密扩展数据帧，长度=~p", [byte_size(Rest)]),
    
    % 加密数据帧格式：加密算法(1字节) + 初始化向量(16字节) + 加密数据
    case byte_size(Rest) of
        Size when Size >= 17 ->
            <<Algorithm:8, IV:16/binary, EncryptedData/binary>> = Rest,
            
            ParsedMap = #{
                frame_type => extended,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                algorithm => Algorithm,
                iv => IV,
                encrypted_data => EncryptedData,
                encrypted_length => byte_size(EncryptedData),
                data_type => extended,
                extended_type => 1,
                extended_subtype => encrypted
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        _ ->
            ?LOG(warning, "[EB90] 加密扩展数据帧长度不足: ~p字节，需要至少17字节", [byte_size(Rest)]),
            {error, {incomplete_encrypted_extended, byte_size(Rest)}}
    end.

%% 解析压缩扩展数据帧
parse_compressed_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    ?LOG(info, "[EB90] 解析压缩扩展数据帧，长度=~p", [byte_size(Rest)]),
    
    % 压缩数据帧格式：压缩算法(1字节) + 原始长度(4字节) + 压缩数据
    case byte_size(Rest) of
        Size when Size >= 5 ->
            <<Algorithm:8, OriginalSize:32/big, CompressedData/binary>> = Rest,
            
            ParsedMap = #{
                frame_type => extended,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                compression_algorithm => Algorithm,
                original_size => OriginalSize,
                compressed_data => CompressedData,
                compressed_length => byte_size(CompressedData),
                compression_ratio => case OriginalSize of
                                        0 -> 0.0;
                                        _ -> byte_size(CompressedData) / OriginalSize
                                    end,
                data_type => extended,
                extended_type => 2,
                extended_subtype => compressed
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        _ ->
            ?LOG(warning, "[EB90] 压缩扩展数据帧长度不足: ~p字节，需要至少5字节", [byte_size(Rest)]),
            {error, {incomplete_compressed_extended, byte_size(Rest)}}
    end.

%% 解析大文件扩展数据帧
parse_largefile_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    ?LOG(info, "[EB90] 解析大文件扩展数据帧，长度=~p", [byte_size(Rest)]),
    
    % 大文件数据帧格式：文件ID(4字节) + 分片序号(4字节) + 总分片数(4字节) + 文件数据
    case byte_size(Rest) of
        Size when Size >= 12 ->
            <<FileId:32/big, ChunkIndex:32/big, TotalChunks:32/big, FileData/binary>> = Rest,
            
            ParsedMap = #{
                frame_type => extended,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                file_id => FileId,
                chunk_index => ChunkIndex,
                total_chunks => TotalChunks,
                file_data => FileData,
                file_data_length => byte_size(FileData),
                data_type => extended,
                extended_type => 3,
                extended_subtype => largefile
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>};
        _ ->
            ?LOG(warning, "[EB90] 大文件扩展数据帧长度不足: ~p字节，需要至少12字节", [byte_size(Rest)]),
            {error, {incomplete_largefile_extended, byte_size(Rest)}}
    end.

%% 解析通用扩展数据帧（兼容性处理）
parse_generic_extended_frame(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    ?LOG(info, "[EB90] 解析通用扩展数据帧，尝试多种格式，长度=~p", [byte_size(Rest)]),
    
    % 尝试多种格式解析
    Results = [
        try_parse_as_standard_extended(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest),
        try_parse_as_simple_extended(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest),
        try_parse_as_length_prefixed(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest)
    ],
    
    % 选择第一个成功的解析结果
    SuccessfulResults = lists:filter(fun({Status, _}) -> Status =:= ok end, Results),
    case SuccessfulResults of
        [{ok, Result} | _] ->
            ?LOG(info, "[EB90] 通用扩展数据帧解析成功，使用格式: ~p", [maps:get(extended_subtype, Result)]),
            Result;
        [] ->
            % 所有格式都失败，返回基本解析结果
            ?LOG(warning, "[EB90] 所有扩展数据帧格式解析失败，返回基本解析"),
            ParsedMap = #{
                frame_type => extended,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                raw_payload => Rest,
                raw_payload_length => byte_size(Rest),
                data_type => extended,
                extended_type => (PlatformType bsr 4) band 16#0F,
                extended_subtype => unknown,
                parse_status => partial
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, FullFrame, ParsedMap, <<>>}
    end.

%% 尝试解析为标准扩展数据帧格式
try_parse_as_standard_extended(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    case byte_size(Rest) >= 5 of
        true ->
            <<Key:3/binary, CRC1:16/big, Payload/binary>> = Rest,
            DataForCRC = <<(DestAddr bsr 8):8, (DestAddr band 16#FF):8,
                           (SrcAddr bsr 8):8, (SrcAddr band 16#FF):8,
                           PlatformType:8, FrameNo:8, Key/binary>>,
            Crc1Valid = (calculate_crc16(DataForCRC) =:= CRC1),
            
            ParsedMap = #{
                frame_type => extended,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                platform_type => PlatformType,
                frame_no => FrameNo,
                key => Key,
                crc1 => CRC1,
                payload => Payload,
                crc1_valid => Crc1Valid,
                data_type => extended,
                extended_type => (PlatformType bsr 4) band 16#0F,
                extended_subtype => standard
            },
            FullFrame = <<Header/binary, Rest/binary>>,
            {ok, {ok, FullFrame, ParsedMap, <<>>}};
        false ->
            {error, insufficient_length}
    end.

%% 尝试解析为简单扩展数据帧格式
try_parse_as_simple_extended(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    ParsedMap = #{
        frame_type => extended,
        dest_addr => DestAddr,
        src_addr => SrcAddr,
        platform_type => PlatformType,
        frame_no => FrameNo,
        payload => Rest,
        payload_length => byte_size(Rest),
        data_type => extended,
        extended_type => (PlatformType bsr 4) band 16#0F,
        extended_subtype => simple
    },
    FullFrame = <<Header/binary, Rest/binary>>,
    {ok, {ok, FullFrame, ParsedMap, <<>>}}.

%% 尝试解析为带长度前缀的扩展数据帧格式
try_parse_as_length_prefixed(Header, DestAddr, SrcAddr, FrameNo, PlatformType, Rest) ->
    case byte_size(Rest) >= 2 of
        true ->
            <<Length:16/big, Payload/binary>> = Rest,
            case Length =:= byte_size(Payload) of
                true ->
                    ParsedMap = #{
                        frame_type => extended,
                        dest_addr => DestAddr,
                        src_addr => SrcAddr,
                        platform_type => PlatformType,
                        frame_no => FrameNo,
                        length_field => Length,
                        payload => Payload,
                        payload_length => Length,
                        data_type => extended,
                        extended_type => (PlatformType bsr 4) band 16#0F,
                        extended_subtype => length_prefixed
                    },
                    FullFrame = <<Header/binary, Rest/binary>>,
                    {ok, {ok, FullFrame, ParsedMap, <<>>}};
                false ->
                    {error, length_mismatch}
            end;
        false ->
            {error, insufficient_length}
    end.

%% CRC16 计算（多项式 0x8005，初始 0xFFFF，小端输出）
calculate_crc16(Data) ->
    calculate_crc16(Data, 16#FFFF).

calculate_crc16(<<>>, Crc) ->
    Crc;
calculate_crc16(<<Byte, Rest/binary>>, Crc) ->
    Crc1 = Crc bxor Byte,
    Crc2 = crc16_byte(Crc1, 0),
    calculate_crc16(Rest, Crc2).

crc16_byte(_, 8) -> 0;
crc16_byte(Value, Bit) ->
    case (Value band 1) of
        1 -> crc16_byte((Value bsr 1) bxor 16#A001, Bit + 1);
        0 -> crc16_byte(Value bsr 1, Bit + 1)
    end.

%% @doc 解析遥测帧（兼容旧接口）
%% 返回 {ok, TelemetryMap} | {error, term()}
-spec parse_telemetry_frame(binary()) -> {ok, map()} | {error, term()}.
parse_telemetry_frame(Packet) ->
    case parse_link_frame(Packet) of
        {ok, _FrameBinary, ParsedMap, _Rest} ->
            {ok, ParsedMap};
        {error, Reason} ->
            {error, Reason};
        {more, _Required} ->
            {error, incomplete}
    end.

%% @doc 解析遥控帧（兼容旧接口）
%% 返回 {ok, ControlMap} | {error, term()}
-spec parse_remote_control_frame(binary()) -> {ok, map()} | {error, term()}.
parse_remote_control_frame(Packet) ->
    case parse_link_frame(Packet) of
        {ok, _FrameBinary, ParsedMap, _Rest} ->
            {ok, ParsedMap};
        {error, Reason} ->
            {error, Reason};
        {more, _Required} ->
            {error, incomplete}
    end.
