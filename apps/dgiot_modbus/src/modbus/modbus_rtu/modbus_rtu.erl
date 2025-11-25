%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(modbus_rtu).
-author("jonhl").

-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/logger.hrl").

%% @doc Modbus RTU协议处理主模块
%% 负责Modbus RTU协议的核心流程控制，协调编码器、解码器和工具模块
%% 支持多种功能码和数据格式，包括线圈、离散输入、保持寄存器、输入寄存器等
-export([
    init/1,
    dealwith/1,
    parse_frame/3,
    to_frame/1,
    encode_data/5,
    modbus_encoder/4, 
    modbus_decoder/5, 
    set_params/3, 
    decode_data/5, 
    get_datasource/1,
    test_simulated_data_block_split/0,
    is16/1
]).

%% 定义协议类型常量
-define(TYPE, ?MODBUS_RTU).

%% @doc 注册Modbus RTU协议参数
%% 定义协议配置所需的参数结构，用于前端界面展示和参数验证
-params(#{
    <<"slaveid">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"0000"/utf8>>,
        title => #{
            zh => <<"从机地址"/utf8>>
        },
        description => #{
            zh => <<"从机地址(16进制加0X,例如:0X10,否在是10进制),范围1-247,一个字节"/utf8>>
        }
    },
    <<"operatetype">> => #{
        order => 2,
        type => string,
        required => true,
        default => #{<<"value">> => <<"readCoils">>, <<"label">> => <<"0X01:读线圈寄存器"/utf8>>},
        enum => [
            #{<<"value">> => <<"readCoils">>, <<"label">> => <<"0X01:读线圈寄存器"/utf8>>},
            #{<<"value">> => <<"readInputs">>, <<"label">> => <<"0X02:读离散输入寄存器"/utf8>>},
            #{<<"value">> => <<"readHregs">>, <<"label">> => <<"0X03:读保持寄存器"/utf8>>},
            #{<<"value">> => <<"readIregs">>, <<"label">> => <<"0X04:读输入寄存器"/utf8>>},
            #{<<"value">> => <<"writeCoil">>, <<"label">> => <<"0X05:写单个线圈寄存器"/utf8>>},
            #{<<"value">> => <<"writeHreg">>, <<"label">> => <<"0X06:写单个保持寄存器"/utf8>>},
            #{<<"value">> => <<"writeCoils">>, <<"label">> => <<"0X0f:写多个线圈寄存器"/utf8>>},
            #{<<"value">> => <<"writeHregs">>, <<"label">> => <<"0X10:写多个保持寄存器"/utf8>>}
        ],
        title => #{
            zh => <<"寄存器功能码"/utf8>>
        },
        description => #{
            zh => <<"寄存器功能码"/utf8>>
        }
    },
    <<"address">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"0X00"/utf8>>,
        title => #{
            zh => <<"寄存器起始地址"/utf8>>
        },
        description => #{
            zh => <<"寄存器起始地址:原数据地址(16进制加0X,例如:0X10,否在是10进制);8位寄存器,一个字节;16位寄存器,两个字节;32位寄存器,四个字节"/utf8>>
        }
    },
    <<"registersnumber">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"1">>,
        title => #{
            zh => <<"寄存器个数"/utf8>>
        },
        description => #{
            zh => <<"寄存器个数(多个寄存器个数)"/utf8>>
        }
    },
    <<"originaltype">> => #{
        order => 5,
        type => string,
        required => true,
        default => #{<<"value">> => <<"raw">>, <<"label">> => <<"原始值"/utf8>>},
        enum => [
            #{<<"value">> => <<"raw">>, <<"label">> => <<"原始值"/utf8>>},
            #{<<"value">> => <<"bit">>, <<"label">> => <<"位"/utf8>>},
            #{<<"value">> => <<"short16_AB">>, <<"label">> => <<"16位 有符号(AB)"/utf8>>},
            #{<<"value">> => <<"short16_BA">>, <<"label">> => <<"16位 有符号(BA)"/utf8>>},
            #{<<"value">> => <<"ushort16_AB">>, <<"label">> => <<"16位 无符号(AB)"/utf8>>},
            #{<<"value">> => <<"ushort16_BA">>, <<"label">> => <<"16位 无符号(BA)"/utf8>>},
            #{<<"value">> => <<"long32_ABCD">>, <<"label">> => <<"32位 有符号(ABCD)"/utf8>>},
            #{<<"value">> => <<"long32_CDAB">>, <<"label">> => <<"32位 有符号(CDAB)"/utf8>>},
            #{<<"value">> => <<"ulong32_ABCD">>, <<"label">> => <<"32位 无符号(ABCD)"/utf8>>},
            #{<<"value">> => <<"ulong32_CDAB">>, <<"label">> => <<"32位 无符号(CDAB)"/utf8>>},
            #{<<"value">> => <<"float32_ABCD">>, <<"label">> => <<"32位 浮点数(ABCD)"/utf8>>},
            #{<<"value">> => <<"float32_CDAB">>, <<"label">> => <<"32位 浮点数(CDAB)"/utf8>>}
        ],
        title => #{
            zh => <<"数据格式"/utf8>>
        },
        description => #{
            zh => <<"数据格式：原始值直接返回二进制数据，位数据按位解析，其他格式按指定字节序解析"/utf8>>
        }
    }
}).

%% @doc 注册Modbus RTU协议类型
%% 定义协议在系统中的类型标识和显示信息
-protocol_type(#{
    cType => ?TYPE,
    type => <<"energy">>,
    colum => 10,
    title => #{
        zh => <<"MODBUS RTU协议"/utf8>>
    },
    description => #{
        zh => <<"MODBUS RTU协议"/utf8>>
    }
}).

%% @doc 初始化Modbus RTU协议状态
%% 设置初始状态，包括请求列表、时间戳和轮询间隔
%% State: 初始状态
%% 返回: 包含默认配置的状态映射
init(State) ->
    io:format("~s ~p Modbus RTU协议初始化完成~n", [?FILE, ?LINE]),
    State#{<<"req">> => [], <<"ts">> => dgiot_datetime:now_ms(), <<"interval">> => 300}.
%%
%% 登录功能（已注释，保留供参考）
%%login(#{<<"devaddr">> := DTUAddr, <<"product">> := ProductId, <<"ip">> := Ip,
%%    <<"channelId">> := ChannelId} = State) ->
%%    Topic = <<ProductId/binary, "/", ChannelId/binary, "/", DTUAddr/binary>>,
%%    dgiot_mqtt:subscribe(Topic),
%%    dgiot_device:register(ProductId, DTUAddr, ChannelId, #{<<"ip">> => Ip}),
%%    {ok, State};
%%
%%login(State) ->
%%    {ok, State}.

%% @doc 构建Modbus RTU请求帧
%% 根据配置参数生成Modbus RTU协议请求帧
%% 参数: 包含寄存器数量、从机地址、操作类型、数据格式、寄存器地址的映射
%% 返回: 编码后的Modbus RTU请求帧
to_frame(#{
    <<"registersnumber">> := Quality,
    <<"slaveid">> := SlaveId,
    <<"operatetype">> := Operatetype,
    <<"originaltype">> := Originaltype,
    <<"address">> := Address
}) ->
    modbus_rtu_encoder:encode_data(Quality, Address, SlaveId, Operatetype, Originaltype);

%% @doc 构建子设备Modbus RTU请求帧
%% 针对子设备构建请求帧，通过网关设备获取子设备信息
%% 参数: 包含寄存器数量、网关地址、从机地址、数据格式、寄存器地址的映射
%% 返回: 编码后的Modbus RTU请求帧或空列表
to_frame(#{
    <<"registersnumber">> := Quality,
    <<"gateway">> := DtuAddr,
    <<"slaveid">> := SlaveId,
    <<"originaltype">> := Originaltype,
    <<"address">> := Address
}) ->
    case dgiot_device:get_subdevice(DtuAddr, SlaveId) of
        not_find -> 
            io:format("~s ~p Subdevice not found for DtuAddr: ~p, SlaveId: ~p~n", [?FILE, ?LINE, DtuAddr, SlaveId]),
            [];
        [_ProductId, _DevAddr] ->
            % 为子设备使用默认的读保持寄存器操作类型
            modbus_rtu_encoder:encode_data(Quality, Address, SlaveId, <<"readHregs">>, Originaltype)
    end.

%% Quality 读的时候代表寄存器个数，16位的寄存器，一个寄存器表示两个字节，写的时候代表实际下发值
encode_data(Quality, Address, SlaveId, OperateType, Originaltype) ->
    modbus_rtu_encoder:encode_data(Quality, Address, SlaveId, OperateType, Originaltype).

%% @doc 设置参数并构建Modbus RTU请求消息
%% 参数: Payload - 负载数据，ProductId - 产品ID，DevAddr - 设备地址
%% 返回: 构建的请求消息列表
set_params(Payload, _ProductId, _DevAddr) ->
    try
        maps:fold(fun(_, Data, Acc) ->
            process_single_param(Data, Acc)
        end, [], Payload)
    catch
        _:Error ->
            io:format("~s ~p Error in set_params: ~p, Payload: ~p~n", [?FILE, ?LINE, Error, Payload]),
            []
    end.

%% @doc 处理单个参数配置
process_single_param(#{<<"dataForm">> := #{<<"protocol">> := <<"MODBUSRTU">>, <<"control">> := Setting},
                     <<"dataSource">> := #{<<"slaveid">> := SlaveId, <<"address">> := Address,
                                          <<"originaltype">> := Originaltype, <<"operatetype">> := OperateType} = DataSource} = Data, Acc) ->
    case maps:find(<<"value">>, Data) of
        {ok, Value} when byte_size(Value) > 0 ->
            build_rtu_request(Value, Setting, SlaveId, Address, OperateType, Originaltype, DataSource, Acc);
        _ ->
            Acc
    end;
process_single_param(_, Acc) ->
    Acc.

%% @doc 构建RTU请求
build_rtu_request(Value, Setting, SlaveId, Address, OperateType, Originaltype, DataSource, Acc) ->
    FunCode = modbus_rtu_encoder:get_funcode(OperateType),
    AddressHex = is16(Address),
    SlaveIdHex = is16(SlaveId),
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(AddressHex),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(SlaveIdHex),
    
    Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
    Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>)),
    
    Registersnumber = maps:get(<<"registersnumber">>, DataSource, <<"1">>),
    Bytes = modbus_rtu_utils:get_len(Registersnumber, Originaltype),
    
    RtuReq = #rtu_req{
        slaveId = Sh * 256 + Sl,
        funcode = dgiot_utils:to_int(FunCode),
        address = H * 256 + L,
        registersnumber = dgiot_utils:to_int(Registersnumber),
        dataByteSize = dgiot_utils:to_int(Bytes),
        quality = Value1
    },
    Acc ++ [modbus_rtu_encoder:build_req_message(RtuReq)].

%%%% @doc 获取功能码 - 已迁移到 modbus_rtu_encoder 模块
%%%% 使用 modbus_rtu_encoder:get_funcode/1 替代

%% 010300000002C40B 01030438A93E3B76C0
dealwith(<<SlaveId:8, FunCode:8, Address:16, _:4/binary, SlaveId:8, FunCode:8, Rest/binary>>) ->
    {ok, #{<<"buff">> => <<SlaveId:8, FunCode:8, Rest/binary>>, <<"slaveId">> => SlaveId, <<"address">> => Address}};

dealwith(Buff) ->
    Buff.

%rtu modbus
parse_frame(<<>>, Acc, _State) -> {<<>>, Acc};

parse_frame(Buff, Acc, _State) when size(Buff) < 6 ->
    {<<>>, Acc};

parse_frame(<<MbAddr:8, BadCode:8, ErrorCode:8, Crc:2/binary>> = Buff, Acc,
    #{<<"addr">> := DtuAddr} = State) ->
    CheckCrc = dgiot_utils:crc16(<<MbAddr:8, BadCode:8, ErrorCode:8>>),
    case CheckCrc =:= Crc of
        true ->
            Error = case ErrorCode of
                        ?ILLEGAL_FUNCTION -> {error, illegal_function};
                        ?ILLEGAL_DATA_ADDRESS -> {error, illegal_data_address};
                        ?ILLEGAL_DATA_VALUE -> {error, illegal_data_value};
                        ?SLAVE_DEVICE_FAILURE -> {error, slave_device_failure};
                        ?ACKNOWLEDGE -> {error, acknowledge};
                        ?SLAVE_DEVICE_BUSY -> {error, slave_device_busy};
                        ?NEGATIVE_ACKNOWLEDGE -> {error, negative_acknowledge};
                        ?MEMORY_PARITY_ERROR -> {error, memory_parity_error};
                        ?GATEWAY_PATH_UNAVAILABLE -> {error, gateway_path_unavailable};
                        ?GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND -> {error, gateway_target_device_failed_to_respond};
                        _ -> {error, unknown_response_code}
                    end,
            io:format("~s ~p DtuAddr ~p Modbus ~p, BadCode ~p, Error ~p~n", [?FILE, ?LINE, DtuAddr, MbAddr, BadCode, Error]),
            {<<>>, #{}};
        false ->
            parse_frame(Buff, Acc, State)
    end;

%% 传感器直接做为dtu物模型的一个指标
parse_frame(<<SlaveId:8, _/binary>> = Buff, Acc, #{<<"dtuproduct">> := ProductId, <<"slaveId">> := SlaveId, <<"dtuaddr">> := DtuAddr, <<"address">> := Address} = State) ->
    %io:format("~s ~p SlaveId ~p, DtuAddr ~p, Address ~p~n", [?FILE, ?LINE, SlaveId, DtuAddr, Address]),
    case decode_data(Buff, ProductId, DtuAddr, Address, Acc) of
        {Rest, NewAcc} ->
            parse_frame(Rest, NewAcc, State)
    end;

%% 传感器独立建产品，做为子设备挂载到dtu上面
parse_frame(<<SlaveId:8, _/binary>> = Buff, Acc, #{<<"dtuaddr">> := DtuAddr, <<"slaveId">> := SlaveId, <<"address">> := Address} = State) ->
    %io:format("~s ~p SlaveId ~p, DtuAddr ~p, Address ~p~n", [?FILE, ?LINE, SlaveId, DtuAddr, Address]),
    case dgiot_device:get_subdevice(DtuAddr, dgiot_utils:to_binary(SlaveId)) of
        not_find ->
            {<<>>, Acc};
        [ProductId, _DevAddr] ->
            case decode_data(Buff, ProductId, DtuAddr, Address, Acc) of
                {Rest1, Acc1} ->
                    parse_frame(Rest1, Acc1, State)
            end
    end;
%rtu modbus
parse_frame(_Other, Acc, _State) ->
    %io:format("~s ~p Other: ~p Acc:~p State:~p ~n", [?FILE, ?LINE, _Other, Acc, _State]),
    io:format("~s ~p Unknown frame format: ~p~n", [?FILE, ?LINE, _Other]),
    {error, Acc}.

decode_data(Buff, ProductId, DtuAddr, Address, Acc) ->
    <<SlaveId:8, FunCode:8, ResponseData/binary>> = Buff,
    {SizeOfData, DataBytes} =
        case FunCode of
            ?FC_READ_COILS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_READ_INPUTS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_READ_HREGS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_READ_IREGS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_WRITE_COIL -> {0, []};
            ?FC_WRITE_HREG -> {0, []};
            ?FC_WRITE_COILS -> {0, []};
            ?FC_WRITE_HREGS -> {0, []};
            _ -> {0, []}
        end,
    case SizeOfData > 0 of
        true ->
            <<UserZone:SizeOfData/bytes, Crc:2/binary, Rest1/binary>> = DataBytes,
            CheckBuf = <<SlaveId:8, FunCode:8, SizeOfData:8, UserZone/binary>>,
            CheckCrc = dgiot_utils:crc16(CheckBuf),
            case CheckCrc =:= Crc of
                true ->
                    {Rest1, modbus_decoder(ProductId, SlaveId, Address, UserZone, Acc)};
                false ->
                    {Rest1, Acc}
            end;
        false ->
            case FunCode of
                ?FC_WRITE_COIL ->
                    get_write(ResponseData, SlaveId, FunCode, DtuAddr, ProductId, Address, Acc);
                ?FC_WRITE_HREG ->
                    get_write(ResponseData, SlaveId, FunCode, DtuAddr, ProductId, Address, Acc);
                ?FC_WRITE_COILS ->
                    {Buff, Acc};
                ?FC_WRITE_HREGS ->
                    {Buff, Acc};
                _ -> {Buff, Acc}
            end
    end.

get_write(ResponseData, SlaveId, FunCode, DtuAddr, ProductId, Address, Acc) ->
    <<_Addr:2/binary, Rest1/binary>> = ResponseData,
    Size1 = byte_size(Rest1) - 2,
    <<UserZone:Size1/bytes, Crc:2/binary>> = Rest1,
    CheckBuf = <<SlaveId:8, FunCode:8, _Addr:2/binary, UserZone/binary>>,
    CheckCrc = dgiot_utils:crc16(CheckBuf),
    case CheckCrc =:= Crc of
        true ->
            {<<>>, modbus_decoder(ProductId, SlaveId, Address, UserZone, Acc)};
        false ->
            io:format("~s ~p CRC check failed for DtuAddr: ~p, ProductId: ~p~n", [?FILE, ?LINE, DtuAddr, ProductId]),
            {<<>>, Acc}
    end.

%%%% @doc 构建请求消息 - 已迁移到 modbus_rtu_encoder 模块
%%%% 使用 modbus_rtu_encoder:build_req_message/1 替代



modbus_decoder(ProductId, SlaveId, Address, Data, Acc1) ->
    modbus_rtu_decoder:modbus_decoder(ProductId, SlaveId, Address, Data, Acc1).

modbus_encoder(ProductId, SlaveId, Address, Value) ->
    modbus_rtu_encoder:modbus_encoder(ProductId, SlaveId, Address, Value).

%%%% @doc 格式解析函数 - 已迁移到 modbus_rtu_decoder 模块
%%%% 使用 modbus_rtu_decoder:format_value/3 替代


%% @doc 测试模拟数据块拆分功能
%% 验证数据块模式下的属性解析是否正确
test_simulated_data_block_split() ->
    io:format("~s ~p Starting simulated data block split test...~n", [?FILE, ?LINE]),
    
    % 模拟数据块：直接使用二进制数据（十六进制字符串 "31030C08E908E908EA000B000B00014631" 的二进制形式）
    SimulatedDataBlock = <<49,3,12,8,233,8,233,8,234,0,11,0,11,0,1,70,49>>,
    io:format("~s ~p Simulated data block: ~p~n", [?FILE, ?LINE, SimulatedDataBlock]),
    
    % 使用已配置的产品属性
    ProductId = <<"d361627b6d">>,
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            % 测试数据块缓存
            DataBlockCache = #{<<"block1">> => SimulatedDataBlock},
            io:format("~s ~p Data block cache: ~p~n", [?FILE, ?LINE, DataBlockCache]),
            
            % 处理数据块
            Result = modbus_rtu_data_blocks:process_data_blocks(DataBlockCache, Props),
            io:format("~s ~p Final result: ~p~n", [?FILE, ?LINE, Result]),
            {ok, Result};
        Error ->
            io:format("~s ~p Product not found: ~p~n", [?FILE, ?LINE, Error]),
            {error, product_not_found}
    end.

%% @doc 将地址转换为16进制格式（代理函数，调用工具模块）
%% 处理多种输入格式：0X前缀、0x前缀、十进制数字
%% 返回: 4字符的16进制字符串
is16(Data) ->
    modbus_rtu_utils:is16(Data).

%% @doc 获取数据源配置
get_datasource(#{<<"operatetype">> := <<"writeHreg">>, <<"data">> := Data} = DataSource) ->
    DataSource#{<<"data">> => Data};

get_datasource(DataSource) ->
    DataSource.
