%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http) ->%%www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_iec104_utils).
-include_lib("dgiot_iec104.hrl").
-author("stoneliu").
-include_lib("dgiot/include/logger.hrl").
%% API
-export([
    get_sys_type/1,
    get_status_type/1,
    get_result_type/1,
    get_return_type/1,
    get_len/1,
    crc16/1,
    get_cps/1,
    getCommandType/1
]).


%%6.6.1.2 系统编码方法
%%系统编码（见表 5）由两位取值 0~9、A~Z 的字符表示。
%% 6．6．1．2 污染物在线监控 系统编码表
%%---------------------------------------------------------------------------------------------------------------------------------------
%%地表水质量监测 21
%%空气质量监测 22
%%声环境质量监测 23
%%地下水质量监测 24
%%土壤质量监测 25
%%海水质量监测 26
%%挥发性有机物监测 27
%%大气环境污染源 31
%%地表水体环境污染源 32
%%地下水体环境污染源 33
%%海洋环境污染源 34
%%土壤环境污染源 35
%%声环境污染源 36
%%振动环境污染源 37
%%放射性环境污染源 38
%%工地扬尘污染源 39
%%电磁环境污染源 41
%%烟气排放过程监控 51
%%污水排放过程监控 52
%%系统交互 91 用于现场机和上位机的交互
%%---------------------------------------------------------------------------------------------------------------------------------------

get_sys_type(21) ->
    <<"地表水质量监测"/utf8>>;
get_sys_type(22) ->
    <<"空气质量监测"/utf8>>;
get_sys_type(23) ->
    <<"声环境质量监测"/utf8>>;
get_sys_type(24) ->
    <<"地下水质量监测"/utf8>>;
get_sys_type(25) ->
    <<"土壤质量监测"/utf8>>;
get_sys_type(26) ->
    <<"海水质量监测"/utf8>>;
get_sys_type(27) ->
    <<"挥发性有机物监测"/utf8>>;
get_sys_type(31) ->
    <<"大气环境污染源"/utf8>>;
get_sys_type(32) ->
    <<"地表水体环境污染源"/utf8>>;
get_sys_type(33) ->
    <<"地下水体环境污染源"/utf8>>;
get_sys_type(34) ->
    <<"海洋环境污染源"/utf8>>;
get_sys_type(35) ->
    <<"土壤环境污染源"/utf8>>;
get_sys_type(36) ->
    <<"声环境污染源"/utf8>>;
get_sys_type(37) ->
    <<"振动环境污染源"/utf8>>;
get_sys_type(38) ->
    <<"放射性环境污染源"/utf8>>;
get_sys_type(39) ->
    <<"工地扬尘污染源"/utf8>>;
get_sys_type(41) ->
    <<"电磁环境污染源"/utf8>>;
get_sys_type(51) ->
    <<"烟气排放过程监控"/utf8>>;
get_sys_type(52) ->
    <<"污水排放过程监控"/utf8>>;
get_sys_type(91) ->
    <<"系统交互"/utf8>>.


%%-------------------------------------------------------------------------------------
%%6.6.2 执行结果定义（可扩充）
%%执行结果定义如表 6 所示。
%%表 6 执行结果定义表
%%编号 描述 备注
%% 1 执行成功
%% 2 执行失败，但不知道原因
%% 3 命令请求条件错误
%% 4 通讯超时
%% 5 系统繁忙不能执行
%% 6 系统故障
%% 100 没有数据
%%------------------------------------------------------------------------------------

get_result_type(1) ->
    <<"执行成功">>;
get_result_type(2) ->
    <<"执行失败，但不知道原因">>;
get_result_type(3) ->
    <<"命令请求条件错误">>;
get_result_type(4) ->
    <<"通讯超时">>;
get_result_type(5) ->
    <<"系统繁忙不能执行">>;
get_result_type(6) ->
    <<"系统故障">>;
get_result_type(100) ->
    <<"没有数据">>;
get_result_type(_) ->
    <<"可扩充">>.

%%6.6.3 请求命令返回（可扩充）
%%请求命令返回如表 7 所示。
%%表 7 请求命令返回表
%%1 准备执行请求
%%2 请求被拒绝
%%3 PW 错误
%%4 MN 错误
%%5 ST 错误
%%6 Flag 错误
%%7 QN 错误
%%8 CN 错误
%%9 CRC 校验错误
%%100 未知错误
get_return_type(1) ->
    <<"准备执行请求"/utf8>>;
get_return_type(2) ->
    <<"请求被拒绝"/utf8>>;
get_return_type(3) ->
    <<"PW 错误"/utf8>>;
get_return_type(4) ->
    <<"MN 错误"/utf8>>;
get_return_type(5) ->
    <<"ST 错误"/utf8>>;
get_return_type(6) ->
    <<"Flag 错误"/utf8>>;
get_return_type(7) ->
    <<"QN 错误"/utf8>>;
get_return_type(8) ->
    <<"CN 错误"/utf8>>;
get_return_type(9) ->
    <<"CRC 校验错误"/utf8>>;
get_return_type(100) ->
    <<"未知错误"/utf8>>;
get_return_type(Type) ->
    BinType = dgiot_utils:to_binary(Type),
    <<"可扩充"/utf8, "_", BinType/binary>>.

%%6.6.4 数据标记（可扩充）
%%数据标记如表 8 所示。
%% 表 8 数据标记表
%%N 在线监控（监测）仪器仪表工作正常
%%F 在线监控（监测）仪器仪表停运
%%M 在线监控（监测）仪器仪表处于维护期间产生的数据
%%S 手工输入的设定值
%%D 在线监控（监测）仪器仪表故障
%%C 在线监控（监测）仪器仪表处于校准状态
%%T 在线监控（监测）仪器仪表采样数值超过测量上限
%%B 在线监控（监测）仪器仪表与数采仪通讯异常

get_status_type("N") ->
    <<"在线监控（监测）仪器仪表工作正常"/utf8>>;
get_status_type("F") ->
    <<"在线监控（监测）仪器仪表停运"/utf8>>;
get_status_type("M") ->
    <<"在线监控（监测）仪器仪表处于维护期间产生的数据"/utf8>>;
get_status_type("S") ->
    <<"手工输入的设定值"/utf8>>;
get_status_type("D") ->
    <<"在线监控（监测）仪器仪表故障"/utf8>>;
get_status_type("C") ->
    <<"在线监控（监测）仪器仪表处于校准状态"/utf8>>;
get_status_type("T") ->
    <<"在线监控（监测）仪器仪表采样数值超过测量上限"/utf8>>;
get_status_type("B") ->
    <<"在线监控（监测）仪器仪表与数采仪通讯异常"/utf8>>;
get_status_type(Type) ->
    BinType = dgiot_utils:to_binary(Type),
    <<"可扩充"/utf8, "_", BinType/binary>>.


%%6.6.5.2 命令编码方法
%%命令编码用 4 位阿拉伯数字表示，如表 9 所示。
%%表 9 命令编码表

%%设置超时时间及重发次数
getCommandType(?SetTimeOutReSendTimes) ->
    ?Request;
%%提取现场机时间
getCommandType(?GetSceneDeviceTime) ->
    ?Request;
%%设置现场机时间
getCommandType(?SetSceneDeviceTime) ->
    ?Request;
%%现场机时间校准请求
getCommandType(?SceneDeviceTimeCalibration) ->
    ?Notice;
%%提取实时数据间隔
getCommandType(?GetRtdDataInterval) ->
    ?Request;
%%设置实时数据间隔
getCommandType(?SetRtdDataInterval) ->
    ?Request;
%%提取分钟数据间隔
getCommandType(?GetMinuteDataInterval) ->
    ?Request;
%%设置分钟数据间隔
getCommandType(?SetMinuteDataInterval) ->
    ?Request;
%%用于设置现场机的密码
getCommandType(?SetSceneDevicePassword) ->
    ?Request;
%%取污染物实时数据
getCommandType(?GetRtdData) ->
    ?Request;
%%停止察看污染物实时数据
getCommandType(?StopRtdData) ->
    ?Notice;
%%取设备运行状态数据
getCommandType(?GetDeviceRunState) ->
    ?Request;
%%停止察看设备运行状态
getCommandType(?StopDeviceRunState) ->
    ?Request;
%%取污染物日历史数据
getCommandType(?GetDayData) ->
    ?Request;
%%取设备运行时间日历史数据
getCommandType(?GetDeviceRunTimeDayData) ->
    ?Request;
%%取污染物分钟数据
getCommandType(?GetMinuteData) ->
    ?Request;
%%取污染物小时数据
getCommandType(?GetHourData) ->
    ?Request;
%%上传数采仪开机时间
getCommandType(?UploadComputerPowerOnTime) ->
    ?Upload;
%%零点校准量程校准
getCommandType(?RangeCalibration) ->
    ?Request;
%%即时采样
getCommandType(?TakeSampleImmediately) ->
    ?Request;
%%启动清洗/反吹
getCommandType(?StartClear) ->
    ?Request;
%%比对采样
getCommandType(?CompareSample) ->
    ?Request;
%%超标留样
getCommandType(?LeaveSuperstandardSample) ->
    ?Request;
%%设置采样时间周期
getCommandType(?SetSampleTimeInterval) ->
    ?Request;
%%提取采样时间周期
getCommandType(?GetSampleTimeInterval) ->
    ?Request;
%%提取出样时间
getCommandType(?GetSampleTime) ->
    ?Request;
%%提取设备唯一标识
getCommandType(?GetSceneDeviceUUID) ->
    ?Request;
%%提取现场机信息
getCommandType(?GetSceneDeviceInfo) ->
    ?Request;
%%设置现场机参数
getCommandType(?SetSceneDeviceParam) ->
    ?Request;
%%取污染物周期数据
getCommandType(?GetCycleData) ->
    ?Request;
%%请求应答
getCommandType(?RequestResponse) ->
    ?Other;
%%执行结果
getCommandType(?ExecuteResponse) ->
    ?Other;
%%通知应答
getCommandType(?NoticeResponse) ->
    ?Other;
%%数据应答
getCommandType(?DataResponse) ->
    ?Other;
%%心跳
getCommandType(?HeartBeat) ->
    ?Other;
getCommandType(_) ->
    ?None.

get_len(Rdata) ->
    Len = byte_size(Rdata),
    list_to_binary(lists:flatten(io_lib:format("~4.10.0b", [Len]))).


%%CRC 校验（Cyclic Redundancy Check）是一种数据传输错误检查方法。本标准采用 ANSI CRC16，
%%简称 CRC16。
%%CRC16 码由传输设备计算后加入到数据包中。接收设备重新计算接收数据包的 CRC16 码，并与接
%%收到的 CRC16 码比较，如果两值不同，则有误。
%%CRC16 校验字节的生成步骤如下：
%%1) CRC16 校验寄存器赋值为 0xFFFF；
%%2) 取被校验串的第一个字节赋值给临时寄存器；
%%3) 临时寄存器与 CRC16 校验寄存器的高位字节进行“异或”运算，赋值给 CRC16 校验寄存器；
%%4) 取 CRC16 校验寄存器最后一位赋值给检测寄存器；
%%5) 把 CRC16 校验寄存器右移一位；
%%6) 若检测寄存器值为 1，CRC16 校验寄存器与多项式 0xA001 进行“异或”运算，赋值给 CRC16
%%校验寄存器；
%%7) 重复步骤 4~6，直至移出 8 位；
%%8) 取被校验串的下一个字节赋值给临时寄存器；
%%9) 重复步骤 3~8，直至被校验串的所有字节均被校验；
%%10) 返回 CRC16 校验寄存器的值。
%%校验码按照先高字节后低字节的顺序存放。
crc16(Buff) -> crc16(Buff, 16#FFFF).
crc16(<<>>, Crc) ->
    list_to_binary(string:to_upper(lists:flatten(io_lib:format("~4.16.0b", [Crc]))));
crc16(<<B:8, Rest/binary>>, Crc) ->
    NewCrc =
        lists:foldl(fun(_, CrcRegister) ->
            Check = CrcRegister band 16#0001,
            New = CrcRegister bsr 1,
            case Check of
                1 ->
                    New bxor 16#A001;
                0 ->
                    New
            end
                    end, (Crc bsr 8) bxor B, lists:seq(1, 8)),
    crc16(Rest, NewCrc).

get_cps(ParamCodes) ->
    CP1 = re:replace(ParamCodes, <<"&&">>, <<"">>),
    lists:foldl(fun(X, Acc) ->
        case re:split(X, <<"=">>) of
            [K, V] ->
                Acc#{K => V};
            _ -> Acc
        end
                end, #{}, re:split(CP1, ",")).