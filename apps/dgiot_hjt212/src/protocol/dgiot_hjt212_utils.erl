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

-module(dgiot_hjt212_utils).
-include_lib("dgiot_hjt212.hrl").
-author("stoneliu").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    get_infobody_len/1,
    get_info_type/1,
    get_sys_type/1,
    get_equ_type/1,
    get_equ_addr/3,
    get_op/2,
    get_op_desc/2,
    get_status/2,
    get_status_desc/2,
    get_time/1,
    get_timestamp/1,
    to_time/1,
    get_address/1
]).

get_status(Type, Flag) ->
    {_, Data} =
        lists:foldl(fun(X, {Num, Acc}) ->
            FlagId = dgiot_utils:to_binary("status_" ++ dgiot_utils:to_list(Type) ++ "_bit" ++ dgiot_utils:to_list(Num)),
            {Num + 1, Acc#{FlagId => X}}
                    end, {0, #{}}, [X || <<X:1>> <= dgiot_utils:reverse(Flag)]),
    Data.

get_op(Type, Flag) ->
    {_, Data} =
        lists:foldl(fun(X, {Num, Acc}) ->
            FlagId = dgiot_utils:to_binary("op_" ++ dgiot_utils:to_list(Type) ++ "_bit" ++ dgiot_utils:to_list(Num)),
            {Num + 1, Acc#{FlagId => get_op_desc(FlagId, X)}}
                    end, {0, #{}}, [X || <<X:1>> <= dgiot_utils:reverse(Flag)]),
    Data.

%% 8．2．1．1 建筑消防设施系统状态
%%---------------------------------------------------------------------------------------------------------------------------------------
%%   |bit15|bit14 |bit13 |bit12    |bit11   |bit10  |bit9    |bit8    |bit7   |bit6   |bit5      |bit4  |bit3  |bit2 |bit1   |bit0         |
%% 1 |预留 |预留  |复位   |配置改变  |手动状态|总线故障|备电故障|主电故障|延时状态|反馈   |启动(开启) |监管   |屏蔽  |故障  |火警  |正常运行状态 |
%% 0 |预留 |预留  |正常   |无配置改变|自动状态|总线正常|备电正常|主电正常|未延时  |无反馈 |停止(关闭) |无监管 |无屏蔽|无故障|无火警 |测试状态    |
%%---------------------------------------------------------------------------------------------------------------------------------------
get_status_desc(<<"status_8_2_1_1_bit13">>, 1) ->
    <<"复位"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit13">>, 0) ->
    <<"正常"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit12">>, 1) ->
    <<"配置改变"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit12">>, 0) ->
    <<"无配置改变"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit11">>, 1) ->
    <<"手动状态"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit11">>, 0) ->
    <<"自动状态"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit10">>, 1) ->
    <<"总线故障"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit10">>, 0) ->
    <<"总线正常"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit9">>, 1) ->
    <<"备电故障"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit9">>, 0) ->
    <<"备电正常"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit8">>, 1) ->
    <<"主电故障"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit8">>, 0) ->
    <<"主电正常"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit7">>, 1) ->
    <<"延时状态"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit7">>, 0) ->
    <<"未延时"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit6">>, 1) ->
    <<"反馈"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit6">>, 0) ->
    <<"无反馈"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit5">>, 1) ->
    <<"启动(开启)"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit5">>, 0) ->
    <<"停止(关闭)"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit4">>, 1) ->
    <<"监管"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit4">>, 0) ->
    <<"无监管"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit3">>, 1) ->
    <<"屏蔽"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit3">>, 0) ->
    <<"无屏蔽"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit2">>, 1) ->
    <<"故障"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit2">>, 0) ->
    <<"无故障"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit1">>, 1) ->
    <<"火警"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit1">>, 0) ->
    <<"无火警"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit0">>, 1) ->
    <<"正常运行状态"/utf8>>;
get_status_desc(<<"status_8_2_1_1_bit0">>, 0) ->
    <<"测试状态"/utf8>>;

%% 8．2．1．2  建筑消防设施部件状态
%%-------------------------------------------------------------------------------------------------------------------------
%%   |bit15|bit14|bit13|bit12|bit11|bit10|bit9|bit8    |bit7   |bit6   |bit5      |bit4  |bit3  |bit2 |bit1  |bit0        |
%% 1 |预留 |预留 |预留 |预留  |预留 |预留 |预留 |电源故障|延时状态|反馈   |启动(开启)|监管  |屏蔽   |故障  |火警   |正常运行状态|
%% 0 |预留 |预留 |预留 |预留  |预留 |预留 |预留 |电源正常|未延时  |无反馈 |停止(关闭)|无监管|无屏蔽 |无故障|无火警 |测试状态    |
%%-------------------------------------------------------------------------------------------------------------------------
get_status_desc(<<"status_8_2_1_2_bit8">>, 1) ->
    <<"电源故障"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit8">>, 0) ->
    <<"电源正常"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit7">>, 1) ->
    <<"延时状态"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit7">>, 0) ->
    <<"未延时"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit6">>, 1) ->
    <<"反馈"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit6">>, 0) ->
    <<"无反馈"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit5">>, 1) ->
    <<"启动(开启)"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit5">>, 0) ->
    <<"停止(关闭)"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit4">>, 1) ->
    <<"监管"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit4">>, 0) ->
    <<"无监管"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit3">>, 1) ->
    <<"屏蔽"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit3">>, 0) ->
    <<"无屏蔽"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit2">>, 1) ->
    <<"故障"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit2">>, 0) ->
    <<"无故障"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit1">>, 1) ->
    <<"火警"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit1">>, 0) ->
    <<"无火警"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit0">>, 1) ->
    <<"正常运行状态"/utf8>>;
get_status_desc(<<"status_8_2_1_2_bit0">>, 0) ->
    <<"测试状态"/utf8>>;

%%8．2．1．8 用户信息传输装置运行状态
%%---------------------------------------------------------------------------------------------
%%  |bit7|bit6           |bit5                  |bit4    |bit3     |bit2  |bit1  |bit0       |
%%---------------------------------------------------------------------------------------------
%% 1|预留|监测连接线路故障|与监控中心通信信道故障  |备电故障|主电故障 |故障   |火警  |正常监视状态|
%% 0|预留|监测连接线路正常|通信信道正常           |备电正常|主电正常 |无故障 |无火警|测试状态    |
%%---------------------------------------------------------------------------------------------
get_status_desc(<<"status_8_2_1_8_bit6">>, 1) ->
    <<"监测连接线路故障"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit6">>, 0) ->
    <<"监测连接线路正常"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit5">>, 1) ->
    <<"与监控中心通信信道故障"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit5">>, 0) ->
    <<"通信信道正常"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit4">>, 1) ->
    <<"备电故障"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit4">>, 0) ->
    <<"备电正常"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit3">>, 1) ->
    <<"主电故障"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit3">>, 0) ->
    <<"主电正常"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit2">>, 1) ->
    <<"故障"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit2">>, 0) ->
    <<"无故障"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit1">>, 1) ->
    <<"火警"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit1">>, 0) ->
    <<"无火警"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit0">>, 1) ->
    <<"正常监视状态"/utf8>>;
get_status_desc(<<"status_8_2_1_8_bit0">>, 0) ->
    <<"测试状态"/utf8>>;
get_status_desc(_, _) ->
    <<"预留"/utf8>>.


%%8．2．1．4 建筑消防设施操作信息
%%-----------------------------------------------------------------
%%  |bit7|bit6 |bit5   |bit4   | bit3    |bit2     |bit1   |bit0 |
%%-----------------------------------------------------------------
%% 1|预留|测试  |确认   |自检   | 警情消除 |手动报警 |消音   |复位  |
%% 0|预留|无操作|无操作 |无操作 | 无操作   |无操作   |无操作 |无操作 |
%%-----------------------------------------------------------------
get_op_desc(<<"op_8_2_1_4_bit6">>, 1) ->
    <<"测试"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit6">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit5">>, 1) ->
    <<"确认"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit5">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit4">>, 1) ->
    <<"自检"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit4">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit3">>, 1) ->
    <<"警情消除"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit3">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit2">>, 1) ->
    <<"手动报警"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit2">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit1">>, 1) ->
    <<"消音"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit1">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit0">>, 1) ->
    <<"复位"/utf8>>;
get_op_desc(<<"op_8_2_1_4_bit0">>, 0) ->
    <<"无操作"/utf8>>;


%% 8．2．1．9用户信息传输装置操作信息
%%-------------------------------------------------------------
%%  |bit7 |bit6  |bit5   |bit4  |bit3    |bit2   |bit1  |bit0  |
%%-------------------------------------------------------------
%% 1|预留 |测试  |查岗应答|自检  |警情消除|手动报警|消音  |复位  |
%% 0|预留 |无操作|无操作  |无操作|无操作  |无操作  |无操作|无操作|
%%-------------------------------------------------------------
get_op_desc(<<"op_8_2_1_9_bit6">>, 1) ->
    <<"测试"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit6">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit5">>, 1) ->
    <<"查岗应答"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit5">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit4">>, 1) ->
    <<"自检"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit4">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit3">>, 1) ->
    <<"警情消除"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit3">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit2">>, 1) ->
    <<"手动报警"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit2">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit1">>, 1) ->
    <<"消音"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit1">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit0">>, 1) ->
    <<"复位"/utf8>>;
get_op_desc(<<"op_8_2_1_9_bit0">>, 0) ->
    <<"无操作"/utf8>>;
get_op_desc(_, _) ->
    <<"预留"/utf8>>.

%% 不包含时间的信息体长度
get_infobody_len(Type) ->
    case Type of
        ?CLASS_UP_SYSTEM_STATE -> %  8.2.1.1  4 + 6 个字节,  有时戳， 上传建筑消防设施系统状态
            {<<"systemstate">>, 10};
        ?CLASS_UP_RUNNING_STATUS -> %  8.3.1.2  40 + 6 个字节,  有时戳，上传建筑消防设施部件运行状态
            {<<"runningstatus">>, 46};
        ?CLASS_UP_ANALOG_QUANTITY ->  %  8.3.1.3  10 + 6 个字节, 有时戳， 上传建筑消防设施部件模拟量值
            {<<"analog_quantity">>, 16};
        ?CLASS_UP_OPERATING_INFORMATION -> %  8.3.1.4  4 + 6 个字节，有时戳，上传建筑消防设施操作信息记录
            {<<"operating_information">>, 10};
        ?CLASS_UP_SOFTWARE_VERSION -> %  8.3.1.5   4 + 0个字节，无时戳 上传建筑消防设施软件版本
            {<<"software_version">>, 4};
        ?CLASS_UP_SYSTEM_CONFIGURATION ->  %  8.3.1.6   3 + L个字节,  无时戳, 上传建筑消防设施系统配置情况
            {<<"system_configuration">>, 3};
        ?CLASS_UP_EQUIPMENT_CONFIGURATION -> %  8.3.1.7   38 + 0个字节, 无时戳, 上传建筑消防设施部件配置情况
            {<<"equipment_configuration">>, 38};
        ?CLASS_UP_SYSTEM_TIME ->  %  8.3.1.8   2 + 6 个字节,  有时戳，上传建筑消防设施系统时间
            {<<"system_time">>, 8};
        ?CLASS_UP_USERDEV_RUNINFO -> %  8.3.1.9   1 + 6 个字节,  有时戳， 上传用户信息传输装置运行状态
            {<<"userdev_runinfo">>, 7};
        ?CLASS_UP_USERDEV_OPINFO -> %  8.3.1.10  2 + 6个字节,  有时戳， 上传用户信息传输装置操作信息记录
            {<<"userdev_opinfo">>, 8};
        ?CLASS_UP_USERDEV_SOFTVER ->  %  8.3.1.11  2 + 0 个字节，无时戳， 上传用户信息传输装置软件版本
            {<<"userdev_softver">>, 2};
        ?CLASS_UP_USERDEV_CONFIG -> %  8.3.1.12  1 +L字节 ，无时戳， 上传用户信息传输装置配置情况
            {<<"userdev_config">>, 1};
        ?CLASS_UP_USERDEV_SYSTIME -> %  8.3.1.13  0 + 6 字节,  有时戳， 上传用户信息传输装置系统时间
            {<<"userdev_systime">>, 6};
        _ ->
            {<<"unknow">>, 4}
    end.

get_info_type(?CLASS_UP_SYSTEM_STATE) ->
    <<"建筑消防设施部件"/utf8>>.

%%8．2．1．2建筑消防设施部件状态 表4
get_sys_type(0) ->
    <<"通用"/utf8>>;
get_sys_type(1) ->
    <<"火灾报警系统"/utf8>>;
get_sys_type(10) ->
    <<"消防联动控制器"/utf8>>;
get_sys_type(11) ->
    <<"消火栓系统"/utf8>>;
get_sys_type(12) ->
    <<"自动喷水灭火系统"/utf8>>;
get_sys_type(13) ->
    <<"气体灭火系统"/utf8>>;
get_sys_type(14) ->
    <<"水喷雾灭火系统(泵启动方式)"/utf8>>;
get_sys_type(15) ->
    <<"水喷雾灭火系统(压力容器启动方式)"/utf8>>;
get_sys_type(16) ->
    <<"泡沫灭火系统"/utf8>>;
get_sys_type(17) ->
    <<"干粉灭火系统"/utf8>>;
get_sys_type(18) ->
    <<"防烟排烟系统"/utf8>>;
get_sys_type(19) ->
    <<"防火门及卷帘系统"/utf8>>;
get_sys_type(20) ->
    <<"消防电梯"/utf8>>;
get_sys_type(21) ->
    <<"消防应急广播"/utf8>>;
get_sys_type(22) ->
    <<"消防应急照明和疏散指示系统"/utf8>>;
get_sys_type(23) ->
    <<"消防电源"/utf8>>;
get_sys_type(24) ->
    <<"消防电话"/utf8>>;
get_sys_type(Type) when Type > 127 andalso Type < 256 ->
    <<"用户自定义"/utf8>>;
get_sys_type(_) ->
    <<"预留"/utf8>>.

%%8．2．1．2建筑消防设施部件状态 表5
get_equ_type(0) ->
    <<"通用"/utf8>>;
get_equ_type(1) ->
    <<"火灾报警控制器"/utf8>>;
get_equ_type(10) ->
    <<"可燃气体探铡器"/utf8>>;
get_equ_type(11) ->
    <<"点型可燃气体探测器"/utf8>>;
get_equ_type(12) ->
    <<"独立式可燃气体探测器"/utf8>>;
get_equ_type(13) ->
    <<"线型可燃气体探测器"/utf8>>;
get_equ_type(16) ->
    <<"电气火灾监控报警器"/utf8>>;
get_equ_type(17) ->
    <<"剩余电流式电气火灾监控探测器"/utf8>>;
get_equ_type(18) ->
    <<"测温式电气火灾监控探测器"/utf8>>;
get_equ_type(21) ->
    <<"探测回路"/utf8>>;
get_equ_type(22) ->
    <<"火灾显示盘"/utf8>>;
get_equ_type(23) ->
    <<"手动火灾报警按钮"/utf8>>;
get_equ_type(24) ->
    <<"消火栓按钮"/utf8>>;
get_equ_type(25) ->
    <<"火灾探测器"/utf8>>;
get_equ_type(30) ->
    <<"感温火灾探测器"/utf8>>;
get_equ_type(31) ->
    <<"点型感温火灾探测器"/utf8>>;
get_equ_type(32) ->
    <<"点型感温火灾探测器(s型)"/utf8>>;
get_equ_type(33) ->
    <<"点型感温火灾探测器(R型)"/utf8>>;
get_equ_type(34) ->
    <<"线型感温火灾探测器"/utf8>>;
get_equ_type(35) ->
    <<"线型感温火灾探测器(S型)"/utf8>>;
get_equ_type(36) ->
    <<"线型感温火灾探测器(R型)"/utf8>>;
get_equ_type(37) ->
    <<"光纤感温火灾探测器"/utf8>>;
get_equ_type(40) ->
    <<"感烟火灾探测器"/utf8>>;
get_equ_type(41) ->
    <<"点型离子感烟火灾探测器"/utf8>>;
get_equ_type(42) ->
    <<"点型光电感烟火灾探测器"/utf8>>;
get_equ_type(43) ->
    <<"线型光束感烟火灾探测器"/utf8>>;
get_equ_type(44) ->
    <<"吸气式感烟火灾探测器"/utf8>>;
get_equ_type(50) ->
    <<"复合式火灾探测器"/utf8>>;
get_equ_type(51) ->
    <<"复合式感烟感温火灾探测器"/utf8>>;
get_equ_type(52) ->
    <<"复合式感光感温火灾探浏器"/utf8>>;
get_equ_type(53) ->
    <<"复合式感光感烟火灾探测器"/utf8>>;
get_equ_type(61) ->
    <<"紫外火焰探测器"/utf8>>;
get_equ_type(62) ->
    <<"红外火焰探测器"/utf8>>;
get_equ_type(69) ->
    <<"感光火灾探测器"/utf8>>;
get_equ_type(74) ->
    <<"气体探测器"/utf8>>;
get_equ_type(78) ->
    <<"图像摄像方式火灾探测器"/utf8>>;
get_equ_type(79) ->
    <<"感声火灾探测器"/utf8>>;
get_equ_type(81) ->
    <<"气体灭火控制器"/utf8>>;
get_equ_type(82) ->
    <<"消防电气控制装置"/utf8>>;
get_equ_type(83) ->
    <<"消防控制室图形显示装置"/utf8>>;
get_equ_type(84) ->
    <<"模块"/utf8>>;
get_equ_type(85) ->
    <<"输入模块"/utf8>>;
get_equ_type(86) ->
    <<"输出模块"/utf8>>;
get_equ_type(87) ->
    <<"输入／输出模块"/utf8>>;
get_equ_type(88) ->
    <<"中继模块"/utf8>>;
get_equ_type(91) ->
    <<"消防水泵"/utf8>>;
get_equ_type(92) ->
    <<"消防水箱"/utf8>>;
get_equ_type(95) ->
    <<"喷淋泵"/utf8>>;
get_equ_type(96) ->
    <<"水流指示器"/utf8>>;
get_equ_type(97) ->
    <<"信号阀"/utf8>>;
get_equ_type(98) ->
    <<"报警阀"/utf8>>;
get_equ_type(99) ->
    <<"压力开关"/utf8>>;
get_equ_type(101) ->
    <<"阀驱动装置"/utf8>>;
get_equ_type(102) ->
    <<"防火门"/utf8>>;
get_equ_type(103) ->
    <<"防火阀"/utf8>>;
get_equ_type(104) ->
    <<"通风空调"/utf8>>;
get_equ_type(105) ->
    <<"泡沫液泵"/utf8>>;
get_equ_type(106) ->
    <<"管网电磁阀"/utf8>>;
get_equ_type(111) ->
    <<"防烟排烟风机"/utf8>>;
get_equ_type(113) ->
    <<"排烟防火阀"/utf8>>;
get_equ_type(114) ->
    <<"常闭送风口"/utf8>>;
get_equ_type(115) ->
    <<"排烟口"/utf8>>;
get_equ_type(116) ->
    <<"电控挡烟垂壁"/utf8>>;
get_equ_type(117) ->
    <<"防火卷帘控制器"/utf8>>;
get_equ_type(118) ->
    <<"防火门监控器"/utf8>>;
get_equ_type(121) ->
    <<"警报装置"/utf8>>;
%% 三江自定义
get_equ_type(128) ->
    <<"打印机故障"/utf8>>;
%% 三江自定义
get_equ_type(249) ->
    <<"备电故障"/utf8>>;
%% 三江自定义
get_equ_type(251) ->
    <<"主电故障"/utf8>>;
get_equ_type(Type) when Type > 127 andalso Type < 256 ->
    BinType = dgiot_utils:to_binary(Type),
    <<"用户自定义"/utf8, "_", BinType/binary>>;
get_equ_type(Type) ->
    BinType = dgiot_utils:to_binary(Type),
    <<"预留"/utf8, "_", BinType/binary>>.

get_equ_addr(Board, <<Loop:16, Number:16>>, EquType) ->
    BinBoard = dgiot_utils:to_binary(Board),
%%    0xFF+接口板回路号n
    NewLoop =
        case Loop > 65280 of
            true ->
                Loop - 65280;
            _ -> Loop
        end,
    BinLoop = dgiot_utils:to_binary(NewLoop),
    BinNumber = dgiot_utils:to_binary(Number),
    BinEquType = dgiot_hjt212_utils:get_equ_type(EquType),
    case Number of
        0 ->
            <<BinEquType/binary, "_", BinBoard/binary, "号接口板回路"/utf8>>;
        _ ->
            <<BinEquType/binary, "_", BinBoard/binary, "号接口板"/utf8, BinLoop/binary, "回路"/utf8, BinNumber/binary, "号"/utf8>>
    end.

get_time(<<S:8, M:8, H:8, D:8, Mon:8, Y:8>> = Time) ->
    io:format("~s ~p ~p ~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(Time)]),
    BinY = dgiot_utils:to_binary(Y),
    BinMon = dgiot_utils:to_binary(Mon),
    BinD = dgiot_utils:to_binary(D),
    BinH = dgiot_utils:to_binary(H),
    BinM = dgiot_utils:to_binary(M),
    BinS = dgiot_utils:to_binary(S),
    At = <<"20", BinY/binary, "-", BinMon/binary, "-", BinD/binary, " ", BinH/binary, ":", BinM/binary, ":", BinS/binary>>,
    Ts = get_timestamp(Time),
    #{
        <<"at">> => At,
        <<"ts">> => Ts
    }.

get_timestamp(<<S:8, M:8, H:8, D:8, Mon:8, Y:8>>) when Mon < 13 andalso D < 32 andalso H < 25 andalso M < 61 andalso S < 61 ->
    dgiot_datetime:localtime_to_unixtime({{2000 + Y, Mon, D}, {H, M, S}});
get_timestamp(_) ->
    dgiot_datetime:now_secs().

to_time(Timestamp) ->
    {{Y, Mon, D}, {H, M, S}} = dgiot_datetime:unixtime_to_localtime(Timestamp),
    Y1 = Y - 2000,
    <<S:8, M:8, H:8, D:8, Mon:8, Y1:8>>.

get_address(<<A:8, B:8, C:8, D:8, Port:16>>) ->
    Address = lists:concat([A, ".", B, ".", C, ".", D, ":", Port]),
    dgiot_utils:to_binary(Address).
