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

-define(ZETAG, <<"ZETA-G">>).

-define(DIR_UP, 1).%上行
-define(DIR_DOWN, 0).%下行

-define(ACK_SUC, 1).%回复成功
-define(ACK_FAIL, 2).%回复失败

-define(RELOAD_MOD,16#E0).%%复位

-define(LOGIN_IN, 16#F0).%%基站注册
-define(HEART_BEAT, 16#F1).%%基站心跳
-define(SEND_RSA, 16#F2).%%基站发送公钥上行命令

-define(MOD_START, 16#08).%%基站模块启动
-define(OFFLINE_DATA, 16#1A).%%离线数据上下行
-define(ONLINE_DATA, 16#1B).%%在线数据上下行
-define(DEVICE_HB, 16#1C).%%ZETA-G终端心跳
-define(ZETAG_HB, 16#1D).%%新版本ZETA-G终端心跳
-define(ACK_CMD, 16#FF).%%上行/下行 ACK 命令

%%基站控制
-define(RESTART, 16#E1).%%重启
-define(SET, 16#E2).%%参数设置
-define(QUERY, 16#E3).%%参数查询
-define(GET_VERSION, 16#E4).%%基站版本查询
-define(GET_MOD_INFO, 16#E5).%%GPRS模块信息查询
-define(SET_TIME, 16#EA).%%设置基站时间
-define(GET_TIME, 16#EB).%%获取基站时间
-define(GET_GPS, 16#ED).%%获取基站GPS状态
-define(SEND_MOD, 16#E6).%%下行到基站模块

%%中继
-define(MOTE_REG, 16#02).%%中继注册到基站上行命令
-define(DEV_MOTE_REG, 16#03).%%设备注册到中继上行命令
-define(DEV_REG, 16#04).%%终端注册到基站上行命令
-define(MOTE_HEART, 16#05).%%中继心跳
-define(DEV_RX, 16#0A).%%终端上行变长数据帧上行命令
-define(DEV_HEART, 16#0F).%%终端心跳包上行命令（0f）
%%远程基站升级协议

-record(state,{product, channelid, sendfun, ip, secret, apID, order,env = #{}}).
-define(Metrics, zeta_metrics).

-define(MAX_INT, 21474836).

-record(mystate, {
    tid,
    apid,
    x, y,
    seq = 0,
    rssi = -180,
    i = 0,
    freq = 100,
    pool = #{},
    secret = true,
    version,
    status,
    count = 0,
    key
}).

-define(KI, <<"1111111111111111">>).

-define(ZETA_AP, zeta_ap).
-define(ZETA_ZETA, zeta_zeta).
