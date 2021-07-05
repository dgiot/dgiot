%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-define(METER, <<"METER">>).
-define(DLT645, <<"DLT645">>).

-record(state, {
    id,
    env = #{},
    dtuaddr = <<>>,
    step = login,
    ref = undefined
}).

%% Internal Header File

%% @doc dlt645 COMMAND.
-define(DLT645_MS_BROADCAST_DATA,     16#08).
-define(DLT645_MS_READ_DATA,          16#11).
-define(DLT645_SM_READ_DATA_NONE,     16#91).
-define(DLT645_SM_READ_DATA_MORE,     16#B1).
-define(DLT645_SM_READ_DATA_ERRO,     16#D1).
-define(DLT645_MS_READ_FOLLOW_DATA,          16#12).
-define(DLT645_SM_READ_FOLLOW_DATA_NONE,     16#92).
-define(DLT645_SM_READ_FOLLOW_DATA_MORE,     16#B2).
-define(DLT645_SM_READ_FOLLOW_DATA_ERRO,     16#D2).
-define(DLT645_MS_READ_COMADDR_DATA,          16#13).
-define(DLT645_SM_READ_COMADDR_DATA_NONE,     16#93).
-define(DLT645_SM_READ_COMADDR_DATA_MORE,     16#B3).
-define(DLT645_SM_READ_COMADDR_DATA_ERRO,     16#D3).
-define(DLT645_MS_WRITE_DATA,         16#14).
-define(DLT645_SM_WRITE_DATA_NORM,    16#94).
-define(DLT645_SM_WRITE_DATA_ERRO,    16#D4).
-define(DLT645_MS_WRITE_COMADDR_DATA,         16#15).
-define(DLT645_SM_WRITE_COMADDR_DATA_NORM,    16#95).
-define(DLT645_SM_WRITE_COMADDR_DATA_ERRO,    16#D5).
-define(DLT645_MS_FREEZE_CMD,         16#16).
-define(DLT645_SM_FREEZE_CMD_NORM,    16#96).
-define(DLT645_SM_FREEZE_CMD_ERRO,    16#D6).
-define(DLT645_MS_CHANGE_SPEED_CMD,         16#17).
-define(DLT645_SM_CHANGE_SPEED_CMD_NORM,    16#97).
-define(DLT645_SM_CHANGE_SPEED_CMD_ERRO,    16#D7).
-define(DLT645_MS_MODIFY_PSW,         16#18).
-define(DLT645_SM_MODIFY_PSW_NORM,    16#98).
-define(DLT645_SM_MODIFY_PSW_ERRO,    16#D8).
-define(DLT645_MS_CLEAR_DEMAND,       16#19).
-define(DLT645_SM_CLEAR_DEMAND_NORM,  16#99).
-define(DLT645_SM_CLEAR_DEMAND_ERRO,  16#D9).
-define(DLT645_MS_CLEAR_DATA,         16#1A).
-define(DLT645_SM_CLEAR_DATA_NORM,    16#9A).
-define(DLT645_SM_CLEAR_DATA_ERRO,    16#DA).
%%//事件清零
-define(DLT645_MS_CLEAR_EVENT,        16#1B).
-define(DLT645_SM_CLEAR_EVENT_NORM,   16#9B).
-define(DLT645_SM_CLEAR_EVENT_ERRO,   16#DB).

-define(DLT645_MS_FORCE_EVENT,        16#1C).
-define(DLT645_SM_FORCE_EVENT_NORM,   16#9C).
-define(DLT645_SM_FORCE_EVENT_ERRO,   16#DC).
-define(DLT645_MS_OUTPUT_SIGNAL,      16#1D).
-define(DLT645_SM_OUTPUT_SIGNAL_NORM, 16#9D).
-define(DLT645_SM_OUTPUT_SIGNAL_ERRO, 16#DD).
-define(DLT645_MS_HEARTBEAT,          16#1E).
-define(DLT645_MS_AUTH,               16#03).
-define(DLT645_SM_AUTH_NORM,          16#83).
-define(DLT645_SM_AUTH_ERRO,          16#C3).
-define(DLT645_MS_FIND_CARD,          16#09).
-define(DLT645_SM_FIND_CARD_NORM,     16#89).
-define(DLT645_SM_FIND_CARD_ERRO,     16#C9).

%% @doc dlt645_97 COMMAND.
-define(DLT645_97_MS_READ_DATA,          16#01).
-define(DLT645_97_SM_READ_DATA_NONE,     16#81).
-define(DLT645_97_SM_READ_DATA_MORE,     16#A1).
-define(DLT645_97_SM_READ_DATA_ERRO,     16#C1).

-define(DLT645_97_MS_READ_FOLLOW_DATA,          16#02).
-define(DLT645_97_SM_READ_FOLLOW_DATA_NONE,     16#82).
-define(DLT645_97_SM_READ_FOLLOW_DATA_MORE,     16#A2).
-define(DLT645_97_SM_READ_FOLLOW_DATA_ERRO,     16#C2).

-define(DLT645_97_MS_RE_READ_DATA,          16#03).
-define(DLT645_97_SM_RE_READ_DATA_NONE,     16#83).
-define(DLT645_97_SM_RE_READ_DATA_MORE,     16#A3).
-define(DLT645_97_SM_RE_READ__DATA_ERRO,     16#C3).

-define(DLT645_97_MS_WRITE_DEVADDR_DATA,         16#0A).
-define(DLT645_97_SM_WRITE_DEVADDR_DATA_NORM,    16#8A).
-define(DLT645_97_SM_WRITE_DEVADDR_DATA_ERRO,    16#CA).
-define(DLT645_97_MS_WRITE_DATA,         16#04).
-define(DLT645_97_SM_WRITE_DATA_NORM,    16#84).
-define(DLT645_97_SM_WRITE_DATA_ERRO,    16#C4).

-define(DLT645_97_MS_CHANGE_SPEED_CMD,         16#0C).
-define(DLT645_97_SM_CHANGE_SPEED_CMD_NORM,    16#8C).
-define(DLT645_97_SM_CHANGE_SPEED_CMD_ERRO,    16#CC).
-define(DLT645_97_MS_MODIFY_PSW,         16#0F).
-define(DLT645_97_SM_MODIFY_PSW_NORM,    16#8F).
-define(DLT645_97_SM_MODIFY_PSW_ERRO,    16#CF).
-define(DLT645_97_MS_CLEAR_DEMAND,       16#10).
-define(DLT645_97_SM_CLEAR_DEMAND_NORM,  16#90).
-define(DLT645_97_SM_CLEAR_DEMAND_ERRO,  16#D0).


-define(DLT645_MS_READ_DATA_NAME,          <<"11">>).
-define(DLT645_SM_READ_DATA_NONE_NAME,     <<"91">>).
-define(DLT645_SM_READ_DATA_MORE_NAME,     <<"B1">>).
-define(DLT645_SM_READ_DATA_ERRO_NAME,     <<"D1">>).
-define(DLT645_MS_WRITE_DATA_NAME,         <<"14">>).
-define(DLT645_SM_WRITE_DATA_NORM_NAME,    <<"94">>).
-define(DLT645_SM_WRITE_DATA_ERRO_NAME,    <<"D4">>).
-define(DLT645_MS_MODIFY_PSW_NAME,         <<"18">>).
-define(DLT645_SM_MODIFY_PSW_NORM_NAME,    <<"98">>).
-define(DLT645_SM_MODIFY_PSW_ERRO_NAME,    <<"D8">>).
-define(DLT645_MS_CLEAR_DEMAND_NAME,       <<"19">>).
-define(DLT645_SM_CLEAR_DEMAND_NORM_NAME,  <<"99">>).
-define(DLT645_SM_CLEAR_DEMAND_ERRO_NAME,  <<"D9">>).
-define(DLT645_MS_CLEAR_DATA_NAME,         <<"1A">>).
-define(DLT645_SM_CLEAR_DATA_NORM_NAME,    <<"9A">>).
-define(DLT645_SM_CLEAR_DATA_ERRO_NAME,    <<"DA">>).
-define(DLT645_MS_CLEAR_EVENT_NAME,        <<"1B">>).
-define(DLT645_SM_CLEAR_EVENT_NORM_NAME,   <<"9B">>).
-define(DLT645_SM_CLEAR_EVENT_ERRO_NAME,   <<"DB">>).
-define(DLT645_MS_FORCE_EVENT_NAME,        <<"1C">>).
-define(DLT645_SM_FORCE_EVENT_NORM_NAME,   <<"9C">>).
-define(DLT645_SM_FORCE_EVENT_ERRO_NAME,   <<"DC">>).
-define(DLT645_MS_OUTPUT_SIGNAL_NAME,      <<"1D">>).
-define(DLT645_SM_OUTPUT_SIGNAL_NORM_NAME, <<"9D">>).
-define(DLT645_SM_OUTPUT_SIGNAL_ERRO_NAME, <<"DD">>).
-define(DLT645_MS_HEARTBEAT_NAME,          <<"1E">>).
-define(DLT645_MS_AUTH_NAME,               <<"03">>).
-define(DLT645_SM_AUTH_NORM_NAME,          <<"83">>).
-define(DLT645_SM_AUTH_ERRO_NAME,          <<"C3">>).
-define(DLT645_MS_FIND_CARD_NAME,          <<"09">>).
-define(DLT645_SM_FIND_CARD_NORM_NAME,     <<"89">>).
-define(DLT645_SM_FIND_CARD_ERRO_NAME,     <<"C9">>).

%%--------------------------------------------------------------------
%% dlt645 Frame
%%--------------------------------------------------------------------

-record(dlt645_frame, {command, headers = [], body = <<>> :: iolist()}).

-type dlt645_frame() ::  #dlt645_frame{}.

-record(di_645data, {di1, di2, di3, di4}).

-type di_645data() ::  #di_645data{}.

-record(a1_645data, { di :: di_645data(),
    da
}).

