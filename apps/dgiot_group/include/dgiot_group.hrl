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

-define(IS_TEST, false).

-define(SEND_SUCC, 0).   %%成功
-define(UN_SEND, 1).     %%未发送
-define(PASS_SEND, 2).   %%已发送
-define(SEND_TIMEOUT, 3). %%超时次数 N -3

-define(UNIT_MIN,0).
-define(UNIT_HOUR,1).
-define(UNIT_DAY,2).
-define(UNIT_MONTH,3).
-record(meter, { addr,  channel }).

-record(schedule_worker, { tid, chslen, cid, rid,
    vcaddr, pns, quelen, pn, di, fdate, status, ref, ack = 3}).

-define(dgiot_GROUP, dgiot_group).
-define(dgiot_GROUP_ROUTE, dgiot_group_route).
-define(dgiot_GROUP_TASK, dgiot_group_task).
-define(dgiot_GROUP_WORK, dgiot_group_work).
-define(dgiot_GROUP_METER, dgiot_group_meter).

-define(dgiot_GROUP_METRICS, <<"dgiot_group_metrics">>).
-define(dgiot_PARSE_METRICS, <<"dgiot_parse_metrics">>).

-define(GROUP_METRICS, dgiot_group_metrics).

-define(dlt645, 1).

-define(TIMEOUT_INTERVAL, 15000).

