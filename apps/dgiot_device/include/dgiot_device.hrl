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

-define(DGIOT_PRODUCT, dgiot_product).
-define(DGIOT_PRODUCT_IDENTIFIE, dgiot_product_identifie).
-define(DGIOT_PRODUCT_STAB, dgiot_product_stab).
-define(DGIOT_DEVICE, dgiot_device).
-define(DEVICE_PROFILE, dgiot_device_profile).
-define(DEVICE_DEVICE_COLOR, dgiot_device_color).
-define(DGIOT_CHANNEL_SESSION, dgiot_channel_session).

-define(LOCAL, smartdev).
-define(VCON, 0).
-define(DEV, 1).
-define(DGIOT_LOCATION_ADDRESS, dgiot_location_address).

%% 通知类型
-define(NOTIFY_TYPE_STATE, <<"state">>).
-define(NOTIFY_TYPE_ALARM, <<"alarm">>).
-define(NOTIFY_TYPE_DATA, <<"data">>).

%% 告警级别
-define(ALARM_LEVEL_INFO, <<"info">>).
-define(ALARM_LEVEL_WARNING, <<"warning">>).
-define(ALARM_LEVEL_ERROR, <<"error">>).
-define(ALARM_LEVEL_CRITICAL, <<"critical">>).
