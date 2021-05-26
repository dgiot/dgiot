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
-author("johnliu").


%% ====  全局缓存表 ========
-define(MCACHE, mnesia_dgiotcache).
-record(dgiot_mcache, {
    key,
    value
}).

%% ====  设备路由表 ========
-define(DGIOT_ROUTE, mnesia_route).
-record(dgiot_route, {
    key,      % [ProductId, DevAddr], [产品ID, 设备地址]
    status,   % 设备状态
    node      % 节点
}).

%% ====  设备表 ========
-define(SMART_DEV, mnesia_smartdev).
-record(dgiot_device, {
    key,      % [ProductId, DevAddr], [产品ID, 设备地址]
    basedata  % 设备基本数据,map类型
}).

-define(SMART_PROD, mnesia_smartprod).
-record(dgiot_prod, {
    key,      % [ProductId], [产品ID]
    product   % 产品基本数据,map类型
}).

-define(SMART_HUB, mnesia_smarthub).
-record(dgiot_hub, {
    key,      % [vcaddr,pn], [网关设备地址,子设备子网地址]
    subdev    % [ProductId,DevAddr], [产品ID, 设备地址]
}).

-define(SMART_GATEWAY, mnesia_smartgateway).
-record(dgiot_gateway, {
    key,    % [ProductId, DevAddr], [产品ID, 设备地址]
    gatway  % [DtuProductId, vcaddr,pn], [网关产品ID, 网关设备地址,子设备子网地址]
}).

-define(TABLES, [
    {?MCACHE, [
        {ram_copies, [node()]},
        {record_name, dgiot_mcache},
        {attributes, record_info(fields, dgiot_mcache)},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]},

    {?DGIOT_ROUTE, [
        {ram_copies, [node()]},
        {record_name, dgiot_route},
        {type, set},
        {index, [node]},
        {attributes, record_info(fields, dgiot_route)},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]},

    {?SMART_DEV, [
        {disc_copies, [node()]},
        {local_content, true},
        {type, set},
        {record_name, dgiot_device},
        {attributes, record_info(fields, dgiot_device)},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]},

    {?SMART_PROD, [
        {ram_copies, [node()]},
        {record_name, dgiot_prod},
        {type, set},
        {attributes, record_info(fields, dgiot_prod)},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]},

    {?SMART_HUB, [
        {ram_copies, [node()]},
        {record_name, dgiot_hub},
        {type, set},
        {attributes, record_info(fields, dgiot_hub)},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]},

    {?SMART_GATEWAY, [
        {ram_copies, [node()]},
        {record_name, dgiot_gateway},
        {type, set},
        {attributes, record_info(fields, dgiot_gateway)},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]}
]).
