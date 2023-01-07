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


-define(MAX_PROFILE, 100).
-define(TYPE, <<"TD">>).
-define(DEFAULT, <<"default">>).
-define(PRE, <<"_">>).
-define(Database(Name), <<?PRE/binary, Name/binary>>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).
-define(TABLEDESCRIBE, describe_table).
-define(TABLEFIELDS, fields_table).

-define(Struct(Field, Field1), <<"struct_", Field/binary, "_", Field1/binary>>).

-define(TYPES, [
    <<"TIMESTAMP">>,
    <<"INT">>,
    <<"BIGINT">>,
    <<"FLOAT">>,
    <<"DOUBLE">>,
    <<"BINARY">>,
    <<"SMALLINT">>,
    <<"TINYINT">>,
    <<"BOOL">>,
    <<"NCHAR">>
]).

-define(DGIOT_TD_CH_WORK, dgiot_td_ch_work).
-define(DGIOT_TD_CH_MODEL, dgiot_td_ch_model).
-define(DGIOT_TD_THING_ETS, dgiot_td_thing_ets).
