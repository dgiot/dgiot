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
-define(TYPE, <<"INSTRUCT">>).
-define(DGIOT_TASK, dgiot_task).
-define(DGIOT_PNQUE, dgiot_pnque).
-define(DGIOT_DATA_CACHE, dgiot_data_cache).
-define(TASK_NAME(Name), dgiot_utils:to_atom(lists:concat([dgiot_utils:to_atom(Name), "task"]))).
-define(TASK_SUP(Name), dgiot_utils:to_atom(lists:concat(["dgiot_task_sup", dgiot_utils:to_atom(Name)]))).
