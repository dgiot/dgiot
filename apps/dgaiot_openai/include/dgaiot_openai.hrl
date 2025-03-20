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

% 使用说明:
% --------------------------------------------------------------------
% 在 dgiot_task文件 save_pnque(DtuProductId, DtuAddr, ProductId, DevAddr) 函数,用于注册电表的远程控制的topic
% timer:sleep(500),
% TopicCtrl = <<"thingctrl/", ProductId/binary, "/", DevAddr/binary>>,
% dgiot_mqtt:subscribe(TopicCtrl),
% timer:sleep(500),
% TopicStatus = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/status">>,
% dgiot_mqtt:subscribe(TopicStatus),
% --------------------------------------------------------------------

-define(dgaiot_openai, <<"dgaiot_openai">>).

-record(state, {
    id,
    env = #{},
    product = <<>>,
    dtuaddr = <<>>
}).
