%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 John liu <34489690@qq.com>.
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

%% @doc dgiot_modbus_tcp COMMAND.

%%--------------------------------------------------------------------
%% Frame Size Limits
%%
%% To prevent malicious clients from exploiting memory allocation in a server,
%% servers MAY place maximum limits on:
%%
%% the number of frame headers allowed in a single frame
%% the maximum length of header lines
%% the maximum size of a frame body
%%
%% If these limits are exceeded the server SHOULD send the client an ERROR frame
%% and then close the connection.
%%--------------------------------------------------------------------

-define(dgiot_modbus_tcp_DTU, dgiot_modbus_tcp_dtu_ets).
-record(state, {
    id,
    devaddr = <<>>,
    heartcount = 0,
    regtype = <<>>,
    head = "xxxxxx0eee",
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    scale = 10,
    temperature = 0,
    env = <<>>
}).

