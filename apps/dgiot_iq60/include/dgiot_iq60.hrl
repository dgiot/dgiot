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


-define(IQ60, <<"IQ60">>).

-record(state, {
    id,
    env = #{},
    dtuaddr = <<>>,
    step = login,
    ref = undefined,
    search = <<"quick">>,
    protocol = iq60
}).


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

