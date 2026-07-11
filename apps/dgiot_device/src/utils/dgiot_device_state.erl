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

-module(dgiot_device_state).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([get_online/1, online/1, offline/1, enable/1, disable/1,
         put_color/3, get_color/2, put_location/3, get_location/1,
         get_address/3]).

get_online(_) -> 0.
online(_) -> ok.
offline(_) -> ok.
enable(_) -> ok.
disable(_) -> ok.
put_color(_, _, _) -> ok.
get_color(_, _) -> <<"#000000">>.
put_location(_, _, _) -> ok.
get_location(_) -> {0.0, 0.0}.
get_address(_, _, _) -> <<"">>.