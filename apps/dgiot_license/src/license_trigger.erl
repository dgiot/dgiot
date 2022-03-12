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
-module(license_trigger).
-author("kenneth").

%% API
-export([do/1]).


do(Req0) ->
    {ok, Body, Req} = dgiot_req:read_body(Req0),
    _Data = jsx:decode(Body, [{labels, binary}, return_maps]),
    Req1 = dgiot_req:reply(500, #{
        <<"content-type">> => <<"application/json; charset=utf-8">>
    }, jsx:encode(#{error => <<>>}), Req),
    {ok, Req1}.
