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
-module(http_decoder).
-author("kenneth").
-behavior(dgiot_decoder).
-export([handle_info/2]).


handle_info({http, Req}, State) ->
    Path = dgiot_req:path(Req),
    Method = dgiot_req:method(Req),
    ?LOG(info,"~p ~p", [Method, Path]),
    handle_info(Path, Req, State).


handle_info(<<"/test">>, _Req, _State) ->
    {reply, {200, <<"Hello World">>}};

handle_info(<<"/test1">>, _Req, State) ->
    Header = #{
        <<"content-type">> => <<"application/json; charset=utf-8">>
    },
    {reply, {400, Header, <<"Forbidden">>}, State}.
