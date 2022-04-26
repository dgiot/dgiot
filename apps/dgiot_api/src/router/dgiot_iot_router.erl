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

-module(dgiot_iot_router).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([init/2]).

%% Static Callback
-export([malformed_request/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([last_modified/2]).
-export([generate_etag/2]).
-export([get_file/2]).

init(Req, {iot, DocRoot}) ->
    io:format("~s ~p ~p ~n",[?FILE, ?LINE, cowboy_req:path(Req)]),
    <<"/iot/", Path/binary>> = cowboy_req:path(Req),
    Index = dgiot_httpc:url_join([DocRoot, Path]),
    dgiot_router:init(Req, {file, Index, []});

init(Req, {product, DocRoot}) ->
    _Path = cowboy_req:path(Req),
    _Product = dgiot_req:binding(<<"Product">>, Req),
    Index = dgiot_httpc:url_join([DocRoot, "index.html"]),
    dgiot_router:init(Req, {file, Index, []}).


malformed_request(Req, State) ->
    cowboy_static:malformed_request(Req, State).

forbidden(Req, State) ->
    cowboy_static:forbidden(Req, State).

content_types_provided(Req, State) ->
    cowboy_static:content_types_provided(Req, State).

resource_exists(Req, State) ->
    cowboy_static:resource_exists(Req, State).

generate_etag(Req, State) ->
    cowboy_static:generate_etag(Req, State).

last_modified(Req, State) ->
    cowboy_static:last_modified(Req, State).

get_file(Req, State) ->
    cowboy_static:get_file(Req, State).
