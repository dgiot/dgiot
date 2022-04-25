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

-module(dgiot_http_worker).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("johnliu").

-record(state, {id, env, type = stream}).

%% API
-export([childSpec/3]).

childSpec(Name, ChannelId, #{<<"port">> := Port} = ChannelArgs) ->
    State = #state{
        id = ChannelId,
        env = maps:without([<<"port">>, <<"path">>, <<"product">>, <<"behaviour">>], ChannelArgs)
    },
    Opts = [
        {ip, {0, 0, 0, 0}},
        {port, Port}
    ],
    SSL = maps:with([<<"cacertfile">>, <<"certfile">>, <<"keyfile">>], ChannelArgs),
    {Transport, TransportOpts} =
        case maps:to_list(SSL) of
            [] ->
                {ranch_tcp, Opts};
            SslOpts = [_ | _] ->
                {ranch_ssl, Opts ++ SslOpts}
        end,
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, dgiot_bridge, "www/index.html"}},
            {"/websocket/[...]", dgiot_ws_h, State},
            {"/static/[...]", cowboy_static, {priv_dir, dgiot_bridge, "www/static"}},
            {"/api/[...]", dgiot_rest_h, State},
            {"/http2ws/[...]", dgiot_http2ws_h, State},
            {"/[...]", dgiot_http2ws_h, State}
        ]}
    ]),
    CowboyOpts = #{
        env => #{
            dispatch => Dispatch
        }
    },
    ranch:child_spec(Name, 300, Transport, TransportOpts, cowboy_clear, CowboyOpts).