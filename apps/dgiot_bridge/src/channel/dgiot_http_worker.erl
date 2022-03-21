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

-record(state, {id, env}).

%% API
-export([childSpec/3,
    init/2]).

childSpec(Name, ChannelId, #{<<"port">> := Port} = ChannelArgs) ->
    State = #state{
        id = ChannelId,
        env = maps:without([<<"port">>,<<"path">>,<<"product">>,<<"behaviour">>], ChannelArgs)
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
    Route = get_route(maps:get(<<"path">>, ChannelArgs, <<>>)),
    Dispatch = cowboy_router:compile([
        {'_', [
            {Route, ?MODULE, State}
        ]}
    ]),
    CowboyOpts = #{
        env =>#{
            dispatch => Dispatch
        }
    },
  ranch:child_spec(Name, 300, Transport, TransportOpts, cowboy_clear, CowboyOpts).

%% ====== http callback ======
init(Req, #state{ id = ChannelId, env = Env} = State) ->
    {ok, _Type, ProductIds} = dgiot_bridge:get_products(ChannelId),
    case dgiot_bridge:apply_channel(ChannelId, ProductIds, handle_info, [{http, Req}], Env) of
        {ok, NewEnv} ->
            Req1 = cowboy_req:reply(200, #{}, <<"hello word">>, Req),
            {ok, Req1, State#state{env = NewEnv}};
        {reply, _ProductId, {HTTPCode, Reply}, NewEnv} ->
            Req1 = cowboy_req:reply(HTTPCode, #{}, Reply, Req),
            {ok, Req1, State#state{env = NewEnv}};
        {reply, _ProductId, {HTTPCode, Header, Reply}, NewEnv} ->
            Req1 = cowboy_req:reply(HTTPCode, Header, Reply, Req),
            {ok, Req1, State#state{env = NewEnv}}
    end.

get_route(<<"http://", Path>>) ->
    get_route(Path);
get_route(Path) when is_binary(Path) ->
    binary_to_list(Path);
get_route(_) ->
    "/[...]".