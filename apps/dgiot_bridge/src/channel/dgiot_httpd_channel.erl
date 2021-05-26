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

-module(dgiot_httpd_channel).
-behavior(dgiot_channelx).
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"HTTPD">>).
-author("kenneth").
-record(state, {id, env}).
%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, stop/3]).

-export([init/2]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"HTTP采集通道"/utf8>>
    },
    description => #{
        zh => <<"HTTP采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 8080,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"path">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"/test">>,
        title => #{
            zh => <<"路径"/utf8>>
        },
        description => #{
            zh => <<"路径"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).


%% 通道初始化
init(?TYPE, ChannelId, #{<<"port">> := Port} = ChannelArgs) ->
    State = #state{
        id = ChannelId,
        env = maps:without([<<"port">>,<<"path">>,<<"product">>,<<"behaviour">>], ChannelArgs)
    },
    Name = dgiot_channelx:get_name(?TYPE, ChannelId),
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
    ChildSpec = ranch:child_spec(Name, 300, Transport, TransportOpts, cowboy_clear, CowboyOpts),
    {ok, State, ChildSpec}.


%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info,"channel ~p, ~p", [EventId, Event]),
    ok.

handle_message(Message, State) ->
    ?LOG(info,"channel ~p ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _) ->
    ?LOG(info,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.


%% ====== http callback ======

init(Req, #state{ id = ChannelId, env = Env} = State) ->
    {ok, _Type, ProductIds} = dgiot_bridge:get_products(ChannelId),
    case dgiot_bridge:apply_channel(ChannelId, ProductIds, handle_info, [{http, Req}], Env) of
        {ok, NewEnv} ->
            Req1 = cowboy_req:reply(200, #{}, <<>>, Req),
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
