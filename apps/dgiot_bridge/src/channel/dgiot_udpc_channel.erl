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
-module(dgiot_udpc_channel).
-behavior(dgiot_channelx).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"UDPC">>).
-record(state, {id, env}).
%% API
-export([
    start/2
]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

-channel(?TYPE).
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"UDPC资源通道"/utf8>>
    },
    description => #{
        zh => <<"UDPC资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ip">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1"/utf8>>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 3456,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"login">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"ALLSTATUS;">>,
        title => #{
            zh => <<"登录报文"/utf8>>
        },
        description => #{
            zh => <<"登录报文"/utf8>>
        }
    },
    <<"module">> => #{
        order => 4,
        type => enum,
        required => false,
        default => <<"dgiot_udp"/utf8>>,
        enum => [
            #{<<"value">> => <<"dgiot_udp">>, <<"label">> => <<"dgiot_udp"/utf8>>},
            #{<<"value">> => <<"dgiot_sonbs">>, <<"label">> => <<"dgiot_sonbs"/utf8>>},
            #{<<"value">> => <<"dgiot_feixing">>, <<"label">> => <<"dgiot_feixing"/utf8>>}
        ],
        title => #{
            zh => <<"回调模块"/utf8>>
        },
        description => #{
            zh => <<"回调模块"/utf8>>
        }
    },
    <<"hb">> => #{
        order => 5,
        type => integer,
        required => true,
        default => 60,
        title => #{
            zh => <<"心跳"/utf8>>
        },
        description => #{
            zh => <<"心跳(S)"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/UdpIcon.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).


%% 通道初始化
init(?TYPE, ChannelId, Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    {ok, State, []}.

handle_init(#state{env = Args} = State) ->
    #{<<"product">> := Products,
        <<"ip">> := Ip,
        <<"port">> := Port} = Args,
    lists:map(fun({ProductId, _Opt}) ->
        start_client(ProductId, Ip, Port, Args)
              end, Products),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "channel ~p", [Event]),
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.


start_client(ProductId, Ip, Port,
    #{<<"hb">> := Hb, <<"login">> := Login, <<"module">> := Module}) ->
    dgiot_udpc_worker:start_connect(#{
        <<"auto_reconnect">> => 10,
        <<"reconnect_times">> => 3,
        <<"ip">> => Ip,
        <<"port">> => Port,
        <<"productid">> => ProductId,
        <<"hb">> => Hb,
        <<"login">> => Login,
        <<"module">> => Module
    }).

%%start_client(ProductId, Ip, Port,
%%    #{<<"page_index">> := PageIndex, <<"page_size">> := PageSize, <<"total">> := Total, <<"hb">> := Hb, <<"login">> := Login, <<"module">> := Module}) ->
%%    Success = fun(Page) ->
%%        lists:map(fun(X) ->
%%            case X of
%%                #{<<"devaddr">> := DevAddr} ->
%%                    dgiot_udpc_worker:start_connect(#{
%%                        <<"auto_reconnect">> => 10,
%%                        <<"reconnect_times">> => 3,
%%                        <<"ip">> => Ip,
%%                        <<"port">> => Port,
%%                        <<"productid">> => ProductId,
%%                        <<"hb">> => Hb,
%%                        <<"devaddr">> => DevAddr,
%%                        <<"login">> => Login,
%%                        <<"module">> => Module
%%                    });
%%                _ ->
%%                    ok
%%            end
%%                  end, Page)
%%              end,
%%    Query = #{
%%        <<"where">> => #{<<"product">> => ProductId}
%%    },
%%    dgiot_parse_loader:start(<<"Device">>, Query, PageIndex, PageSize, Total, Success).
