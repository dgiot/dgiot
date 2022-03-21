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
-module(dgiot_httpc_channel).
-behavior(dgiot_channelx).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"HTTPC">>).
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
        zh => <<"HTTPC资源通道"/utf8>>
    },
    description => #{
        zh => <<"HTTPC资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"url">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"http://127.0.0.1:5080"/utf8>>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"page_index">> => #{
        order => 3,
        type => integer,
        required => false,
        default => 1,
        title => #{
            zh => <<"起始记录号"/utf8>>
        },
        description => #{
            zh => <<"起始记录号"/utf8>>
        }
    },
    <<"page_size">> => #{
        order => 4,
        type => integer,
        required => false,
        default => 1,
        title => #{
            zh => <<"每页记录数"/utf8>>
        },
        description => #{
            zh => <<"每页记录数"/utf8>>
        }
    },
    <<"total">> => #{
        order => 5,
        type => integer,
        required => false,
        default => 1,
        title => #{
            zh => <<"总计页数"/utf8>>
        },
        description => #{
            zh => <<"总计页数"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/TcpIcon.jpeg">>,
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
    {ok, State, dgiot_httpc_worker:childSpec()}.

handle_init(#state{id = ChannelId, env = Args} = State) ->
    #{<<"product">> := Products, <<"url">> := Url} = Args,
    lists:map(fun({ProductId, _Opt}) ->
        start_client(ChannelId, ProductId, Url, Args)
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

start_client(ChannelId, ProductId, Url, #{<<"page_index">> := PageIndex,
    <<"page_size">> := PageSize, <<"total">> := Total}) ->
    Success = fun(Page) ->
        lists:map(fun(X) ->
            case X of
                #{<<"devaddr">> := DevAddr} ->
                    ?LOG(info,"DevAddr ~p",[DevAddr]),
                    dgiot_httpc_worker:start(#{
                        <<"url">> => Url,
                        <<"channelid">> => ChannelId,
                        <<"productid">> => ProductId,
                        <<"devaddr">> => DevAddr
                    });
                _ ->
                    ok
            end
                  end, Page)
              end,
    Query = #{
        <<"where">> => #{<<"product">> => ProductId}
    },
    dgiot_parse_loader:start(<<"Device">>, Query, PageIndex, PageSize, Total, Success).