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
    <<"host">> => #{
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
    <<"path">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"/iotapi/login"/utf8>>,
        title => #{
            zh => <<"请求路径"/utf8>>
        },
        description => #{
            zh => <<"请求路径"/utf8>>
        }
    },
    <<"header">> => #{
        order => 2,
        type => object,
        allowCreate => true,
        required => true,
        default => [
            #{<<"value">> => "lable", <<"name">> => <<"key">>}
        ],
        title => #{
            zh => <<"请求头"/utf8>>
        },
        description => #{
            zh => <<"请求头"/utf8>>
        },
        <<"table">> => #{
            <<"key">> => #{
                key => <<"key">>,
                order => 1,
                type => string,
                required => true,
                default => #{<<"value">> => <<"Content-Type">>, <<"label">> => <<"Content-Type"/utf8>>},
                enum => [
                    #{<<"value">> => <<"accept">>, <<"label">> => <<"accept"/utf8>>},
                    #{<<"value">> => <<"Content-Type">>, <<"label">> => <<"Content-Type"/utf8>>}
                ],
                title => #{
                    zh => <<"请求头"/utf8>>
                },
                description => #{
                    zh => <<"请求头"/utf8>>
                }
            },
            <<"value">> => #{
                key => <<"value">>,
                order => 2,
                type => string,
                required => true,
                default => #{<<"value">> => <<"accept">>, <<"label">> => <<"application/json"/utf8>>},
                enum => [
                    #{<<"value">> => <<"application/json">>, <<"label">> => <<"application/json"/utf8>>},
                    #{<<"value">> => <<"text/plain">>, <<"label">> => <<"text/plain"/utf8>>}
                ],
                title => #{
                    zh => <<"请求头参数"/utf8>>
                },
                description => #{
                    zh => <<"请求头参数"/utf8>>
                }
            }
        }
    },
    <<"method">> => #{
        order => 3,
        type => string,
        required => true,
        enum => [
            #{<<"value">> => <<"GET">>, <<"label">> => <<"GET"/utf8>>},
            #{<<"value">> => <<"POST">>, <<"label">> => <<"POST"/utf8>>},
            #{<<"value">> => <<"PUT">>, <<"label">> => <<"PUT"/utf8>>},
            #{<<"value">> => <<"PATCH">>, <<"label">> => <<"PATCH"/utf8>>},
            #{<<"value">> => <<"DELETE">>, <<"label">> => <<"DELETE"/utf8>>},
            #{<<"value">> => <<"HARD">>, <<"label">> => <<"HARD"/utf8>>},
            #{<<"value">> => <<"CONNECT">>, <<"label">> => <<"CONNECT"/utf8>>},
            #{<<"value">> => <<"OPTIONS">>, <<"label">> => <<"OPTIONS"/utf8>>},
            #{<<"value">> => <<"TRACE">>, <<"label">> => <<"TRACE"/utf8>>},
            #{<<"value">> => <<"CUSTOM">>, <<"label">> => <<"CUSTOM"/utf8>>}
        ],
        default => [
            #{<<"value">> => "GET", <<"label">> => <<"GET">>}
        ],
        title => #{
            zh => <<"请求方法"/utf8>>
        },
        description => #{
            zh => <<"请求方法"/utf8>>
        }
    },
    <<"body">> => #{
        order => 4,
        type => object,
        allowCreate => true,
        title => #{
            zh => <<"请求参数"/utf8>>
        },
        description => #{
            zh => <<"请求参数"/utf8>>
        },
        <<"table">> => #{
            <<"key">> => #{
                key => <<"key">>,
                order => 1,
                type => string,
                default => <<"">>,
                title => #{
                    zh => <<"参数名称"/utf8>>
                },
                description => #{
                    zh => <<"参数名称"/utf8>>
                }
            },
            <<"value">> => #{
                key => <<"value">>,
                order => 2,
                type => string,
                default => <<"">>,
                title => #{
                    zh => <<"参数值"/utf8>>
                },
                description => #{
                    zh => <<"参数值"/utf8>>
                }
            }
        }
    },
    <<"contenttype">> => #{
        order => 7,
        type => string,
        required => false,
        default => #{<<"value">> => <<"text/plain">>, <<"label">> => <<"text/plain"/utf8>>},
        enum => [
            #{<<"value">> => <<"text/plain">>, <<"label">> => <<"text/plain"/utf8>>}
        ],
        title => #{
            zh => <<"起始记录号"/utf8>>
        },
        description => #{
            zh => <<"起始记录号"/utf8>>
        }
    },
    <<"freq">> => #{
        order => 8,
        type => integer,
        required => false,
        default => 180,
        title => #{
            zh => <<"采集频率/秒"/utf8>>
        },
        description => #{
            zh => <<"采集频率/秒"/utf8>>
        }
    },
    <<"page_index">> => #{
        order => 9,
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
        order => 10,
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
        order => 11,
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
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/HTTP-collection.png">>,
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
    dgiot_httpc_worker:set_host(ChannelId, Args),
    dgiot_httpc_worker:set_path(ChannelId, Args),
    dgiot_httpc_worker:set_method(ChannelId, Args),
    dgiot_httpc_worker:set_contenttype(ChannelId, Args),
    dgiot_httpc_worker:set_header(ChannelId, Args),
    dgiot_httpc_worker:set_body(ChannelId, Args),
    {ok, State, dgiot_httpc_sup:childSpec(ChannelId)}.

handle_init(#state{id = ChannelId, env = Args} = State) ->
    #{<<"product">> := Products} = Args,
    lists:map(fun({ProductId, _Opt}) ->
        start_client(ChannelId, ProductId, Args)
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

start_client(ChannelId, ProductId, #{<<"freq">> := Freq , <<"page_index">> := PageIndex,
    <<"page_size">> := PageSize, <<"total">> := Total} ) ->
    Success = fun(Page) ->
        lists:map(fun(X) ->
            case X of
                #{<<"devaddr">> := DevAddr} ->
                    dgiot_httpc_sup:start(#{
                        <<"channelid">> => ChannelId,
                        <<"productid">> => ProductId,
                        <<"devaddr">> => DevAddr,
                        <<"freq">> => Freq
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
