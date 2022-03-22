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

-module(dgiot_http_channel).
-behavior(dgiot_channelx).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"HTTP">>).
-author("johnliu").

-record(state, {id, env}).
%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"HTTP采集通道"/utf8>>
    },
    description => #{
        zh => <<"HTTP采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"header">> => #{
        order => 1,
        type => object,
        allowCreate => true,
        required => true,
        default => [
            #{<<"value">> => "lable", <<"name">> => <<"key">>}
        ],
        title => #{
            zh => <<"数据标识"/utf8>>
        },
        description => #{
            zh => <<"数据标识"/utf8>>
        },
        <<"table">> => #{
            <<"key">> => #{
                key => <<"key">>,
                order => 1,
                type => string,
                required => true,
                default => <<"数据标识"/utf8>>,
                title => #{
                    zh => <<"数据标识"/utf8>>
                },
                description => #{
                    zh => <<"数据标识"/utf8>>
                }
            },
            <<"value">> => #{
                key => <<"value">>,
                order => 2,
                type => string,
                required => true,
                default => <<"0000"/utf8>>,
                title => #{
                    zh => <<"数据内容"/utf8>>
                },
                description => #{
                    zh => <<"数据内容"/utf8>>
                }
            },
            <<"type">> => #{
                key => <<"type">>,
                order => 3,
                type => string,
                required => true,
                default => #{<<"value">> => <<"bit">>, <<"label">> => <<"位"/utf8>>},
                enum => [
                    #{<<"value">> => <<"bit">>, <<"label">> => <<"位"/utf8>>},
                    #{<<"value">> => <<"short16_AB">>, <<"label">> => <<"16位 有符号(AB)"/utf8>>},
                    #{<<"value">> => <<"short16_BA">>, <<"label">> => <<"16位 有符号(BA)"/utf8>>},
                    #{<<"value">> => <<"ushort16_AB">>, <<"label">> => <<"16位 无符号(AB)"/utf8>>},
                    #{<<"value">> => <<"ushort16_BA">>, <<"label">> => <<"16位 无符号(BA)"/utf8>>},
                    #{<<"value">> => <<"long32_ABCD">>, <<"label">> => <<"32位 有符号(ABCD)"/utf8>>},
                    #{<<"value">> => <<"long32_CDAB">>, <<"label">> => <<"32位 有符号(CDAB)"/utf8>>},
                    #{<<"value">> => <<"ulong32_ABCD">>, <<"label">> => <<"32位 无符号(ABCD)"/utf8>>},
                    #{<<"value">> => <<"ulong32_CDAB">>, <<"label">> => <<"32位 无符号(CDAB)"/utf8>>},
                    #{<<"value">> => <<"float32_ABCD">>, <<"label">> => <<"32位 浮点数(ABCD)"/utf8>>},
                    #{<<"value">> => <<"float32_CDAB">>, <<"label">> => <<"32位 浮点数(CDAB)"/utf8>>}
                ],
                title => #{
                    zh => <<"数据格式"/utf8>>
                },
                description => #{
                    zh => <<"数据格式"/utf8>>
                }
            }
        }
    },
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 3080,
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
init(?TYPE, ChannelId, ChannelArgs) ->
    State = #state{
        id = ChannelId,
        env = maps:without([<<"port">>,<<"path">>,<<"product">>,<<"behaviour">>], ChannelArgs)
    },
    Name = dgiot_channelx:get_name(?TYPE, ChannelId),
    {ok, State, dgiot_http_worker:childSpec(Name, ChannelId, ChannelArgs)}.


%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info,"channel ~p, ~p", [EventId, Event]),
    ok.

handle_message(Message, State) ->
    ?LOG(info,"channel ~p ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.
