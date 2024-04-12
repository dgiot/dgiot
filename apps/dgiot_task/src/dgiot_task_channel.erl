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

-module(dgiot_task_channel).
-behavior(dgiot_channelx).
-author("jonhliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_task.hrl").
-include_lib("dgiot/include/logger.hrl").
-record(state, {id, mod, products, env = #{}}).

-dgiot_data("ets").
-export([init_ets/0]).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"指令任务通道"/utf8>>
    },
    description => #{
        zh => <<"指令任务通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"freq">> => #{
        order => 1,
        type => integer,
        required => false,
        default => 180,
        title => #{
            zh => <<"采集频率/秒"/utf8>>
        },
        description => #{
            zh => <<"网关及网关下所有子设备的采集频率/秒"/utf8>>
        }
    },
    <<"start_time">> => #{
        order => 2,
        type => string,
        required => false,
        default => <<"2020-03-26 10:35:10"/utf8>>,
        title => #{
            zh => <<"任务开始时间"/utf8>>
        },
        description => #{
            zh => <<"任务开始时间"/utf8>>
        }
    },
    <<"end_time">> => #{
        order => 3,
        type => string,
        required => false,
        default => <<"2025-05-28 10:35:10"/utf8>>,
        title => #{
            zh => <<"任务结束时间"/utf8>>
        },
        description => #{
            zh => <<"任务结束时间"/utf8>>
        }
    },
    <<"rand">> => #{
        order => 4,
        type => boolean,
        required => true,
        default => true,
        title => #{
            zh => <<"是否错峰执行任务"/utf8>>
        },
        description => #{
            zh => <<"每轮任务开始时,是否随机开始,错峰处理"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/TaskIcon.png">>,
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

init_ets() ->
    dgiot_data:init(?DGIOT_TASK),
    dgiot_data:init(?DGIOT_DATA_CACHE),
    dgiot_data:init(?DGIOT_PNQUE).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{<<"product">> := Products} = Args) ->
    #{<<"freq">> := Freq, <<"start_time">> := Start_time, <<"end_time">> := End_time} = Args,
    Rand = maps:get(<<"rand">>, Args, true),
    dgiot_client:add_clock(ChannelId, Start_time, End_time),
    State = #state{id = ChannelId, products = get_productids(Products)},
    {ok, State, dgiot_client:register(ChannelId, task_sup, #{
        <<"channel">> => ChannelId,
        <<"starttime">> => dgiot_datetime:localtime_to_unixtime(dgiot_datetime:to_localtime(Start_time)),
        <<"endtime">> => dgiot_datetime:localtime_to_unixtime(dgiot_datetime:to_localtime(End_time)),
        <<"freq">> => Freq,
        <<"rand">> => Rand
    })}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "channel ~p", [Event]),
    {ok, State}.

handle_message(start_client, #state{id = ChannelId, products = Products} = State) ->
%%    io:format("~s ~p ChannelId = ~p.~n", [?FILE, ?LINE, ChannelId]),
    case dgiot_data:get({start_client, binary_to_atom(ChannelId)}) of
        not_find ->
            dgiot_task:start(ChannelId, Products),
            dgiot_data:insert({start_client, ChannelId}, ChannelId),
            erlang:send_after(1000 * 60 * 1, self(), check_newdevice);
        _ ->
            pass
    end,
    {ok, State};

handle_message(stop_client, #state{id = ChannelId} = State) ->
%%    io:format("~s ~p ChannelId = ~p.~n", [?FILE, ?LINE, ChannelId]),
    dgiot_client:stop(ChannelId),
    dgiot_data:insert({stop_client, ChannelId}, ChannelId),
    {ok, State};

handle_message(check_newdevice, #state{id = ChannelId, products = Products} = State) ->
%%    io:format("~s ~p time ~p ChannelId = ~p.~n", [?FILE, ?LINE, dgiot_datetime:format(dgiot_datetime:now_secs(), <<"YY-MM-DD HH:NN:SS">>), ChannelId]),
    case dgiot_data:get({stop_client, binary_to_atom(ChannelId)}) of
        not_find ->
            dgiot_task:start(ChannelId, Products),
            erlang:send_after(1000 * 60 * 1, self(), check_newdevice);
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, ChannelId, _State) ->
    dgiot_task:del_client(ChannelId),
    dgiot_client:stop(ChannelId),
    ok.

get_productids(Products) ->
    lists:foldl(fun({ProductId, _}, Acc) ->
        Acc ++ [ProductId]
                end, [], Products).
