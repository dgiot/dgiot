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

-module(dgiot_device_channel).
-behavior(dgiot_channelx).
-author("kenneth").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-define(TYPE, <<"DEVICE">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {id, mod, product, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    priority => 2,
    title => #{
        zh => <<"Device缓存通道"/utf8>>
    },
    description => #{
        zh => <<"Device缓存通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"order">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"createdAt"/utf8>>,
        title => #{
            zh => <<"排序"/utf8>>
        },
        description => #{
            zh => <<"排序"/utf8>>
        }
    },
    <<"offline">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 180,
        title => #{
            zh => <<"离线超时时间(秒)"/utf8>>
        },
        description => #{
            zh => <<"离线超时时间(秒)"/utf8>>
        }
    },
    <<"checktime">> => #{
        order => 3,
        type => integer,
        required => true,
        default => 3,
        title => #{
            zh => <<"设备状态落库周期(分)"/utf8>>
        },
        description => #{
            zh => <<"设备状态落库周期(分)"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/device_profile.png">>,
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
    dgiot_parse_hook:subscribe(<<"Product/*">>, get, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product">>, get, ChannelId),
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, [<<"isEnable">>], ChannelId),
    dgiot_parse_hook:subscribe(<<"Product">>, put, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product">>, post, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product/*">>, delete, ChannelId),
    {ok, State, []}.

handle_init(State) ->
    erlang:send_after(5000, self(), {message, <<"_Pool">>, load}),
    erlang:send_after(3 * 60 * 1000, self(), {message, <<"_Pool">>, check}),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.

handle_message(load, #state{env = #{<<"order">> := _Order, <<"offline">> := OffLine}} = State) ->
    dgiot_data:insert({device, offline}, OffLine),
    {ok, State};

handle_message(check, #state{env = #{<<"offline">> := OffLine, <<"checktime">> := CheckTime}} = State) ->
    erlang:send_after(CheckTime * 60 * 1000, self(), {message, <<"_Pool">>, check}),
    dgiot_device:sync_parse(OffLine),
    {ok, State};

handle_message({sync_parse, Pid, 'after', get, Header, <<"Product">>, #{<<"results">> := _Results} = ResBody}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid,Header]),
    Key = dgiot_device_static:get_count(Header),
    timer:sleep(100),
    NewResBody = dgiot_device_static:stats(ResBody, Key),
    dgiot_parse_hook:publish(Pid, NewResBody),
    {ok, State};

handle_message({sync_parse, Pid, 'after', get, _Header, <<"Product">>, #{<<"objectId">> := _ObjectId} = ResBody}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, ObjectId]),
    dgiot_parse_hook:publish(Pid, ResBody),
    {ok, State};

handle_message({sync_parse, Pid, 'after', put, _Header, <<"Device">>,  QueryData}, State) ->
    io:format("~s ~p ~p  ~p ~n", [?FILE, ?LINE, Pid,  QueryData]),
    dgiot_device:put(QueryData),
    {ok, State};

handle_message({sync_parse, Pid, 'after', post, _Header, <<"Product">>, QueryData}, State) ->
    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, QueryData]),
    dgiot_product_hook:post('after', QueryData),
    {ok, State};

handle_message({sync_parse, Pid, 'after', put, _Header, <<"Product">>, QueryData}, State) ->
    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, QueryData]),
    dgiot_product_hook:put('after', QueryData),
    {ok, State};

handle_message({sync_parse, Pid, 'after', delete, _Header, <<"Product">>, ObjectId}, State) ->
    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, ObjectId]),
    dgiot_product_hook:delete('after', ObjectId),
    {ok, State};

handle_message(Message, State) ->
    ?LOG(info, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.

