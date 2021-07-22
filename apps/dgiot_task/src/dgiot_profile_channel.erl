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

-module(dgiot_profile_channel).
-behavior(dgiot_channelx).
-author("jonhliu").
-include("dgiot_task.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"PROFILE">>).
-record(state, {id, mod, product, env = #{}}).

-dgiot_data("ets").
-export([init_ets/0]).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"配置同步通道"/utf8>>
    },
    description => #{
        zh => <<"设备配置同步通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"vcaddr">> => #{
        order => 1,
        type => string,
        required => false,
        default => <<"all"/utf8>>,
        title => #{
            zh => <<"网关逻辑地址"/utf8>>
        },
        description => #{
            zh => <<"网关逻辑地址"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/%E6%8C%87%E4%BB%A4%E4%BB%BB%E5%8A%A1%E5%9B%BE%E6%A0%87.png">>,
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
    dgiot_data:init(?TASK_ARGS),
    dgiot_data:init(?DGIOT_PNQUE).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, Args) ->
    #{<<"product">> := Products} = Args,
    NewArgs = maps:without([<<"product">>], Args),
    State = #state{id = ChannelId, env = #{
        <<"products">> => Products,
        <<"args">> => NewArgs}
    },
    {ok, State, []}.

handle_init(#state{id = ChannelId, env = #{<<"products">> := Products, <<"args">> := Args}} = State) ->
    lists:map(fun({ProductId, #{<<"ACL">> := Acl}}) ->
        Predicate = fun(E) ->
            case E of
                <<"role:", _/binary>> -> true;
                _ -> false
            end
                    end,
        [<<"role:", App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
        dgiot_data:set_consumer(<<"taskdelay/", ChannelId/binary>>, 100),
        NewArgs = Args#{
            <<"app">> => App,
            <<"product">> => ProductId,
            <<"channel">> => ChannelId},
        dgiot_data:insert({?TASK_ARGS, ProductId}, NewArgs),
        dgiot_task:load(NewArgs)
              end, Products),
    dgiot_task:timing_start(Args#{<<"channel">> => ChannelId}),
    dgiot_parse:subscribe(<<"Device">>, delete),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "channel ~p", [Event]),
    {ok, State}.

handle_message({sync_parse, Args}, State) ->
    case jsx:decode(Args, [return_maps]) of
        #{<<"objectId">> := DtuId} ->
%%            从队列删除该设备
            dgiot_task:del_pnque(DtuId);
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    ?LOG(info, "_Message ~p", [_Message]),
    {ok, State}.

stop(ChannelType, ChannelId, #state{env = #{<<"product">> := ProductId, <<"args">> := Args}} = _State) ->
    spawn(fun() ->
        dgiot_task:stop(Args#{<<"product">> => ProductId, <<"channel">> => ChannelId})
          end),
    ?LOG(warning, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

