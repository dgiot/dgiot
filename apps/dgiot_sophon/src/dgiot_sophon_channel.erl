%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_sophon_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_sophon.hrl").
-define(TYPE, <<"SOPHON">>).
%%-dgiot_channel(?MODULE).

%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"sophon测试通道"/utf8>>
    },
    description => #{
        zh => <<"sophon测试通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 20660,

        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"freq">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 10,
        title => #{
            zh => <<"推流间隔/秒"/utf8>>
        },
        description => #{
            zh => <<"推流间隔/秒"/utf8>>
        }
    },
    <<"pushcount">> => #{
        order => 3,
        type => integer,
        required => true,
        default => 3,
        title => #{
            zh => <<"推流路数"/utf8>>
        },
        description => #{
            zh => <<"推流路数"/utf8>>
        }
    },
    <<"push">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"ffmpeg -rtsp_transport tcp -re -stimeout 5000000 -i rtsp://{{username}}:{{password}}@{{ip}}{{channel}} -q 5 -f rtsp rtsp://127.0.0.1:8554/stream"/utf8>>,
        title => #{
            zh => <<"推流地址"/utf8>>
        },
        description => #{
            zh => <<"推流地址"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/sophon_channel.png">>,
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
init(?TYPE, ChannelId, #{
    <<"product">> := Products
} = Args) ->
    {ProductId, App} =
        case get_app(Products) of
            [{ProductId1, App1} | _] ->
                {ProductId1, App1};
            [] ->
                {<<>>, <<>>};
            _ ->
                {<<>>, <<>>}
        end,
    Que =
        case dgiot_parsex:query_object(<<"Device">>, #{<<"limit">> => 10000, <<"keys">> => [<<"devaddr">>, <<"profile">>, <<"name">>, <<"product">>],
            <<"where">> => #{<<"product">> => ProductId, <<"profile.control">> => <<"start">>}}) of
            {ok, #{<<"results">> := Results}} ->
                Results;
            _ ->
                []
        end,
    State = #state{
        id = ChannelId,
        env = maps:without([<<"product">>], Args),
        app = App,
        product = ProductId,
        que = Que
    },
    {ok, State, []}.

handle_init(State) ->
    erlang:send_after(3 * 1000, self(), push_rtsp),
    erlang:send_after(5 * 60 * 1000, self(), check_push),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, State) ->
    ?LOG(error, "EventId ~p Event ~p", [EventId, Event]),
    {ok, State}.

handle_message(check_push, #state{id = ChannelId, que = Que, env = #{<<"push">> := Push}} = State) ->
    TaskQue =
        lists:foldl(fun
                        (#{<<"objectId">> := DeviceId, <<"profile">> := #{<<"camera_type">> := <<"hikvision">>, <<"streamname">> := Streamname}} = Device, Acc) ->
                            case dgiot_rtsp2ws:get_ospid(DeviceId, Streamname) of
                                Pid when is_pid(Pid) ->
                                    case is_process_alive(Pid) of
                                        true ->
                                            Acc;
                                        _ ->
                                            Acc ++ [Device]
                                    end;
                                _ ->
                                    Acc ++ [Device]
                            end;
                        (_, Acc) ->
                            Acc
                    end, [], Que),
    dgiot_sophon:send_push(ChannelId, TaskQue, Push),
    erlang:send_after(10 * 60 * 1000, self(), check_push),
    {noreply, State};

handle_message(push_rtsp, #state{id = ChannelId, que = Que, env = #{<<"pushcount">> := Pushcount, <<"freq">> := Freq, <<"push">> := Push}} = State) when length(Que) > Pushcount ->
    {NewQue, TaskQue} =
        case catch lists:split(Pushcount, Que) of
            {'EXIT', _} ->
                {Que, Que};
            {First, Second} ->
                erlang:send_after(Freq * 1000, self(), push_rtsp),
                {Second ++ First, First};
            _ ->
                {Que, Que}
        end,
    dgiot_sophon:send_push(ChannelId, TaskQue, Push, polling),
    {ok, State#state{que = NewQue}};

handle_message(push_rtsp, #state{id = ChannelId, que = Que, env = #{<<"push">> := Push}} = State) ->
    dgiot_sophon:send_push(ChannelId, Que, Push),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, ChannelId, #state{que = Que}) ->
    lists:foldl(fun(#{<<"objectId">> := DeviceId}, _) ->
        dgiot_rtsp2ws:stop(ChannelId, DeviceId)
                end, {}, Que),
    ok.

get_app(Products) ->
    lists:map(fun({ProductId, #{<<"ACL">> := Acl}}) ->
        Predicate = fun(E) ->
            case E of
                <<"role:", _/binary>> -> true;
                _ -> false
            end
                    end,
        App =
            case lists:filter(Predicate, maps:keys(Acl)) of
                [<<"role:", Name/binary>> | _] ->
                    Name;
                _ ->
                    <<"dgiot">>
            end,
        {ProductId, App}
              end, Products).
