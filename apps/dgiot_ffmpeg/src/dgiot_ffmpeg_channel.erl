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
-module(dgiot_ffmpeg_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include("dgiot_ffmpeg.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"FFMPEG">>).
-record(state, {id, env}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

-channel(?TYPE).
-channel_type(#{
    type => 2,
    title => #{
        zh => <<"视频采集资源通道"/utf8>>
    },
    description => #{
        zh => <<"视频采集资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"push">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"rtmp://livepush.iotn2n.com/live/"/utf8>>,
        title => #{
            zh => <<"推流地址"/utf8>>
        },
        description => #{
            zh => <<"推流地址"/utf8>>
        }
    },
    <<"play">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"http://liveplay.iotn2n.com/live/"/utf8>>,
        title => #{
            zh => <<"直播地址"/utf8>>
        },
        description => #{
            zh => <<"直播地址"/utf8>>
        }
    },
    <<"pushkey">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"88a90279202da01c7202435e89a4e66d"/utf8>>,
        title => #{
            zh => <<"推流Key"/utf8>>
        },
        description => #{
            zh => <<"推流Key"/utf8>>
        }
    },
    <<"PARAM">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<" -r 15 -s 640*480 -an -vcodec h264 -max_delay 100 -g 5 -b 700000 "/utf8>>,
        title => #{
            zh => <<"视频参数"/utf8>>
        },
        description => #{
            zh => <<"视频参数"/utf8>>
        }
    },
    <<"IPCUSER">> => #{
        order => 5,
        type => string,
        required => true,
        default => <<"admin"/utf8>>,
        title => #{
            zh => <<"摄像头用户"/utf8>>
        },
        description => #{
            zh => <<"摄像头登录用户名"/utf8>>
        }
    },
    <<"IPCPWD">> => #{
        order => 6,
        type => string,
        required => true,
        default => <<"shuwafly2020"/utf8>>,
        title => #{
            zh => <<"访问密码"/utf8>>
        },
        description => #{
            zh => <<"摄像头访问密码"/utf8>>
        }
    },
    <<"IPCTYPE">> => #{
        order => 7,
        type => string,
        required => true,
        default => <<"网络"/utf8>>,
        title => #{
            zh => <<"摄像头型号"/utf8>>
        },
        description => #{
            zh => <<"摄像头型号"/utf8>>
        }
    },
    <<"IPCMAC">> => #{
        order => 8,
        type => string,
        required => true,
        default => <<"a0-bd-1d-ce-8d-99;8C-16-45-6E-83-D6">>,
        title => #{
            zh => <<"摄像头地址"/utf8>>
        },
        description => #{
            zh => <<"摄像头MAC地址"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/ffmpeg.png">>,
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
    <<"product">> := _Products,
    <<"IPCTYPE">> :=  _IPCTYPE
} = ChannelArgs) ->
    _Env = get_newenv(ChannelArgs),
%%    [{ProductId, App} | _] = get_app(Products),
%%    NewEnv = Env#{
%%        <<"channelid">> => ChannelId,
%%        <<"app">> => App,
%%        <<"product">> => ProductId,
%%        <<"IPCTYPE">> =>  IPCTYPE,
%%        <<"ips">> => []
%%    },
    State = #state{
        id = ChannelId,
        env = []
    },
    {ok, State, []}.


handle_init(State) ->
    erlang:send_after(5000, self(), init),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info,"channel ~p", [Event]),
    {ok, State}.

handle_message({deliver, _Topic, Msg},
        #state{id = ChannelId, env = #{<<"ips">> := Ips} = Env} = State) ->
    dgiot_bridge:send_log(ChannelId, "from_task: ~ts:  ~p ", [_Topic, dgiot_mqtt:get_payload(Msg)]),
    case binary:split(dgiot_mqtt:get_topic(Msg), <<$/>>, [global, trim]) of
        [<<"thing">>, ProductId, DevAddr, _, <<"FFMPEG">>] ->
            #{<<"interval">> := Interval, <<"instruct">> := Instruct, <<"appdata">> := AppData} =
                jsx:decode(dgiot_mqtt:get_payload(Msg), [{labels, binary}, return_maps]),
            NewEnv = Env#{
                <<"interval">> => Interval,
                <<"appdata">> => AppData#{ <<"product">> => ProductId},
                <<"instruct">> => Instruct, <<"devaddr">> => DevAddr,
                <<"ips">> => Ips
            },
            ?LOG(info,"AppData ~p", [AppData]),
            dgiot_ffmpeg:start_live(NewEnv),
            dgiot_ffmpeg:start_video(NewEnv),
            {ok, State};
        _ ->
            {ok, State}
    end;

handle_message(init, #state{env = #{
    <<"product">> := GwProductId,
    <<"IPCMAC">> := Mac,
    <<"IPCTYPE">> := IPCTYPE
}  = Env} = State) ->
    dgiot_product:load(GwProductId),
    Filter = #{
        <<"where">> => #{<<"product">> => GwProductId},
        <<"limit">> => 1,
        <<"include">> => <<"product">>
    },
    Ips =
        case dgiot_parse:query_object(<<"Device">>, Filter) of
            {ok, #{<<"results">> := Result}} when length(Result) == 0 -> [];
            {ok, #{<<"results">> := [#{
                <<"devaddr">> :=  GwAddr,
                <<"ACL">> := Acl,
                <<"objectId">> :=  GwDeviceId}
            ]}} ->
                dgiot_ffmpeg:create_scr_device(#{
                    <<"ACL">> => Acl,
                    <<"gwdevaddr">> => GwAddr,
                    <<"gwdevid">> => GwDeviceId,
                    <<"IPCTYPE">> => IPCTYPE
                }),
                dgiot_ffmpeg:create_ipc_device(#{
                    <<"IPCMAC">> => Mac,
                    <<"ACL">> => Acl,
                    <<"gwdevaddr">> => GwAddr,
                    <<"gwdevid">> => GwDeviceId,
                    <<"IPCTYPE">> => IPCTYPE
                });
            _ ->
                #{}
        end,
    NewEnv = Env#{<<"ips">> => Ips},
    {ok, State#state{env = NewEnv}};

handle_message({save_video, Env}, State) ->
    dgiot_ffmpeg:upload(Env),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(error, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

%%get_app(Products) ->
%%    lists:map(fun({_ProdcutId, #{<<"ACL">> := Acl}}) ->
%%        Predicate = fun(E) ->
%%            case E of
%%                <<"role:", _/binary>> -> true;
%%                _ -> false
%%            end
%%                    end,
%%        [<<"role:", _App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
%%        {_ProdcutId, _App}
%%              end, Products).

get_newenv(Args) ->
    maps:without([
        <<"behaviour">>,
        <<"MaxOverFlow">>,
        <<"Size">>,
        <<"applicationtText">>,
        <<"product">>], Args).
