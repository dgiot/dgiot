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

-module(dgiot_parse_channel).
-author("kenneth").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_channelx).
-dgiot_data("ets").
-export([
    init_ets/0,
    send/2]).

-export([get_config/0, get_config/1]).
-export([start/0, start/2, init/3, handle_init/1, handle_event/3, handle_message/2, stop/3, handle_save/1]).
-record(state, {channel, cfg}).

%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    cType => ?BACKEND_CHL,
    type => 1,
    title => #{
        zh => <<"Parser Server存储通道"/utf8>>
    },
    description => #{
        zh => <<"Parser Server存储通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"host">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"http://127.0.0.1:1337">>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"path">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"/parse/">>,
        title => #{
            zh => <<"路径"/utf8>>
        },
        description => #{
            zh => <<"路径"/utf8>>
        }
    },
    <<"appid">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"">>,
        title => #{
            zh => <<"AppId"/utf8>>
        },
        description => #{
            zh => <<"AppId"/utf8>>
        }
    },
    <<"master">> => #{
        order => 4,
        type => string,
        required => false,
        default => <<"">>,
        title => #{
            zh => <<"MasterKey"/utf8>>
        },
        description => #{
            zh => <<"MasterKey"/utf8>>
        }
    },
    <<"jskey">> => #{
        order => 5,
        type => string,
        required => false,
        default => <<"">>,
        title => #{
            zh => <<"JSKey"/utf8>>
        },
        description => #{
            zh => <<"JSKey"/utf8>>
        }
    },
    <<"restkey">> => #{
        order => 6,
        type => string,
        required => false,
        default => <<"">>,
        title => #{
            zh => <<"RestKey"/utf8>>
        },
        description => #{
            zh => <<"RestKey"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/parser%20sever%E5%9B%BE%E6%A0%87.jpg">>,
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


start() ->
    Cfg = #{
        <<"host">> => application:get_env(dgiot_parse, parse_server, not_find),
        <<"path">> => application:get_env(dgiot_parse, parse_path, not_find),
        <<"appid">> => application:get_env(dgiot_parse, parse_appid, not_find),
        <<"master">> => application:get_env(dgiot_parse, parse_master_key, not_find),
        <<"jskey">> => application:get_env(dgiot_parse, parse_js_key, not_find),
        <<"restkey">> => application:get_env(dgiot_parse, parse_rest_key, not_find)
    },
    start(?DEFAULT, Cfg).

start(Channel, Cfg) ->
    dgiot_channelx:add(parse_channelx, ?TYPE, Channel, ?MODULE, Cfg#{
        <<"Size">> => 100,
        <<"MaxOverFlow">> => 50
    }).

%% 通道初始化
init(?TYPE, Channel, Cfg) ->
    State = #state{channel = Channel, cfg = Cfg},
    Opts = [?CACHE(Channel), #{
        auto_save => application:get_env(dgiot_parse, cache_auto_save, 3000),
        size => application:get_env(dgiot_parse, cache_max_size, 50000),
        memory => application:get_env(dgiot_parse, cache_max_memory, 102400),
        max_time => application:get_env(dgiot_parse, cache_max_time, 30),
        handle => {?MODULE, handle_save, [Channel]}
    }],
    Specs = [
        {dgiot_dcache, {dgiot_dcache, start_link, Opts}, permanent, 5000, worker, [dgiot_dcache]}
    ],
    {ok, State, Specs}.

%% 初始化池子
handle_init(State) ->
    emqx_hooks:add('logger.send', {?MODULE, send, []}),
    {ok, State}.

handle_message(config, #state{cfg = Cfg} = State) ->
    {reply, {ok, Cfg}, State};

handle_message(_Message, State) ->
    {ok, State}.

handle_event(_EventId, _Event, _State) ->
    ok.

handle_save(Channel) ->
    dgiot_parse_cache:do_save(Channel).

stop(_ChannelType, _ChannelId, _State) ->
    ok.

get_config() ->
    get_config(?DEFAULT).

get_config(Channel) ->
    dgiot_channelx:call(?TYPE, Channel, config).


init_ets() ->
    dgiot_data:init(?DGIOT_PARSE_ETS),
    dgiot_data:init(?ROLE_USER_ETS),
    dgiot_data:init(?ROLE_PARENT_ETS),
    dgiot_data:init(?USER_ROLE_ETS).

send(Meta, Payload) when is_list(Payload) ->
    send(Meta, iolist_to_binary(Payload));

send(#{error_logger := _Error_logger, mfa := {M, F, A}} = _Meta, Payload) ->
    Mfa = <<(atom_to_binary(M, utf8))/binary, $/, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>,
    Topic = <<"logger_trace/error/", Mfa/binary>>,
    dgiot_mqtt:publish(Mfa, Topic, Payload),
    Map = jiffy:decode(Payload, [return_maps]),
    NewMap = maps:with([<<"domain">>, <<"time">>, <<"pid">>, <<"msg">>, <<"mfa">>, <<"line">>, <<"level">>, <<"clientid">>, <<"topic">>, <<"peername">>], Map),
    dgiot_parse_cache:save_to_cache(#{<<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Log">>,
        <<"body">> => get_body(NewMap)});

send(#{mfa := _MFA} = Meta, Payload) ->
    Map = jiffy:decode(Payload, [return_maps]),
    Mfa = dgiot_utils:to_binary(maps:get(<<"mfa">>, Map, <<"all">>)),
    TraceTopic =
        case maps:find(topic, Meta) of
            {ok, TraceTopic1} ->
                BinTraceTopic = dgiot_utils:to_binary(TraceTopic1),
                <<"/", BinTraceTopic/binary>>;
            _ -> <<"">>
        end,
    Topic =
        case maps:find(clientid, Meta) of
            {ok, ClientId1} ->
                BinClientId = dgiot_utils:to_binary(ClientId1),
                <<"logger_trace/trace/", BinClientId/binary, TraceTopic/binary>>;
            _ ->
                Line =
                    case maps:find(<<"line">>, Map) of
                        {ok, Line1} ->
                            dgiot_utils:to_binary(Line1);
                        _ ->
                            <<"0">>
                    end,
                <<"logger_trace/log/", Mfa/binary, "/", Line/binary>>
        end,
    dgiot_mqtt:publish(Mfa, Topic, Payload),
    NewMap = maps:with([<<"domain">>, <<"time">>, <<"pid">>, <<"msg">>, <<"mfa">>, <<"line">>, <<"level">>, <<"clientid">>, <<"topic">>, <<"peername">>], Map),
    dgiot_parse_cache:save_to_cache(#{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Log">>,
        <<"body">> => get_body(NewMap)});

send(_Meta, Payload) ->
    dgiot_mqtt:publish(<<"logger_trace_other">>, <<"logger_trace/other">>, Payload),
    ok.

get_body(#{<<"msg">> := Msg, <<"clientid">> := _} = Map) when is_map(Msg) ->
    DefaultACL = #{<<"role:admin">> => #{
        <<"read">> => true,
        <<"write">> => true}
    },
    ACl = maps:get(<<"ACL">>, Msg, DefaultACL),
    NewMsg = maps:without([<<"ACL">>], Msg),
    Map#{<<"type">> => <<"json">>, <<"ACL">> => ACl, <<"msg">> => jiffy:encode(NewMsg)};
get_body(#{<<"msg">> := Msg} = Map) when is_map(Msg) ->
    Devaddr = maps:get(<<"devaddr">>, Msg, <<"">>),
    ProductId = maps:get(<<"productid">>, Msg, <<"">>),
    DefaultACL = #{<<"role:admin">> => #{
        <<"read">> => true,
        <<"write">> => true}
    },
    ACl = maps:get(<<"ACL">>, Msg, DefaultACL),
    NewMsg = maps:without([<<"ACL">>], Msg),
    Map#{<<"type">> => <<"json">>, <<"devaddr">> => Devaddr, <<"productid">> => ProductId, <<"ACL">> => ACl, <<"msg">> => jiffy:encode(NewMsg)};
get_body(Map) ->
    Map#{<<"type">> => <<"text">>,
        <<"ACL">> => #{<<"role:admin">> => #{
            <<"read">> => true,
            <<"write">> => true}
        }}.
