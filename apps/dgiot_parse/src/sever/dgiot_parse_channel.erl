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

-export([get_config/0, get_config/1, commit_git/1, commit_git/2]).
-export([start/0, start_timescale/0, start_slave/0, start/2, init/3, handle_init/1, handle_event/3, handle_message/2, stop/3, handle_save/1]).
-record(state, {channel, cfg}).

%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    priority => 1,
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
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/parserIcon.jpg">>,
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

start_slave() ->
    Cfg = #{
        <<"host">> => application:get_env(dgiot_parse, parse_server_slave, not_find),
        <<"path">> => application:get_env(dgiot_parse, parse_path, not_find),
        <<"appid">> => application:get_env(dgiot_parse, parse_appid, not_find),
        <<"master">> => application:get_env(dgiot_parse, parse_master_key, not_find),
        <<"jskey">> => application:get_env(dgiot_parse, parse_js_key, not_find),
        <<"restkey">> => application:get_env(dgiot_parse, parse_rest_key, not_find)
    },
    start(?SLAVE, Cfg).

start_timescale() ->
    Cfg = #{
        <<"host">> => application:get_env(dgiot_parse, timescale_server, not_find),
        <<"path">> => application:get_env(dgiot_parse, parse_path, not_find),
        <<"appid">> => application:get_env(dgiot_parse, parse_appid, not_find),
        <<"master">> => application:get_env(dgiot_parse, parse_master_key, not_find),
        <<"jskey">> => application:get_env(dgiot_parse, parse_js_key, not_find),
        <<"restkey">> => application:get_env(dgiot_parse, parse_rest_key, not_find)
    },
    start(?TIMESCALE, Cfg).

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
    emqx_hooks:add('logger.send', {dgiot_parse_log, send, []}),
    emqx_hooks:add('mqtt_publish.trace', {dgiot_tracer, check_trace, [?MODULE, ?LINE]}),
    {ok, State}.

handle_message(export, #state{cfg = Cfg} = State) ->
    {reply, {ok, Cfg}, State};

handle_message(config, #state{cfg = Cfg} = State) ->
    {reply, {ok, Cfg}, State};

handle_message({git, Data},  State) ->
    dgiot_parse:create_object(<<"Git">>,Data),
    {ok, State};

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

commit_git(Data) ->
    commit_git(?DEFAULT, Data).

commit_git(Channel, Data) ->
    dgiot_channelx:do_message(dgiot_utils:to_binary(Channel), {git, Data}).
