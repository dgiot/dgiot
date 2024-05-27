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
-module(dgiot_modbusc_channel).
-behavior(dgiot_channelx).
-include("dgiot_modbus.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"MODBUSC">>).
%% API
-export([
    start/2
]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
-define(consumer(ChannelId), <<"modbus_consumer_", ChannelId/binary>>).

-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"MODBUSC资源通道"/utf8>>
    },
    description => #{
        zh => <<"MODBUSC资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ip">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1"/utf8>>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 502,
        title => #{
            zh => <<"服务器端口"/utf8>>
        },
        description => #{
            zh => <<"服务器端口"/utf8>>
        }
    },
    <<"slaveid">> => #{
        order => 3,
        type => integer,
        required => true,
        default => 1,
        title => #{
            zh => <<"从机地址"/utf8>>
        },
        description => #{
            zh => <<"从机地址"/utf8>>
        }
    },
    <<"function">> => #{
        order => 4,
        type => string,
        required => true,
        default => #{<<"value">> => <<"readHregs">>, <<"label">> => <<"0X03:读保持寄存器"/utf8>>},
        enum => [#{<<"value">> => <<"readCoils">>, <<"label">> => <<"0X01:读线圈"/utf8>>},
            #{<<"value">> => <<"readInputs">>, <<"label">> => <<"0X02:读离散量输入"/utf8>>},
            #{<<"value">> => <<"readHregs">>, <<"label">> => <<"0X03:读保持寄存器"/utf8>>},
            #{<<"value">> => <<"readIregs">>, <<"label">> => <<"0X04:读输入寄存器"/utf8>>}
        ],
        title => #{
            zh => <<"功能码"/utf8>>
        },
        description => #{
            zh => <<"功能码"/utf8>>
        }
    },
    <<"address">> => #{
        order => 5,
        type => integer,
        required => true,
        default => 0,
        title => #{
            zh => <<"起始地址"/utf8>>
        },
        description => #{
            zh => <<"起始地址"/utf8>>
        }
    },
    <<"quantity">> => #{
        order => 6,
        type => integer,
        required => true,
        default => 100,
        title => #{
            zh => <<"长度"/utf8>>
        },
        description => #{
            zh => <<"长度"/utf8>>
        }
    },
    <<"freq">> => #{
        order => 7,
        type => integer,
        required => true,
        default => 60,
        title => #{
            zh => <<"采集频率/秒"/utf8>>
        },
        description => #{
            zh => <<"采集频率/秒"/utf8>>
        }
    },
    <<"filepath">> => #{
        order => 8,
        type => upload,
        required => true,
        default => <<"">>,
        title => #{
            zh => <<"csv文件"/utf8>>
        },
        description => #{
            zh => <<"上传csv点位文件"/utf8>>
        }
    },
    <<"is_refresh">> => #{
        order => 9,
        type => enum,
        required => true,
        default => #{<<"value">> => true, <<"label">> => <<"是"/utf8>>},
        enum => [
            #{<<"value">> => true, <<"label">> => <<"是"/utf8>>},
            #{<<"value">> => false, <<"label">> => <<"否"/utf8>>}
        ],
        title => #{
            zh => <<"刷新点位"/utf8>>
        },
        description => #{
            zh => <<"是否刷新物模型"/utf8>>
        }
    },
    <<"is_shard">> => #{
        order => 9,
        type => enum,
        required => true,
        default => #{<<"value">> => false, <<"label">> => <<"否"/utf8>>},
        enum => [
            #{<<"value">> => true, <<"label">> => <<"是"/utf8>>},
            #{<<"value">> => false, <<"label">> => <<"否"/utf8>>}
        ],
        title => #{
            zh => <<"是否分片"/utf8>>
        },
        description => #{
            zh => <<"是否分片存储"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/modbusc.png">>,
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
    <<"ip">> := Ip,
    <<"port">> := Port,
    <<"slaveid">> := SlaveId,
    <<"function">> := Function,
    <<"address">> := Address,
    <<"quantity">> := Quantity,
    <<"freq">> := Freq,
    <<"is_refresh">> := Is_refresh,
    <<"Size">> := Size
} = Args) ->
    {FileName, MinAddr, MaxAddr} =
        case maps:find(<<"filepath">>, Args) of
            {ok, FilePath} ->
                {FileName1, MinAddr1, MaxAddr1} = dgiot_product_csv:read_csv(ChannelId, FilePath, Is_refresh, maps:get(<<"is_shard">>, Args, true)),
                %% modbus_tcp:set_addr(ChannelId, MinAddr1, MaxAddr1),
                {FileName1, MinAddr1, MaxAddr1};
            _ ->
                {<<>>, 0, 100}
        end,
    NewArgs = #{
        <<"ip">> => Ip,
        <<"port">> => Port,
        <<"mod">> => dgiot_modbusc_tcp,
        <<"child">> => #{
            slaveid => SlaveId,
            function => Function,
            address => dgiot_utils:to_int(Address),
            quantity => Quantity,
            filename => FileName,
            data => <<>>,
            freq => Freq,
            minaddr => MinAddr,
            maxaddr => MaxAddr}},
%%    dgiot_client:add_clock(ChannelId, Start_time, End_time),
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 5000, dgiot_datetime:now_secs() + 300000),
    {ok, #state{id = ChannelId, env = #{size => Size, filename => FileName, freq => Freq}}, dgiot_client:register(ChannelId, tcp_client_sup, NewArgs)}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event]),
    {ok, State}.

handle_message(check_connection, #state{id = ChannelId, env = #{filename := FileName, freq := Freq}} = Dclient) ->
    Now = dgiot_datetime:now_secs(),
    case dgiot_data:get({check_connection, ChannelId, FileName}) of
        OldTime when (Now - OldTime) > Freq ->
            dgiot_client:stop(ChannelId, FileName),
            dgiot_client:start(ChannelId, FileName);
        _ ->
            pass
    end,
    erlang:send_after(Freq * 1200, self(), check_connection),
    {noreply, Dclient};

handle_message(start_client, #state{id = ChannelId, env = #{size := _Size, filename := FileName}} = State) ->
    case dgiot_data:get({start_client, ChannelId}) of
        not_find ->
%%            [dgiot_client:start(ChannelId, dgiot_utils:to_binary(I)) || I <- lists:seq(1, Size)],
            dgiot_client:start(ChannelId, FileName),
            erlang:send_after(30 * 1000, self(), check_connection),
            dgiot_data:insert({start_client, ChannelId}, ChannelId);
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    dgiot_client:stop(ChannelId),
    dgiot_data:delete({start_client, ChannelId}),
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.




