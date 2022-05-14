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
        default => 8080,
        title => #{
            zh => <<"服务器端口"/utf8>>
        },
        description => #{
            zh => <<"服务器端口"/utf8>>
        }
    },
    <<"file">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"弱电动环"/utf8>>,
        title => #{
            zh => <<"文件名"/utf8>>
        },
        description => #{
            zh => <<"文件名"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/modbus.png">>,
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
    #{<<"product">> := Products,
        <<"ip">> := Ip,
        <<"port">> := Port,
        <<"file">> := FileName} = Args,
    modbus_tcp:read_csv(FileName),
    lists:map(fun({ProductId, #{<<"ACL">> := _Acl}}) ->
        dgiot_modbusc_tcp:start_connect(#{
            <<"auto_reconnect">> => 10,
            <<"reconnect_times">> => 3,
            <<"ip">> => Ip,
            <<"port">> => Port,
            <<"productid">> => ProductId,
            <<"channelid">> => ChannelId,
            <<"hb">> => 10
        })
              end, Products),
    {ok, #state{id = ChannelId}, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event]),
    {ok, State}.

handle_message(_Message, State) ->
    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    io:format("~s ~p channel stop ~p~n ~p~n", [?FILE, ?LINE, ChannelType, ChannelId]),
    ok.

%%start_client(ProductId, Ip, Port, #{<<"parse">> := Parse}) ->
%%    #{
%%        <<"parse_table">> := ParseTable,
%%        <<"devaddr">> := DevAddrKey,
%%        <<"page_index">> := PageIndex,
%%        <<"page_size">> := PageSize,
%%        <<"total">> := Total
%%    } = Parse,
%%    Success = fun(Page) ->
%%        lists:map(fun(#{<<"devaddr">> := DevAddr}) ->
%%            dgiot_modbusc_tcp:start_connect(#{
%%                <<"auto_reconnect">> => 10,
%%                <<"reconnect_times">> => 3,
%%                <<"ip">> => Ip,
%%                <<"port">> => Port,
%%                <<"productid">> => ProductId,
%%                <<"hb">> => 60,
%%                <<"devaddr">> => DevAddr
%%            })
%%                  end, Page)
%%              end,
%%    Query = #{<<"keys">> => [DevAddrKey], <<"order">> => DevAddrKey},
%%    dgiot_parse_loader:start(ParseTable, Query, PageIndex, PageSize, Total, Success).
%%
%%
%%start_timer(Time, Fun) ->
%%    spawn(fun() ->
%%        timer(Time, Fun)
%%          end).
%%
%%timer(Time, Fun) ->
%%    receive
%%        cancel ->
%%            void
%%    after Time ->
%%        Fun()
%%    end.


%%get_app(Products) ->
%%    lists:map(fun({ProdcutId, #{<<"ACL">> := Acl}}) ->
%%        Predicate = fun(E) ->
%%            case E of
%%                <<"role:", _/binary>> -> true;
%%                _ -> false
%%            end
%%                    end,
%%        [<<"role:", App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
%%        {ProdcutId, App}
%%              end, Products).




