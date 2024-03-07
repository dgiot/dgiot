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

-module(dgiot_atomgit_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_atomgit.hrl").
-define(TYPE, <<"atomgit">>).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"atomgit测试通道"/utf8>>
    },
    description => #{
        zh => <<"atomgit测试通道"/utf8>>
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
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/atomgit.jpg">>,
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
    <<"port">> := Port,
    <<"product">> := Products
} = Args) ->
    {ProdcutId, App} =
        case get_app(Products) of
            [{ProdcutId1, App1} | _] ->
                {ProdcutId1, App1};
            [] ->
                {<<>>, <<>>};
            _ ->
                {<<>>, <<>>}
        end,
    State = #state{
        id = ChannelId,
        env = Args,
        app = App,
        product = ProdcutId
    },
    {ok, State, dgiot_atomgit_tcp:start(Port, State)}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, State) ->
    ?LOG(error, "EventId ~p Event ~p", [EventId, Event]),
    {ok, State}.

handle_message({dlink_properties_report, ProductId, DevAddr, Buff}, #state{id = ChannelId} = State) ->
    dgiot_bridge:send_log(ChannelId, ProductId, "~s ~p ~p mqtt revice from ~p => ProductId ~p ", [?FILE, ?LINE, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), Buff, ProductId]),
    case dgiot_atomgit_decoder:parse_frame(Buff, []) of
        {ok, [#{<<"devaddr">> := DevAddr} = Data | _]} ->
            NewData = dgiot_dlink_proctol:parse_payload(ProductId, Data),
            dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, "~s ~p ~p revice from ~p~n save td => ProductId ~p DevAddr ~p ~ts ",
                [?FILE, ?LINE, dgiot_datetime:format("YYYY-MM-DD HH:NN:SS"), Buff, ProductId, DevAddr, unicode:characters_to_list(dgiot_json:encode(NewData))]),
            dgiot_task:save_td(ProductId, DevAddr, NewData, #{});
        _ ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.

get_app(Products) ->
    lists:map(fun({ProdcutId, #{<<"ACL">> := Acl}}) ->
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
        {ProdcutId, App}
              end, Products).
