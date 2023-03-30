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


-module(dgiot_view_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_bamis.hrl").
-define(TYPE, <<"VIEW">>).
-define(MAX_BUFF_SIZE, 1024).

%% API
-export([start/2]).
-dgiot_data("ets").
%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
-export([init_ets/0]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"VIEW资源通道"/utf8>>
    },
    description => #{
        zh => <<"VIEW资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/amis.png">>,
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
    dgiot_data:init(?VIEW_CH_ETS).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

init(?TYPE, ChannelId, Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    dgiot_parse_hook:subscribe(<<"View/*">>, get, ChannelId, [<<"render">>]),
    dgiot_parse_hook:subscribe(<<"View">>, post, ChannelId),
    dgiot_parse_hook:subscribe(<<"View/*">>, put, ChannelId, [<<"isEnable">>]),
    dgiot_parse_hook:subscribe(<<"View/*">>, delete, ChannelId),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message({sync_parse, Pid, 'before', get, _Token, <<"View">>, #{<<"mode">> := Mode} = _Args}, State) ->
    dgiot_data:insert(?VIEW_CH_ETS, Pid, Mode),
    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, Mode]),
    {ok, State};

handle_message({sync_parse, Pid, 'after', get, Token, <<"View">>, #{<<"data">> := _Data, <<"render">> := Renders, <<"objectId">> := ViewId} = ResBody}, State) ->
    ResBody1 =
        case dgiot_data:get(?VIEW_CH_ETS, Pid) of
            not_find ->
                get_resbody(ViewId, Token, Renders, ResBody);
            _ ->
                dgiot_data:delete(?VIEW_CH_ETS, Pid),
                ResBody
        end,
%%    io:format("~s ~p Data ~p ~n", [?FILE, ?LINE, dgiot_json:encode(Data)]),
    dgiot_parse_hook:publish(Pid, ResBody1),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', post, _Token, <<"View">>, QueryData}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, _Pid, QueryData]),
    dgiot_view:post('after', QueryData),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', put, _Token, <<"View">>, QueryData}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, QueryData]),
    dgiot_view:put('after', QueryData),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', delete, _Token, <<"View">>, ObjectId}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, ObjectId]),
    dgiot_view:delete('after', ObjectId),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.


get_resbody(ViewId, Token, Renders, ResBody) ->
    Vars = lists:foldl(
        fun
            (#{<<"key">> := Key, <<"api">> := Api, <<"params">> := Args, <<"value">> := Value}, Acc) ->
                OperationId = dgiot_utils:to_atom(Api),
                case binary:split(Api, <<$_>>, [global, trim]) of
                    [<<"get">>, <<"amis">>, Class, ObjectId] ->
                        case dgiot_parse:get_object(Class, ObjectId, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
                            {ok, Result} ->
                                Res = dgiot_bamis:get({'after', Result}),
                                dgiot_view:get_value(Key, Value, Res, Acc);
                            _ ->
                                Acc
                        end;
                    [<<"get">>, <<"amis">>, Class] ->
                        case dgiot_parse:query_object(Class, Args, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
                            {ok, Result} ->
                                Res = dgiot_bamis:get({'after', Result}),
                                dgiot_view:get_value(Key, Value, Res, Acc);
                            _ ->
                                Acc
                        end;
                    [<<"get">>, <<"classes">>, Class, ObjectId] ->
                        case dgiot_parse:get_object(Class, ObjectId, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
                            {ok, Result} ->
                                dgiot_view:get_value(Key, Value, Result, Acc);
                            _ ->
                                Acc
                        end;
                    [<<"get">>, <<"classes">>, Class] ->
                        case dgiot_parse:query_object(Class, Args, [{"X-Parse-Session-Token", Token}], [{from, rest}]) of
                            {ok, Result} ->
                                dgiot_view:get_value(Key, Value, Result, Acc);
                            _ ->
                                Acc
                        end;
                    _ ->
                        case dgiot_router:get_state_by_operation(OperationId) of
                            {ok, {220, #{logic_handler := Handler, base_path := Base_path}}} ->
                                case Handler:handle(OperationId, Args, #{base_path => Base_path, <<"sessionToken">> => Token}, #{bindings => #{id => ViewId}, headers => #{}}) of
                                    {200, _Headers, Result, _Req} ->
                                        dgiot_view:get_value(Key, Value, Result, Acc);
                                    _ ->
                                        Acc
                                end;
                            _ ->
                                Acc
                        end
                end;
            (_X, Acc) ->
                Acc
        end, #{}, Renders),
    dgiot_json:decode(dgiot_map:map(Vars, dgiot_json:encode(ResBody))).
