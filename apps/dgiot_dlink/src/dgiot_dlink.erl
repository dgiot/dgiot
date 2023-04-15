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
%% https://doc.oschina.net/grpc
%% https://www.grpc.io/docs/

-module(dgiot_dlink).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("dgiot/include/logger.hrl").

-define(TYPE, <<"DLINK">>).

%% 注册协议类型
-protocol_type(#{
    cType => ?TYPE,
    type => <<"DLINK">>,
    colum => 3,
    title => #{
        zh => <<"DLINK协议"/utf8>>
    },
    description => #{
        zh => <<"DLINK协议"/utf8>>
    }
}).
%% 注册协议参数
-params (#{
    <<"dis">> => #{
        order => 1,
        type => object,
        allowCreate => true,
        required => true,
        default => [
            #{<<"value">> => <<"lable">>, <<"name">> => <<"key">>}
        ],
        title => #{
            zh => <<"数据标识"/utf8>>
        },
        description => #{
            zh => <<"数据标识"/utf8>>
        },
        <<"table">> => [
            #{
                key => <<"key">>,
                order => 2,
                type => string,
                required => true,
                default => <<"data.[0].number"/utf8>>,
                title => #{
                    zh => <<"数据标识"/utf8>>
                },
                description => #{
                    zh => <<"数据标识"/utf8>>
                }
            },
            #{
                key => <<"data">>,
                order => 2,
                type => integer,
                required => true,
                default => <<"2"/utf8>>,
                title => #{
                    zh => <<"数据长度(字节)"/utf8>>
                },
                description => #{
                    zh => <<"数据长度(字节)"/utf8>>
                }
            }
        ]
    }
}).

%% API
get_datasource(_) ->
    ok.

start(Server) ->
    Services = #{protos => [dgiot_dlink_pb],
        services => #{'dgiot.Dlink' => dgiot_dlink_server}
    },
    {ok, _} = grpc:start_server(Server, 30051, Services, []).

stop(Server) ->
    _ = grpc:stop_server(Server).

get_all_protocol() ->
    lists:foldl(fun({_, Channel_type}, Acc) ->
        App = maps:get(app, Channel_type),
        Mod = maps:get(mod, Channel_type),
        CType = maps:get(cType, Channel_type),
        Attributes = Mod:module_info(attributes),
        [format_protocol(App, CType, Channel_type, Attributes) | Acc]
                end, [], list()).

format_protocol(App, CType, Channel_type, Attributes) ->
    [Params] = proplists:get_value(params, Attributes, [#{}]),
    Channel_type#{
        cType => CType,
        app => App,
        params => maps:merge(#{
            <<"_dlinkindex">> => #{
                order => 103,
                type => integer,
                required => true,
                default => 0,
                title => #{
                    zh => <<"报文序号"/utf8>>
                },
                description => #{
                    zh => <<"报文序号,初始值为0"/utf8>>
                }
            }
        }, Params)
    }.

list() ->
    Fun =
        fun({App, Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) == false of
                true ->
                    Acc;
                false ->
                    case [Channel || {protocol_type, [Channel]} <- Mod:module_info(attributes)] of
                        [] ->
                            Acc;
                        [Channel | _] ->
                            [{maps:get(priority, Channel, 255), Channel#{app => App, mod => Mod, vsn => Vsn}}] ++ Acc
                    end
            end
        end,
    lists:sort(dgiot_plugin:check_module(Fun, [])).

get_json(Type) ->
    dgiot_utils:get_JsonFile(?MODULE, <<Type/binary, ".json">>).
