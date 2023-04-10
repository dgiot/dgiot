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

-module(dgiot_tdengine_test).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").

-export([test/0]).


-export([max/1, test/1, test/3]).

max(Count) ->
    Now1 = dgiot_datetime:now_ms(),
    test(Count),
    io:format("~s ~p time ~p ~n", [?FILE, ?LINE, dgiot_datetime:now_ms() - Now1]).

test(Count, ProductId, DevAddr) ->
    test(ProductId, DevAddr, 1, Count).

test(Count) ->
    test(<<"676b609811">>, <<"000010800449">>, 1, Count).
test(ProductId, DevAddr, I, Max) when I =< Max ->
    Storage = #{
        <<"energy">> => rand:uniform(9999),
        <<"rate_energy01">> => rand:uniform(9999),
        <<"rate_energy02">> => rand:uniform(9999),
        <<"rate_energy03">> => rand:uniform(9999),
        <<"rate_energy04">> => rand:uniform(9999)
    },
    dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage),
    test(ProductId, DevAddr, I + 1, Max);

test(_, _, _, _) -> ok.

test() ->
    %% 创建超级表
    dgiot_tdengine:create_schemas(#{
        <<"tableName">> => <<"Product">>,
        <<"fields">> => [
            {<<"config">>, #{
                <<"type">> => <<"INT">>
            }},
            {<<"secret">>, #{
                <<"type">> => <<"FLOAT">>
            }},
            {<<"description">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }},
            {<<"enable">>, #{
                <<"type">> => <<"BOOL">>
            }}
        ],
        <<"tags">> => [
            {
                <<"product">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }
            },
            {
                <<"devaddr">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }
            }
        ]
    }),
    %% 创建子表
    dgiot_tdengine:create_schemas(#{
        <<"tableName">> => <<"SW00000001">>,
        <<"using">> => <<"Product">>,
        <<"tags">> => [<<"Product">>, <<"00000001">>]
    }),
    %% 插入设备
    dgiot_tdengine:create_object(<<"SW00000001">>, #{
        <<"using">> => <<"Product">>,
        <<"tags">> => [<<"Product">>, <<"00000001">>],
        <<"values">> => [
            [now, 222, 2.1, <<"aaaaa">>, false],
            [now, 222, 2.2, <<"aaaaa">>, false],
            [now, 222, 2.3, <<"aaaaa">>, false]
        ]
    }),
    F =
        fun(I) ->
            Dev = list_to_binary(io_lib:format("~8.10.0B,", [I])),
            dgiot_tdengine:create_object(<<"SW", Dev/binary>>, #{
                <<"using">> => <<"Product">>,
                <<"tags">> => [<<"Product">>, Dev],
                <<"values">> => [
                    [now, 222, 2.1, <<"aaaaa">>, false],
                    [now, 222, 2.2, <<"aaaaa">>, false],
                    [now, 222, 2.3, <<"aaaaa">>, false]
                ]
            })
        end,
    [F(I) || I <- lists:seq(1, 10)],
    dgiot_tdengine:query_object(<<"Product">>, #{
        <<"keys">> => <<"config,enable">>,
        <<"limit">> => 2,
        <<"skip">> => 0,
        <<"order">> => <<"-createdAt">>,
        <<"where">> => #{
            <<"enable">> => false
        }
    }),

    %% 创建普通表
    dgiot_tdengine:create_schemas(#{
        <<"tableName">> => <<"Device">>,
        <<"fields">> => [
            {<<"devaddr">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }},
            {<<"enable">>, #{
                <<"type">> => <<"BOOL">>
            }},
            {<<"description">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }}
        ]
    }),

    dgiot_tdengine:create_schemas(#{
        <<"tableName">> => <<"Device2">>,
        <<"fields">> => [
            {<<"devaddr">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }},
            {<<"enable">>, #{
                <<"type">> => <<"BOOL">>
            }},
            {<<"description">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }}
        ]
    }),

    %% 插入一条记录
    dgiot_tdengine:create_object(<<"Device">>, #{
        <<"values">> => [now, <<"00000001">>, true, <<>>]
    }),
    %% 插入一条记录，数据对应到指定的列
    dgiot_tdengine:create_object(<<"Device">>, #{
        <<"fields">> => [<<"createdat">>, <<"devaddr">>, <<"enable">>],
        <<"values">> => [now, <<"00000002">>, true]
    }),
    %% 插入多条记录
    dgiot_tdengine:batch(#{
        <<"tableName">> => <<"Device">>,
        <<"values">> => [
            [now, <<"00000003">>, true, <<>>],
            [now, <<"00000004">>, true, <<>>],
            [now, <<"00000005">>, true, <<>>]
        ]
    }),
    %% 按指定的列插入多条记录
    dgiot_tdengine:batch(#{
        <<"tableName">> => <<"Device">>,
        <<"fields">> => [<<"createdat">>, <<"devaddr">>, <<"enable">>],
        <<"values">> => [
            [now, <<"00000006">>, true],
            [now, <<"00000007">>, true],
            [now, <<"00000008">>, true]
        ]
    }),
    %% 向多个表插入多条记录
    dgiot_tdengine:batch([
        #{
            <<"tableName">> => <<"Device">>,
            <<"values">> => [
                [now, <<"00000009">>, true, <<>>],
                [now, <<"00000010">>, true, <<>>]
            ]},
        #{
            <<"tableName">> => <<"Device2">>,
            <<"values">> => [
                [now, <<"00000001">>, true, <<>>],
                [now, <<"00000002">>, true, <<>>]
            ]}
    ]),
    %% 同时向多个表按列插入多条记录
    dgiot_tdengine:batch([
        #{
            <<"tableName">> => <<"Device">>,
            <<"fields">> => [<<"createdat">>, <<"devaddr">>, <<"enable">>],
            <<"values">> => [
                [now, <<"00000011">>, true],
                [now, <<"00000012">>, true],
                [now, <<"00000013">>, true]
            ]},
        #{
            <<"tableName">> => <<"Device2">>,
            <<"fields">> => [<<"createdat">>, <<"devaddr">>, <<"enable">>],
            <<"values">> => [
                [now, <<"00000003">>, true],
                [now, <<"00000004">>, true],
                [now, <<"00000005">>, true]
            ]}
    ]).
