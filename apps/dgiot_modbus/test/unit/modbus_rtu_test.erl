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

-module(modbus_rtu_test).
-author("jonhl").

-include("../../../include/dgiot_modbus.hrl").

-export([test_calculated_properties/0]).

%% @doc 测试计算值属性处理
test_calculated_properties() ->
    io:format("~s ~p Starting calculated properties test...~n", [?FILE, ?LINE]),

    % 模拟数据块
    Buff = <<1,2,3,4,5,6,7,8,9,10>>,
    io:format("~s ~p Test data buffer: ~p~n", [?FILE, ?LINE, Buff]),

    % 模拟计算值属性配置
    CalculatedProps = [
        #{
            <<"identifier">> => <<"calculated_value1">>,
            <<"dataForm">> => #{
                <<"protocol">> => <<"MODBUSRTU">>,
                <<"strategy">> => <<"计算值"/utf8>>
            },
            <<"dataSource">> => #{
                <<"address">> => <<"0">>,
                <<"registersnumber">> => <<"1">>,
                <<"originaltype">> => <<"raw">>
            }
        },
        #{
            <<"identifier">> => <<"calculated_value2">>,
            <<"dataForm">> => #{
                <<"protocol">> => <<"MODBUSRTU">>,
                <<"strategy">> => <<"计算值"/utf8>>
            },
            <<"dataSource">> => #{
                <<"address">> => <<"2">>,
                <<"registersnumber">> => <<"1">>,
                <<"originaltype">> => <<"raw">>
            }
        }
    ],

    % 调用 process_calculated_properties 函数
    Result = modbus_rtu:process_calculated_properties(CalculatedProps, Buff, <<"parent_id">>, <<"parent_value">>, []),
    io:format("~s ~p Result: ~p~n", [?FILE, ?LINE, Result]),

    % 验证结果
    case maps:get(<<"calculated_value1">>, Result, undefined) of
        <<1>> -> 
            io:format("~s ~p Test for calculated_value1 passed!~n", [?FILE, ?LINE]);
        _ -> 
            io:format("~s ~p Test for calculated_value1 failed!~n", [?FILE, ?LINE])
    end,

    case maps:get(<<"calculated_value2">>, Result, undefined) of
        <<3>> -> 
            io:format("~s ~p Test for calculated_value2 passed!~n", [?FILE, ?LINE]);
        _ -> 
            io:format("~s ~p Test for calculated_value2 failed!~n", [?FILE, ?LINE])
    end,

    {ok, Result}.
