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

-module(dgiot_parse_graphql).
-author("johnliu").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    graphql/1,
    test_graphql/0
]).


graphql(Data) ->
    Header =
        case maps:get(<<"access_token">>, Data, <<"undefined">>) of
            <<"undefined">> -> [];
            Token -> [{"X-Parse-Session-Token", dgiot_utils:to_list(Token)}]
        end,
    ?LOG(info, "Header ~p", [Header]),
    graphql(?DEFAULT, Header, maps:without([<<"access_token">>], Data)).
graphql(Name, Header, Data) ->
    dgiot_parse:request_rest(Name, 'POST', Header, <<"/graphql">>, Data, []).

test_graphql() ->
    Data = #{
        <<"operationName">> => <<"Health">>,
        <<"variables">> => #{},
        <<"query">> => <<"query Health {\n  health\n}\n">>
    },
%%    {"operationName":"Health","variables":{},"query":"query Health {\n  health\n}\n"}
    graphql(Data).