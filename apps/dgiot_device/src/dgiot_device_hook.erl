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

-module(dgiot_device_hook).
-author("johnliu").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([post/2, put/2, delete/2]).

post('after', #{<<"objectId">> := _DeviceId} = _QueryData) ->
    pass;

post(_, _) ->
    pass.

put('after', #{<<"objectId">> := _DeviceId}) ->
    pass;

put(_, _) ->
    pass.

delete('after', DeviceId) ->
    dgiot_device:delete(DeviceId),
    case dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"key">> => DeviceId, <<"class">> => <<"Device">>}}) of
        {ok, #{<<"results">> := Dicts}} ->
            DictRequests =
                lists:foldl(fun(#{<<"objectId">> := DictId}, Acc) ->
                    Acc ++ [#{
                        <<"method">> => <<"DELETE">>,
                        <<"path">> => <<"/classes/Dict/", DictId/binary>>,
                        <<"body">> => #{}
                    }]
                            end, [], Dicts),
            dgiot_parse:batch(DictRequests);
        _ ->
            pass
    end,
    case dgiot_parse:query_object(<<"View">>, #{<<"where">> => #{<<"key">> => DeviceId, <<"class">> => <<"Device">>}}) of
        {ok, #{<<"results">> := Views}} ->
            ViewRequests =
                lists:foldl(fun(#{<<"objectId">> := ViewId}, Acc) ->
                    Acc ++ [#{
                        <<"method">> => <<"DELETE">>,
                        <<"path">> => <<"/classes/View/", ViewId/binary>>,
                        <<"body">> => #{}
                    }]
                            end, [], Views),
            dgiot_parse:batch(ViewRequests);
        _ ->
            pass
    end;


delete(_, _) ->
    pass.

