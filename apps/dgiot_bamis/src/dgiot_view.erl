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

%% @doc dgiot_bamis Protocol
-module(dgiot_view).
-include("dgiot_bamis.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_swagger(<<"amis">>).

-export([
    get/2,
    post/2,
    put/2,
    delete/2,
    get_value/4
]).

get('before', Args) ->
%%    io:format("~s ~p ~p ~p~n", [?FILE, ?LINE, Args, Id]),
    Args;

get('after', Args) ->
%%    io:format("~s ~p ~p ~p~n", [?FILE, ?LINE, Args, Id]),
    Args.

post('before', Args) ->
%%    io:format("~s ~p ~p ~p~n", [?FILE, ?LINE, Args, Id]),
    Args;
post('after', Data) ->
%%    io:format("~s ~p ~p~n", [?FILE, ?LINE, Data]),
    Data.

put('before', Args) ->
%%    io:format("~s ~p ~p ~p~n", [?FILE, ?LINE, Args, Id]),
    Args;
put('after', Data) ->
%%    io:format("~s ~p ~p~n", [?FILE, ?LINE, Data]),
    Data.

delete('before', Args) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Args, Id]),
    Args;
delete('after', Data) ->
    Data.

get_value(Key, Values, Data, Acc) when is_list(Values) ->
    List = lists:foldl(fun(Value, NewAcc) ->
        case maps:find(Value, Data) of
            {ok, V} ->
                NewAcc ++ [V];
            _ ->
                NewAcc
        end
                       end, [], Values),
    Acc#{Key => List};
get_value(Key, Value, Data, Acc) ->
    case maps:find(Value, Data) of
        {ok, V} ->
            Acc#{Key => V};
        _ ->
            Acc
    end.

