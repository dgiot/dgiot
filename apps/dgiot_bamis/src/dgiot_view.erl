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
    get_value/4,
    get_resbody/4
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
        Map = dgiot_map:get(Value, Data),
        case maps:find(Value, Map) of
            {ok, V} ->
                NewAcc ++ [V];
            _ ->
                NewAcc
        end
                       end, [], Values),
    Acc#{Key => List};
get_value(Key, Value, Data, Acc) ->
    Map = dgiot_map:get(Value, Data),
    case maps:find(Value, Map) of
        {ok, V} ->
            Acc#{Key => V};
        _ ->
            Acc
    end.

get_resbody(ViewId, Token, Renders, ResBody) ->
    Vars = lists:foldl(
        fun
            (#{<<"key">> := Key, <<"api">> := Api, <<"params">> := Args, <<"value">> := Value}, Acc) ->
%%                io:format("~s ~p ~p Api ~p ~n", [?FILE, ?LINE, Args, Api]),
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
%%                        io:format("~s ~p Key =  ~p Value = ~p OperationId =  ~p ~n", [?FILE, ?LINE, Key, Value, OperationId]),
                        case dgiot_router:get_state_by_operation(OperationId) of
                            {ok, {_, #{logic_handler := Handler, base_path := Base_path}}} ->
%%                                io:format("~s ~p Key =  ~p Value = ~p Handler =  ~p ~n", [?FILE, ?LINE, Key, Value, Handler]),
                                case Handler:handle(OperationId, Args, #{base_path => Base_path, <<"sessionToken">> => Token}, #{bindings => #{id => ViewId}, headers => #{}}) of
                                    {200, _Headers, Result, _Req} ->
                                        io:format("~s ~p Key =  ~p Value = ~p  ~n", [?FILE, ?LINE, Key, Value]),
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
    io:format("~s ~p Vars =  ~p ~n", [?FILE, ?LINE, Vars]),
%%    io:format("~s ~p Body =  ~p ~n", [?FILE, ?LINE,  dgiot_json:encode(ResBody)]),
    dgiot_json:decode(dgiot_map:map(Vars, dgiot_json:encode(ResBody))).
