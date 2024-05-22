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
-module(dgiot_bamis).
-include("dgiot_bamis.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_swagger(<<"amis">>).

-export([
    get/1,
    post/1,
    put/1,
    delete/1,
    format/1,
    start_http/0,
    format_multilayer/1
]).

-define(APP, ?MODULE).
%% amis api 接口适配
%% https://aisuda.bce.baidu.com/amis/zh-CN/docs/types/api#%E6%8E%A5%E5%8F%A3%E8%BF%94%E5%9B%9E%E6%A0%BC%E5%BC%8F-%E9%87%8D%E8%A6%81-

%% @description 接口说明
%% before 请求前的接口适配器拦截
%% after 请求返回的数据适配器，目前已适配amis返回格式参数，参数如下

%% @description 参数说明
%% status: 返回 0，表示当前接口正确返回，否则按错误请求处理；
%% msg: 返回接口处理信息，主要用于表单提交或请求失败时的 toast 显示；
%% data: 必须返回一个具有 key-value 结构的对象。

%% @description 备注
%% 已实现 get post 的适配工作
get({'before', '*', Args}) ->
    NewData = maps:fold(fun(K, V, Acc) ->
        case V of
            undefined -> Acc;
            _ -> Acc#{K => V}
        end
                        end, #{}, Args),
    Basic = format(NewData),
%%    io:format("~s ~p Basic = ~p.~n", [?FILE, ?LINE, Basic]),
    Basic;

get({'after', #{<<"results">> := Response, <<"count">> := Count} = _Data}) ->
    #{
        <<"status">> => 0,
        <<"msg">> => <<"数据请求成功"/utf8>>,
        <<"data">> => #{
            <<"items">> => Response,
            <<"total">> => Count
        }
    };

get({'after', #{<<"results">> := Response} = _Data}) ->
%%    io:format("~s ~p _Data = ~p~n", [?FILE, ?LINE, _Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"数据请求成功"/utf8>>,
        <<"data">> => #{<<"items">> => Response}
    };



get({'before', _Id, Args}) ->
    erlang:put(self(), Args),
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Args, _Id]),
    maps:with([<<"id">>], Args);

%% 查询单个
get({'after', Data}) ->
    Args = erlang:get(self()),
%%    io:format("~s ~p Data ~p ~n", [?FILE, ?LINE, Data]),
    NewData =
        case Args of
            #{<<"keys">> := undefined} ->
%%                io:format("~s ~p Field ~p ~n", [?FILE, ?LINE, maps:get(Field, Data,#{})]),
                Data;
            #{<<"keys">> := Keys} ->
                NewKeys = re:split(Keys, <<",">>, [{return, binary}, trim]),
%%                io:format("~s ~p Field ~p ~n", [?FILE, ?LINE, maps:get(Field, Data,#{})]),
                dgiot_map:with(NewKeys, Data);
            _ ->
                Data
        end,
%%    io:format("~s ~p NewData ~p ~n", [?FILE, ?LINE, NewData]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"数据请求成功"/utf8>>,
        <<"data">> => NewData
    }.

post({'before', _Id, Args}) ->
%%    io:format("~s ~p ~p ~p~n", [?FILE, ?LINE, Args, Id]),
    Args;
post({'after', Data}) ->
%%    io:format("~s ~p ~p~n", [?FILE, ?LINE, Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"数据提交成功"/utf8>>,
        <<"data">> => Data
    }.
put({'before', _Id, Args}) ->
    erlang:put(self(), Args),
%%    io:format("~s ~p ~p ~p~n", [?FILE, ?LINE, Args, Id]),
    Args;
put({'after', Data}) ->
    Request = erlang:get(self()),
%%    io:format("~s ~p ~p~n", [?FILE, ?LINE, Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"修改成功"/utf8>>,
        <<"data">> => #{<<"Data">> => Data, <<"Request">> => Request}
    }.
delete({'before', _Id, Args}) ->
    erlang:put(self(), Args),
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Args, Id]),
    Args;
delete({'after', Data}) ->
    Request = erlang:get(self()),
%%    io:format("~s ~p ~p~n", [?FILE, ?LINE, Data]),
    #{
        <<"status">> => 0,
        <<"msg">> => <<"删除成功"/utf8>>,
        <<"data">> => #{<<"Data">> => Data, <<"Request">> => Request}
    }.


%% 查询多个
%%/**
%%* https://docs.parseplatform.org/rest/guide/#query-constraints
%%* 查询条件
%%Parameter	Use
%%order	Specify a field to sort by
%%limit	Limit the number of objects returned by the query
%%skip	Use with limit to paginate through results
%%keys	Restrict the fields returned by the query
%%excludeKeys	Exclude specific fields from the returned query
%%include	Use on Pointer columns to return the full object
%%*/
format(#{<<"orderBy">> := OrderBy} = Data) when byte_size(OrderBy) > 0 ->
    NewData = maps:without([<<"orderBy">>], Data),
    format(NewData#{<<"order">> => [OrderBy]});

format(#{<<"order">> := [Order | _], <<"orderDir">> := OrderDir} = Data) when byte_size(Order) > 0 ->
    NewData = maps:without([<<"orderDir">>], Data),
    NewOrder =
        case OrderDir of
            <<"desc">> ->
                <<"-", Order/binary>>;
            _ ->
                Order
        end,
    format(NewData#{<<"order">> => [NewOrder]});

format(#{<<"perPage">> := PerPage, <<"page">> := Page} = Data) ->
    NewData = maps:without([<<"perPage">>, <<"page">>], Data),
    Skip = (Page - 1) * PerPage,
    format(NewData#{<<"limit">> => PerPage, <<"skip">> => Skip});

format(#{<<"perPage">> := PerPage} = Data) ->
    NewData = maps:without([<<"perPage">>, <<"page">>], Data),
    Skip = 0,
    format(NewData#{<<"limit">> => PerPage, <<"skip">> => Skip});

format(#{<<"page">> := Page} = Data) ->
    NewData = maps:without([<<"perPage">>, <<"page">>], Data),
    Skip = (Page - 1) * 10,
    format(NewData#{<<"skip">> => Skip});

format(OldData) ->
    Data = case maps:find(<<"where">>, OldData) of
               {ok, Object} ->
                   NewWhere = format_multilayer(Object),
%%                   io:format("~s ~p NewWhere= ~p ~n", [?FILE, ?LINE, NewWhere]),
                   maps:merge(OldData, #{<<"where">> => NewWhere});
               _ ->
                   OldData
           end,

    Where = maps:without([<<"limit">>, <<"skip">>, <<"order">>, <<"count">>,
        <<"limit">>, <<"keys">>, <<"excludeKeys">>, <<"include">>, <<"where">>,
        <<"perPage">>, <<"page">>, <<"orderBy">>, <<"orderDir">>], Data),
    NewData = maps:without(maps:keys(Where) ++ [<<"perPage">>, <<"page">>, <<"orderBy">>, <<"orderDir">>], Data),
    case Data of
        #{<<"where">> := ParesWhere} when is_map(ParesWhere) ->
            NewData#{<<"where">> => maps:merge(ParesWhere, Where)};
        _ ->
            case length(maps:to_list(Where)) of
                0 ->
                    NewData;
                _ ->
                    NewData#{<<"where">> => Where}
            end
    end.

start_http() ->
    Port = 9089,
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    DocRoot = Root ++ "www",
    dgiot_http_server:start_http(?MODULE, Port, DocRoot).

format_multilayer(Object) ->
    MapWhere = dgiot_utils:to_map(Object),
    maps:fold(
        fun
            (K, V, Acc) when is_map(V) ->
                case maps:values(V) of
                    [<<"">>] ->
                        Acc;
                    _ ->
                        NewK = format_value(K),
                        NewV = dgiot_bamis:format_multilayer(V),
                        Acc#{NewK => NewV}
                end;
            (K, V, Acc) when is_list(V) ->
                NewK = format_value(K),
                NewV =
                    lists:foldl(fun
                                    (V1, Acc1) when is_map(V1) ->
                                        Fv = dgiot_bamis:format_multilayer(V1),
                                        case length(maps:values(Fv)) > 0 of
                                            true ->
                                                Acc1 ++ [Fv];
                                            _ ->
                                                Acc1
                                        end;
                                    (V2, Acc1) ->
                                        case size(dgiot_utils:to_binary(V2)) of
                                            0 ->
                                                Acc1;
                                            _ ->
                                                Acc1 ++ [V2]
                                        end
                                end, [], V),
                case length(NewV) > 0 of
                    true ->
                        Acc#{NewK => NewV};
                    _ ->
                        Acc
                end;
            (K, V, Acc) ->
                case size(dgiot_utils:to_binary(V)) of
                    0 ->
                        Acc;
                    _ ->
                        NewK = format_value(K),
                        Acc#{NewK => V}
                end
        end,
        #{}, MapWhere).
format_value(#{<<"gt">> := V}) ->
    #{<<"$gt">> => V};
format_value(#{<<"gte">> := V}) ->
    #{<<"$gte">> => V};
format_value(#{<<"lt">> := V}) ->
    #{<<"$lt">> => V};
format_value(#{<<"lte">> := V}) ->
    #{<<"$lte">> => V};
format_value(#{<<"ne">> := V}) ->
    #{<<"$ne">> => V};
format_value(#{<<"regex">> := V}) ->
    #{<<"$regex">> => V};
format_value(#{<<"in">> := V}) ->
    #{<<"$in">> => V};
format_value(#{<<"or">> := V}) ->
    #{<<"$or">> => V};
format_value(#{<<"and">> := V}) ->
    #{<<"$and">> => V};
format_value(#{<<"relatedTo">> := V}) ->
    #{<<"$relatedTo">> => V};

format_value(<<"gt">>) ->
    <<"$gt">>;
format_value(<<"gte">>) ->
    <<"$gte">>;
format_value(<<"lt">>) ->
    <<"$lt">>;
format_value(<<"lte">>) ->
    <<"$lte">>;
format_value(<<"ne">>) ->
    <<"$ne">>;
format_value(<<"regex">>) ->
    <<"$regex">>;
format_value(<<"in">>) ->
    <<"$in">>;
format_value(<<"or">>) ->
    <<"$or">>;
format_value(<<"and">>) ->
    <<"$and">>;
format_value(<<"relatedTo">>) ->
    <<"$relatedTo">>;
format_value(V) ->
    V.
