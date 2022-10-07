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

-module(dgiot_factory_utils).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_factory.hrl").
-export([get_num/2, get_name/2, turn_name/2, turn_num/3]).
-export([get_usertree/2, getalluser/1]).
-export([get_zero_list/1, get_zero_binary/1]).
-export([fix_model/1, get_worker/1,get_children/1]).

fix_model(ID) ->
    {ok, #{<<"thing">> := Model}} = dgiot_parse:get_object(<<"Product">>, ID),
    {ok, Pro} = maps:find(<<"properties">>, Model),
    Res = lists:foldl(
        fun(X, Acc) ->
            case X of
                #{<<"dataType">> := #{<<"type">> := <<"text">>, <<"specs">> := #{<<"max">> := 999999}}} ->
                    NweData = #{<<"das">> => [], <<"size">> => 50, <<"specs">> => #{}, <<"type">> => <<"text">>},
                    NewX = maps:merge(X, #{<<"dataType">> => NweData}),
                    Acc ++ [NewX];
                _ ->
                    Acc ++ [X]
            end
        end, [], Pro),
    NewThing = maps:merge(Model, #{<<"properties">> => Res}),
    dgiot_parse:update_object(<<"Product">>, ID, #{<<"thing">> => NewThing}).


get_num(K, V) ->
    Id = dgiot_parse_id:get_dictid(K, K, K, K),
    case dgiot_parse:get_object(<<"Dict">>, Id) of
        {ok, #{<<"data">> := #{<<"end">> := End} = Dict}} ->
            case maps:find(V, Dict) of
                {ok, Num} ->
                    #{K => Num};
                _ ->
                    case dgiot_parse:update_object(<<"Dict">>, Id, #{<<"data">> => Dict#{<<"end">> => End + 1, V => End}}) of
                        {ok, _} ->
                            #{K => End};
                        _ ->
                            error
                    end
            end;
        _ ->
            Map = #{<<"class">> => K,
                <<"title">> => K,
                <<"type">> => K,
                <<"key">> => K,
                <<"data">> => #{<<"end">> => 1, V => 0}
            },
            case dgiot_parse:create_object(<<"Dict">>, Map) of
                {ok, _} ->
                    #{K => 0};
                _ ->
                    error
            end
    end.

get_name(K, Num) ->
    Id = dgiot_parse_id:get_dictid(K, K, K, K),
    case dgiot_parse:get_object(<<"Dict">>, Id) of
        {ok, #{<<"data">> := Dict}} ->
            TupleList = maps:to_list(Dict),
            case lists:keytake(Num, 2, TupleList) of
                {value, {Name, _}, _} ->
                    #{K => Name};
                _ ->
                    error
            end;
        _ ->
            error
    end.


turn_name(Data, ThingMap) when is_list(Data) ->
    lists:foldl(
        fun(X, ACC) ->
            ACC ++ turn_name(X, ThingMap)
        end, [], Data);

turn_name(Data, ThingMap) when is_map(Data) ->
    Res = maps:fold(
        fun(K, V, Acc) ->
            case V of
                <<"enum">> ->
                    case maps:find(K, Acc) of
                        {ok, Num} ->
                            case get_name(K, Num) of
                                error ->
                                    Acc;
                                Map ->
                                    maps:merge(Acc, Map)
                            end;
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end, Data, ThingMap),
    [Res];

turn_name(Data, _) ->
    Data.

turn_num(FlatMap, ProductId, Type) ->
    case dgiot_product:get_device_thing(ProductId, Type) of
        {ok, ThingMap} ->
            maps:fold(
                fun(K, V, Acc) ->
                    case V of
                        <<"enum">> ->
                            case maps:find(K, Acc) of
                                {ok, Data} ->
                                    case dgiot_factory_utils:get_num(K, Data) of
                                        error ->
                                            Acc;
                                        Map ->
                                            maps:merge(Acc, Map)
                                    end;
                                _ ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end

                end, FlatMap, ThingMap);
        _ ->
            FlatMap
    end.


get_usertree(#{<<"id">> := undefined},SessionToken) ->
    case get_same_level_role(SessionToken) of
        RoleTree when length(RoleTree) > 0 ->
            get_children(RoleTree);
        _ ->
            []
    end;
get_usertree(#{<<"id">> := Id}, _) ->
    get_worker(Id).


get_same_level_role(SessionToken) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"roles">> := Roles} ->
            maps:fold(
                fun(_RoleId, #{<<"parent">> := Parent}, Acc) ->
                    ChildroleIds = dgiot_role:get_childrole(Parent) -- [Parent],
                    case dgiot_parse:query_object(<<"_Role">>, #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => ChildroleIds}}}) of
                        {ok, #{<<"results">> := RoleList}} ->
                            Acc ++ dgiot_parse_utils:create_tree(RoleList, <<"parent">>);
                        _ ->
                            Acc
                    end
                end, [], Roles);
        _ ->
            []
    end.

get_worker(Id) ->
%%                io:format("~s ~p Id = ~p  ~n", [?FILE, ?LINE, Id]),
%%    case dgiot_parse:get_object(<<"_Role">>, Id) of
%%        {ok, #{<<"parent">> := Parent}} ->
            ChildroleIds = dgiot_role:get_childrole(Id) -- [Id],
            case dgiot_parse:query_object(<<"_Role">>, #{<<"where">> => #{<<"objectId">> => #{<<"$in">> =>ChildroleIds}}}) of
                {ok, #{<<"results">> := RoleList}} ->
                    get_children(dgiot_parse_utils:create_tree(RoleList, <<"parent">>));
                _ ->
                    io:format("~s ~p Id = ~p  ~n", [?FILE, ?LINE, Id]),
                    error
%%            end
    end.

get_children(Results) ->
    lists:foldl(
        fun(Role, Acc) ->
%%            io:format("~s ~p Role = ~p  ~n", [?FILE, ?LINE, Role]),
            X1 = getalluser(Role),
            X2 = case maps:find(<<"children">>, X1) of
                     error ->
                         #{<<"userlist">> := Userlist} = X1,
                         X1#{<<"children">> => Userlist};
                     {ok, SubChildren} ->
                         #{<<"userlist">> := Userlist} = X1,
                         X1#{<<"children">> => get_children(SubChildren) ++ Userlist}
                 end,
            Acc ++ [maps:without([<<"objectId">>, <<"userlist">>], X2)]
        end, [], Results).

getalluser(#{<<"objectId">> := RoleId, <<"name">> := Depname} = Role) ->
    UserIds = dgiot_parse_auth:get_UserIds(RoleId),
    UsersQuery =
        #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => UserIds}}},
    UserList =
        case dgiot_parse:query_object(<<"_User">>, UsersQuery) of
            {ok, #{<<"results">> := Results}} ->
                Results;
            _ ->
                []
        end,
    NewUserList =
        lists:foldl(fun(X, Acc) ->
            case X of
                #{<<"nick">> := <<"user_for_", _/binary>>} ->
                    Acc;
                #{<<"nick">> := Nick, <<"username">> := UserName} ->
                    dgiot_data:insert(?WORKERTREE, UserName, Nick),
%%                    Acc ++ [#{<<"label">> => Nick, <<"value">> => <<UserName/binary, "_", Nick/binary>>}];
                    Acc ++ [#{<<"label">> => Nick, <<"value">> => Nick}];
                _ ->
                    Acc
            end
                    end, [], UserList),
    case maps:find(<<"children">>, Role) of
        error ->
            #{<<"objectId">> => RoleId, <<"name">> => Depname, <<"label">> => Depname, <<"userlist">> => NewUserList};
        {ok, SubChildren} ->
            #{<<"objectId">> => RoleId, <<"name">> => Depname, <<"label">> => Depname, <<"children">> => SubChildren, <<"userlist">> => NewUserList}
    end.




get_zero_list(Num) ->
    get_zero_list([], Num).

get_zero_list(Acc, Num) ->
    case Num > 0 of
        true ->
            get_zero_list(Acc ++ [0], Num - 1);
        _ ->
            Acc
    end.

get_zero_binary(Num) ->
    get_zero_binary(<<>>, Num).

get_zero_binary(Acc, Num) ->
    case Num > 0 of
        true ->
            get_zero_binary(<<Acc/binary, "0">>, Num - 1);
        _ ->
            Acc
    end.

