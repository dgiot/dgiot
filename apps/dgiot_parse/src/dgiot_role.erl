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

-module(dgiot_role).
-author("dgiot").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    post_role/2,
    put_role/1,
    put_role/2,
    get_roletemp/3,
    post_roletemp/3,
    put_roletemp/2,
    get_roletemp/2,
    get_role/2,
    get_roletree/1,
    get_parent_role/1,
    remove_roles_role/1,
    get_roles_role/1,
    remove_menus_role/1,
    get_menus_role/1,
    remove_users_roles/1,
    get_users_role/1,
    remove_rules_role/1,
    get_rules_role/1,
    load_roles/0,
    get_childrole/1
]).


get_childrole(Role) ->
    case dgiot_data:values(?PARENT_ROLE_ETS, Role) of
        {error, not_find} ->
            [Role];
        Values ->
            childrole(Values, dgiot_utils:unique_1(Values ++ [Role]))
    end.

childrole([], Acc) ->
    Acc;
childrole([Role | Roles], Acc) ->
    case dgiot_data:values(?PARENT_ROLE_ETS, Role) of
        {error, not_find} ->
            childrole(Roles, Acc);
        Values ->
            childrole(dgiot_utils:unique_1(Roles ++ Values), Acc ++ Values)
    end.

load_roles() ->
    dgiot_data:delete_all_objects(?PARENT_ROLE_ETS),
    Success = fun(Page) ->
        lists:map(fun(X) ->
            #{<<"objectId">> := RoleId, <<"parent">> := #{<<"objectId">> := ParentId}} = X,
            dgiot_data:insert(?ROLE_PARENT_ETS, RoleId, ParentId),
            dgiot_data:insert(?PARENT_ROLE_ETS, ParentId, RoleId),
            dgiot_data:insert(?ROLE_ETS, RoleId, X)
                  end, Page)
              end,
    Query = #{},
    dgiot_parse_loader:start(<<"_Role">>, Query, 0, 500, 10000, Success).

post_role(#{<<"tempname">> := TempName, <<"parent">> := Parent, <<"depname">> := DepName,
    <<"name">> := Name, <<"desc">> := Desc} = Body, SessionToken) ->
    case dgiot_parse:query_object(<<"Dict">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1, <<"where">> => #{<<"key">> => TempName}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [RoleTemp]}} ->
            #{<<"data">> := Data} = RoleTemp,
            NewData = maps:without([<<"createdAt">>, <<"updatedAt">>, <<"objectId">>], Data),
            {ok, ParentInfo} = dgiot_parse:get_object(<<"_Role">>, Parent),
            NewTag = maps:merge(maps:get(<<"tag">>, Body, #{}), maps:get(<<"tag">>, NewData, #{})),
            Role = NewData#{
                <<"name">> => Name,
                <<"desc">> => Desc,
                <<"alias">> => maps:get(<<"alias">>, Body, Desc),
                <<"depname">> => DepName,
                <<"order">> => maps:get(<<"order">>, ParentInfo, 0) + 1,
                <<"level">> => maps:get(<<"level">>, ParentInfo, 0) + 1,
                <<"leafnode">> => true,
                <<"tag">> => NewTag,
                <<"ACL">> => #{
                    <<"role:", Name/binary, ""/utf8>> => #{
                        <<"read">> => true,
                        <<"write">> => true
                    }
                },
                <<"roles">> => [maps:get(<<"name">>, ParentInfo)]
            },
            create_role(Role);
        {error, What} ->
            {error, What}
    end.

create_role(#{<<"name">> := Name} = Role) ->
    RoleId = dgiot_parse_id:get_roleid(Name),
    case dgiot_parse:get_object(<<"_Role">>, RoleId) of
        {error, _} ->
            {ok, AppUser} = dgiot_parse_auth:create_user_for_app(Name),
            ?LOG(info, "AppUser ~p ", [AppUser]),
            NewUsers = maps:get(<<"users">>, Role, []) ++ [AppUser],
            NewRole = Role#{
                <<"users">> => dgiot_role:get_users_role(NewUsers),
                <<"menus">> => dgiot_role:get_menus_role(maps:get(<<"menus">>, Role, [])),
                <<"roles">> => dgiot_role:get_roles_role(maps:get(<<"roles">>, Role, [])),
                <<"parent">> => dgiot_role:get_parent_role(maps:get(<<"roles">>, Role, [])),
                <<"rules">> => dgiot_role:get_rules_role(maps:get(<<"rules">>, Role, []))
            },
            case dgiot_parse:create_object(<<"_Role">>, NewRole) of
                {ok, R} ->
                    load_roles(),
                    {ok, R};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, #{<<"objectId">> := RoleId}} ->
            {error, #{<<"msg">> => <<"role is exist">>}}
    end.

put_role(#{<<"objectId">> := RoleId} = Role, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, _} ->
            case dgiot_parse:get_object(<<"_Role">>, RoleId) of
                {error, _} ->
                    {error, #{<<"msg">> => <<"role is not exist">>}};
                {ok, #{<<"objectId">> := RoleId}} ->
                    NewRole = #{
                        <<"menus">> => dgiot_role:get_menus_role(maps:get(<<"menus">>, Role, [])),
                        <<"rules">> => dgiot_role:get_rules_role(maps:get(<<"rules">>, Role, []))
                    },
                    dgiot_role:remove_menus_role(RoleId),
                    dgiot_role:remove_rules_role(RoleId),
                    case dgiot_parse:update_object(<<"_Role">>, RoleId, NewRole) of
                        {ok, R} ->
                            {ok, R};
                        {error, Reason} ->
                            dgiot_parse:update_object(<<"_Role">>, RoleId, NewRole),
                            {error, Reason}
                    end
            end;
        Error -> Error
    end.

put_role(RoleId) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId) of
        {error, _} ->
            {error, #{<<"msg">> => <<"role is not exist">>}};
        {ok, #{<<"objectId">> := RoleId}} ->
            NewRole = #{
                <<"menus">> => get_role_relation(<<"Menu">>),
                <<"rules">> => get_role_relation(<<"Permission">>)
            },
            dgiot_role:remove_menus_role(RoleId),
            dgiot_role:remove_rules_role(RoleId),
            case dgiot_parse:update_object(<<"_Role">>, RoleId, NewRole) of
                {ok, R} ->
                    {ok, R};
                {error, Reason} ->
                    dgiot_parse:update_object(<<"_Role">>, RoleId, NewRole),
                    {error, Reason}
            end
    end.

get_roletemp(FileName, TempName, SessionToken) ->
    ?LOG(info, "FileName ~p", [FileName]),
    case dgiot_parse:query_object(<<"Dict">>, #{
        <<"order">> => <<"updatedAt">>, <<"limit">> => 1,
        <<"where">> => #{<<"key">> => TempName}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Role}} ->
            BinFile = unicode:characters_to_binary(jsx:encode(Role)),
            case zip:create(FileName, [{dgiot_utils:to_list(FileName) ++ ".json", BinFile}], [memory]) of
                {ok, {_ZipFile, Bin}} ->
                    {ok, Bin};
                {error, What} ->
                    {error, What}
            end;
        _R -> _R
    end.

post_roletemp(Name, TempName, SessionToken) ->
    case get_roletemp(Name, SessionToken) of
        {ok, Data} ->
            case dgiot_parse:query_object(<<"Dict">>, #{
                <<"order">> => <<"updatedAt">>, <<"limit">> => 1,
                <<"where">> => #{<<"key">> => TempName}},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                {ok, #{<<"results">> := [#{<<"objectId">> := RoleId} | _]}} ->
                    dgiot_parse:update_object(<<"Dict">>, RoleId, #{
                        <<"type">> => <<"roletemp">>,
                        <<"key">> => TempName,
                        <<"data">> => Data#{
                            <<"depname">> => TempName,
                            <<"org_type">> => TempName,
                            <<"name">> => TempName,
                            <<"users">> => [],
                            <<"roles">> => []
                        }
                    });
                {ok, #{<<"results">> := []}} ->
                    dgiot_parse:create_object(<<"Dict">>, #{
                        <<"type">> => <<"roletemp">>,
                        <<"key">> => TempName,
                        <<"data">> => Data#{
                            <<"depname">> => TempName,
                            <<"org_type">> => TempName,
                            <<"name">> => TempName,
                            <<"users">> => [],
                            <<"roles">> => []
                        }
                    });
                Error -> Error
            end;
        Error -> Error
    end.

put_roletemp(#{<<"objectId">> := RoleId} = Role, SessionToken) ->
    case dgiot_parse:get_object(<<"Dict">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"data">> := OldRole}} ->
            dgiot_parse:update_object(<<"Dict">>, RoleId, #{
                <<"data">> => maps:without([
                    <<"ACL">>,
                    <<"objectId">>,
                    <<"users">>,
                    <<"parent">>,
                    <<"createdAt">>,
                    <<"updatedAt">>], maps:merge(OldRole, Role))},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        Error -> Error
    end.

get_roletemp(Name, SessionToken) ->
    case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
        <<"where">> => #{<<"name">> => Name}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [Role]}} ->
            #{<<"objectId">> := RoleId} = Role,
            RulesQuery =
                #{<<"where">> => #{<<"$relatedTo">> => #{
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId},
                    <<"key">> => <<"rules">>}
                }
                },
            Rules =
                case dgiot_parse:query_object(<<"Permission">>, RulesQuery) of
                    {ok, #{<<"results">> := Permission}} when length(Permission) > 0 ->
                        Permission;
                    _ -> []
                end,
            NewRules = lists:foldl(fun(X, Acc) ->
                Acc ++ [maps:get(<<"name">>, X)]
                                   end, [], Rules),
%%            ?LOG(info,"Rules ~p", [Rules]),
            MenusQuery =
                #{<<"where">> => #{<<"$relatedTo">> => #{
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId},
                    <<"key">> => <<"menus">>}
                }
                },
            Menus =
                case dgiot_parse:query_object(<<"Menu">>, MenusQuery) of
                    {ok, #{<<"results">> := Menu}} when length(Menu) > 0 ->
                        Menu;
                    _ -> []
                end,
            NewMenus = lists:foldl(fun(X, Acc) ->
                Acc ++ [maps:get(<<"name">>, X)]
                                   end, [], Menus),
            NewRole = maps:without([
                <<"ACL">>,
                <<"parent">>,
                <<"roles">>,
                <<"users">>,
                <<"createdAt">>,
                <<"updatedAt">>,
                <<"objectId">>
            ], Role),
            {ok, NewRole#{
                <<"rules">> => NewRules,
                <<"menus">> => NewMenus
            }};
        _ -> {error, <<"not find">>}
    end.

get_role(Name, SessionToken) ->
    case dgiot_parse:query_object(<<"_Role">>, #{<<"order">> => <<"updatedAt">>, <<"limit">> => 1,
        <<"where">> => #{<<"name">> => Name}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [Role]}} ->
            #{<<"objectId">> := RoleId} = Role,
            RulesQuery =
                #{<<"where">> => #{<<"$relatedTo">> => #{
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId},
                    <<"key">> => <<"rules">>}
                }
                },
            Rules =
                case dgiot_parse:query_object(<<"Permission">>, RulesQuery) of
                    {ok, #{<<"results">> := Permission}} when length(Permission) > 0 ->
                        Permission;
                    _ -> []
                end,
%%            ?LOG(info,"Rules ~p", [Rules]),
            MenusQuery =
                #{<<"where">> => #{<<"$relatedTo">> => #{
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId},
                    <<"key">> => <<"menus">>}
                }
                },
            Menus =
                case dgiot_parse:query_object(<<"Menu">>, MenusQuery) of
                    {ok, #{<<"results">> := Menu}} when length(Menu) > 0 ->
                        Menu;
                    _ -> []
                end,
%%            ?LOG(info,"Menus ~p", [Menus]),
            RolesQuery =
                #{<<"where">> => #{<<"$relatedTo">> => #{
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId},
                    <<"key">> => <<"roles">>}
                }
                },
            Roles =
                case dgiot_parse:query_object(<<"_Role">>, RolesQuery) of
                    {ok, #{<<"results">> := Parent}} when length(Parent) > 0 ->
                        Parent;
                    _ -> []
                end,
%%            ?LOG(info,"Roles ~p", [Roles]),
            UsersQuery =
                #{<<"where">> => #{<<"$relatedTo">> => #{
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId},
                    <<"key">> => <<"users">>}
                }
                },
            Users =
                case dgiot_parse:query_object(<<"_User">>, UsersQuery) of
                    {ok, #{<<"results">> := User}} when length(User) > 0 ->
                        lists:foldl(fun(X, Acc) ->
                            case maps:get(<<"username">>, X) of
                                <<"user_for_", _/binary>> ->
                                    Acc;
                                _ ->
                                    Acc ++ [X]
                            end
                                    end, [], User);
                    _ -> []
                end,
%%            ?LOG(info,"Users ~p", [Users]),
            TmpRole = maps:without([<<"rules">>, <<"menus">>, <<"roles">>, <<"users">>], Role),
            {ok, TmpRole#{
                <<"rules">> => Rules,
                <<"menus">> => Menus,
                <<"roles">> => Roles,
                <<"users">> => Users
            }};
        _ -> {error, <<"not find">>}
    end.

%% todo 后面优化懒加载
get_roletree(SessionToken) ->
    case dgiot_parse:query_object(<<"_Role">>, #{}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Roles}} when length(Roles) > 0 ->
            case dgiot_auth:get_session(SessionToken) of
                #{<<"roles">> := UserRoles} ->
                    Keys = maps:keys(UserRoles),
                    KeyRoles = lists:filter(fun(#{<<"objectId">> := RoleId}) ->
                        lists:member(RoleId, Keys)
                                            end, Roles),
                    [ParentRoleId, _] =
                        lists:foldl(fun(#{<<"objectId">> := RoleId, <<"level">> := Level}, Acc) ->
                            case Acc of
                                [] -> [RoleId, Level];
                                [OldRoleId, OldLevel] ->
                                    case Level < OldLevel of
                                        true -> [RoleId, Level];
                                        false -> [OldRoleId, OldLevel]
                                    end
                            end
                                    end, [], KeyRoles),
                    NewRoles =
                        lists:foldl(fun(#{<<"objectId">> := RoleId} = Role, Acc) ->
                            case RoleId of
                                ParentRoleId ->
                                    Acc ++ [Role#{<<"parent">> => <<"0">>}];
                                _ -> Acc ++ [Role]
                            end
                                    end, [], Roles),
                    RoleTree = dgiot_parse_utils:create_tree(NewRoles, <<"parent">>),
                    Len = length(RoleTree),
                    Num = 1000,
                    case Len =< Num of
                        true ->
                            {200, #{<<"results">> => RoleTree}};
                        false ->
                            ShowMenus = lists:sublist(RoleTree, 1, Num),
                            MoreMenus = lists:sublist(RoleTree, Num + 1, length(RoleTree) - Num),
                            {200, #{<<"results">> => ShowMenus ++ [#{
                                <<"children">> => MoreMenus,
                                <<"name">> => <<"...">>,
                                <<"icon">> => <<"More">>,
                                <<"parent">> => <<"0">>,
                                <<"url">> => <<"/more">>
                            }]}}
                    end;
                _ -> {error, <<"1 not find">>}
            end;
        _ -> {error, <<"2 not find">>}
    end.

get_parent_role(Parents) ->
    Where = #{<<"keys">> => [<<"name">>],
        <<"where">> => #{<<"name">> => #{<<"$in">> => Parents}}},
    case dgiot_parse:query_object(<<"_Role">>, Where) of
        {ok, #{<<"results">> := Results}} when length(Results) == 0 ->
            #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"_Role">>,
                <<"objectId">> => <<"0">>
            };
        {ok, #{<<"results">> := [Results | _]}} ->
            #{<<"objectId">> := ObjectId} = Results,
            #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"_Role">>,
                <<"objectId">> => ObjectId
            };
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            throw({error, Other})
    end.

remove_roles_role(RoleId) ->
    Where = #{<<"$relatedTo">> => #{
        <<"object">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_Role">>,
            <<"objectId">> => RoleId},
        <<"key">> => <<"roles">>
    }},
    Query = #{<<"where">> => Where, <<"order">> => <<"-updatedAt">>},
    case dgiot_parse:query_object(<<"_Role">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> pass;
                _ ->
                    dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"roles">> => #{
                        <<"__op">> => <<"RemoveRelation">>,
                        <<"objects">> => Objects}
                    })
            end;
        _ -> pass
    end.

get_roles_role(Parents) ->
    Where = #{<<"keys">> => [<<"name">>],
        <<"where">> => #{<<"name">> => #{<<"$in">> => Parents}}},
    case dgiot_parse:query_object(<<"_Role">>, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => <<"0">>
                    }]
                };
                _ ->
                    #{
                        <<"__op">> => <<"AddRelation">>,
                        <<"objects">> => Objects
                    }
            end;
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            throw({error, Other})
    end.

remove_menus_role(RoleId) ->
    Where = #{<<"$relatedTo">> => #{
        <<"object">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_Role">>,
            <<"objectId">> => RoleId},
        <<"key">> => <<"menus">>
    }},
    Query = #{<<"where">> => Where, <<"order">> => <<"-updatedAt">>},
    case dgiot_parse:query_object(<<"Menu">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Menu">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> pass;
                _ ->
                    dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"menus">> => #{
                        <<"__op">> => <<"RemoveRelation">>,
                        <<"objects">> => Objects}
                    })
            end;
        _ -> pass
    end.

get_menus_role(Menus) ->
    Where = #{<<"keys">> => [<<"name">>], <<"where">> => #{<<"name">> => #{<<"$in">> => Menus}}},
    case dgiot_parse:query_object(<<"Menu">>, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Menu">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Menu">>,
                        <<"objectId">> => 0
                    }]
                };
                _ ->
                    #{
                        <<"__op">> => <<"AddRelation">>,
                        <<"objects">> => Objects
                    }
            end;
        Other ->
            throw({error, Other})
    end.

get_role_relation(Table) ->
    Where = #{<<"keys">> => [<<"objectId">>]},
    case dgiot_parse:query_object(Table, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => Table,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => Table,
                        <<"objectId">> => 0
                    }]
                };
                _ ->
                    #{
                        <<"__op">> => <<"AddRelation">>,
                        <<"objects">> => Objects
                    }
            end;
        Other ->
            throw({error, Other})
    end.

remove_users_roles(RoleId) ->
    Where = #{<<"$relatedTo">> => #{
        <<"object">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_Role">>,
            <<"objectId">> => RoleId},
        <<"key">> => <<"users">>
    }},
    Query = #{<<"where">> => Where, <<"order">> => <<"-updatedAt">>},
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_User">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> pass;
                _ ->
                    dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"users">> => #{
                        <<"__op">> => <<"RemoveRelation">>,
                        <<"objects">> => Objects}
                    })
            end;
        _ -> pass
    end.

get_users_role(Users) ->
    Where = #{<<"keys">> => [<<"username">>], <<"where">> => #{<<"username">> => #{<<"$in">> => Users}}},
    case dgiot_parse:query_object(<<"_User">>, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_User">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_User">>,
                        <<"objectId">> => 0
                    }]
                };
                _ ->
                    #{
                        <<"__op">> => <<"AddRelation">>,
                        <<"objects">> => Objects
                    }
            end;
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            {error, Other}
    end.

remove_rules_role(RoleId) ->
    Where = #{<<"$relatedTo">> => #{
        <<"object">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_Role">>,
            <<"objectId">> => RoleId},
        <<"key">> => <<"rules">>
    }},
    Query = #{<<"where">> => Where, <<"order">> => <<"-updatedAt">>},
    case dgiot_parse:query_object(<<"Permission">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Permission">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> pass;
                _ ->
                    dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"rules">> =>
                    #{<<"__op">> => <<"RemoveRelation">>,
                        <<"objects">> => Objects}
                    })
            end;
        _ -> pass
    end.

get_rules_role([<<"*">>]) ->
    Where = #{<<"keys">> => [<<"name">>]},
    case dgiot_parse:query_object(<<"Permission">>, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId}, Acc) ->
                    Acc ++ [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Permission">>,
                        <<"objectId">> => ObjectId
                    }]
                end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Permission">>,
                        <<"objectId">> => 0
                    }]
                };
                _ ->
                    #{
                        <<"__op">> => <<"AddRelation">>,
                        <<"objects">> => Objects
                    }
            end;
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            {error, Other}
    end;

get_rules_role(Rules) ->
    Where = #{<<"keys">> => [<<"name">>]},
    case dgiot_parse:query_object(<<"Permission">>, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects = lists:foldl(
                fun(#{<<"objectId">> := ObjectId, <<"name">> := Name}, Acc) ->
                    case lists:member(Name, Rules) of
                        true ->
                            Acc ++ [#{
                                <<"__type">> => <<"Pointer">>,
                                <<"className">> => <<"Permission">>,
                                <<"objectId">> => ObjectId
                            }];
                        false ->
                            Acc
                    end
                end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Permission">>,
                        <<"objectId">> => 0
                    }]
                };
                _ ->
                    #{
                        <<"__op">> => <<"AddRelation">>,
                        <<"objects">> => Objects
                    }
            end;
        Other ->
            ?LOG(info, "Other ~p", [Other]),
            {error, Other}
    end.


