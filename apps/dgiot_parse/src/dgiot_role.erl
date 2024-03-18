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
    get_views_role/1,
    get_menuviews_role/1,
    remove_users_roles/1,
    get_users_role/1,
    remove_rules_role/1,
    get_rules_role/1,
    load_roles/0,
    load_user/0,
    get_user/1,
    get_childrole/1,
    get_childacl/1,
    get_roleids/1,
    get_alcname/1,
    get_aclNames/1,
    get_acls/1,
    get_acl/1,
    get_rolenames/1,
    save_role_view/1,
    save_role_menuview/1,
    get_role_views/1,
    get_role_view/2
]).

get_childacl(AclName) ->
    case dgiot_data:get(?NAME_ROLE_ETS, dgiot_utils:to_atom(AclName)) of
        not_find ->
            [dgiot_utils:to_atom(AclName)];
        RoleId ->
            ChildAcl =
                lists:foldl(fun(ChilRoleId, Acc) ->
                    Acc ++ [get_alcname(ChilRoleId)]
                            end, [dgiot_utils:to_atom(AclName)], get_childrole(RoleId)),
            dgiot_utils:unique_1(ChildAcl)
    end.

get_childrole(Role) ->
    case dgiot_data:values(?PARENT_ROLE_ETS, Role) of
        {error, not_find} ->
            [Role];
        Values ->
            FlatValues = dgiot_utils:unique_1(lists:flatten(Values)),
            dgiot_utils:unique_1(lists:flatten(childrole(FlatValues, FlatValues ++ [Role])))
    end.

childrole([], Acc) ->
    Acc;
childrole([Role | Roles], Acc) ->
    case dgiot_data:values(?PARENT_ROLE_ETS, Role) of
        {error, not_find} ->
            childrole(Roles, Acc);
        Values ->
            FlatValues = dgiot_utils:unique_1(lists:flatten(Values)),
            childrole(dgiot_utils:unique_1(Roles ++ FlatValues), Acc ++ FlatValues)
    end.

load_roles() ->
    dgiot_data:delete_all_objects(?PARENT_ROLE_ETS),
    Success = fun(Page) ->
        lists:map(fun(X) ->
            #{<<"objectId">> := RoleId, <<"name">> := RoleName, <<"parent">> := #{<<"objectId">> := ParentId}} = X,
            save_role_view(RoleId),
            save_role_menuview(RoleId),
            dgiot_data:insert(?ROLE_PARENT_ETS, RoleId, ParentId),
            dgiot_data:insert(?PARENT_ROLE_ETS, ParentId, RoleId),
            dgiot_data:insert(?NAME_ROLE_ETS, dgiot_utils:to_atom(<<"role:", RoleName/binary>>), RoleId),
            dgiot_data:insert(?ROLE_NAME_ETS, RoleId, dgiot_utils:to_atom(<<"role:", RoleName/binary>>)),
            dgiot_data:insert(?ROLE_ETS, RoleId, maps:without([<<"objectId">>, <<"createdAt">>, <<"users">>, <<"menus">>, <<"rules">>, <<"dict">>], X))
                  end, Page)
              end,
    Query = #{},
    dgiot_parse_loader:start(<<"_Role">>, Query, 0, 500, 10000, Success).

load_user() ->
    Success = fun(Page) ->
        lists:map(fun(User) ->
            user_ets(User)
                  end, Page)
              end,
    Query = #{},
    dgiot_parse_loader:start(<<"_User">>, Query, 0, 500, 10000, Success).

user_ets(#{<<"objectId">> := UserId} = User) ->
    dgiot_data:insert(?USER_ETS, UserId, maps:without([<<"objectId">>, <<"createdAt">>, <<"emailVerified">>, <<"_email_verify_token">>, <<"_failed_login_count">>, <<"updatedAt">>, <<"authData">>, <<"role">>, <<"roles">>, <<"tag">>], User)).

get_user(UserId) ->
    case dgiot_data:get(?USER_ETS, UserId) of
        not_find ->
            not_find;
        User ->
            User
    end.

get_acls(Device) when is_map(Device) ->
    Acl = maps:get(<<"ACL">>, Device, #{}),
    get_acls(maps:keys(Acl));
get_acls(Acls) when length(Acls) == 0 ->
    ['*'];
get_acls(Acls) when is_list(Acls) ->
    AclsNames =
        lists:foldl(
            fun
                (RoleName, Acc) ->
                    Acc ++ [dgiot_utils:to_atom(RoleName)]
            end,
            [], Acls),

    case length(AclsNames) of
        0 ->
            ['*'];
        _ ->
            AclsNames
    end.

get_roleids(Device) when is_map(Device) ->
    Acl = maps:get(<<"ACL">>, Device, #{}),
    get_roleids(maps:keys(Acl));

get_roleids(Acls) when is_list(Acls) ->
    Result =
        lists:foldl(
            fun
                (<<"*">>, Acc1) ->
                    Acc1 ++ ['*'];
                (RoleName, Acc) ->
                    case dgiot_data:get(?NAME_ROLE_ETS, dgiot_utils:to_atom(RoleName)) of
                        not_find ->
                            Acc;
                        RoleId ->
                            Acc ++ [RoleId]
                    end
            end, [], Acls),
    case length(Result) of
        0 ->
            ['*'];
        _ ->
            Result
    end.

get_alcname(RoleId) ->
    case dgiot_data:get(?ROLE_NAME_ETS, dgiot_utils:to_binary(RoleId)) of
        not_find ->
            dgiot_utils:to_atom(RoleId); %%  * or userid
        RoleName ->
            RoleName
    end.

get_aclNames(Roles) ->
    lists:foldl(fun(RoleId, Acc) ->
        Acc ++ [dgiot_utils:to_binary(dgiot_role:get_alcname(RoleId))]
                end, [<<"*">>], maps:keys(Roles)).

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
                    <<"role:", Name/binary>> => #{
                        <<"read">> => true,
                        <<"write">> => true
                    }
                },
                <<"roles">> => [maps:get(<<"name">>, ParentInfo)]
            },
            create_role(Role);
        {_, What} ->
            {error, What}
    end.

create_role(#{<<"name">> := Name} = Role) ->
    RoleId = dgiot_parse_id:get_roleid(Name),
    case dgiot_parse:get_object(<<"_Role">>, RoleId) of
        {error, _} ->
            {ok, AppUser} = dgiot_parse_auth:create_user_for_app(Name),
            NewUsers = maps:get(<<"users">>, Role, []) ++ [AppUser],
            NewRole = Role#{
                <<"users">> => dgiot_role:get_users_role(NewUsers),
                <<"menus">> => dgiot_role:get_menus_role(maps:get(<<"menus">>, Role, [])),
                <<"roles">> => dgiot_role:get_roles_role(maps:get(<<"roles">>, Role, [])),
                <<"parent">> => dgiot_role:get_parent_role(maps:get(<<"roles">>, Role, [])),
                <<"rules">> => dgiot_role:get_rules_role(maps:get(<<"rules">>, Role, []))
            },
            case dgiot_parse:create_object(<<"_Role">>, maps:without([<<"views">>, <<"menuviews">>], NewRole)) of
                {ok, R} ->
                    load_roles(),
                    {ok, R};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, #{<<"objectId">> := RoleId}} ->
            {error, #{<<"objectId">> => RoleId, <<"msg">> => <<"role is exist">>}}
    end.

put_role(#{<<"objectId">> := RoleId} = Role, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, _} ->
            case dgiot_parse:get_object(<<"_Role">>, RoleId) of
                {error, _} ->
                    {error, #{<<"msg">> => <<"role is not exist">>}};
                {ok, #{<<"objectId">> := RoleId}} ->
                    RolesMenus =
                        case maps:find(<<"menus">>, Role) of
                            {ok, Menus} ->
                                dgiot_role:remove_menus_role(RoleId),
                                #{
                                    <<"menus">> => dgiot_role:get_menus_role(Menus),
                                    <<"menuviews">> => dgiot_role:get_menuviews_role(Menus)
                                };
                            _ ->
                                #{}
                        end,
                    NewRole =
                        case maps:find(<<"rules">>, Role) of
                            {ok, Rules} ->
                                dgiot_role:remove_rules_role(RoleId),
                                RolesMenus#{<<"rules">> => dgiot_role:get_rules_role(Rules)};
                            _ ->
                                RolesMenus
                        end,
                    case dgiot_parse:update_object(<<"_Role">>, RoleId, NewRole) of
                        {ok, R} ->
                            _R2 = dgiot_role:save_role_menuview(RoleId),
                            {ok, R};
                        {error, _Reason} ->
                            Result = dgiot_parse:update_object(<<"_Role">>, RoleId, NewRole),
                            _R3 = dgiot_role:save_role_menuview(RoleId),
                            Result
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
    case dgiot_parse:query_object(<<"Dict">>, #{
        <<"order">> => <<"updatedAt">>, <<"limit">> => 1,
        <<"where">> => #{<<"key">> => TempName}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Role}} ->
            BinFile = unicode:characters_to_binary(dgiot_json:encode(Role)),
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

            ViewsQuery =
                #{<<"where">> => #{<<"$relatedTo">> => #{
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => <<"6a08d0cf31">>},
                    <<"key">> => <<"views">>}
                }},
            Views =
                case dgiot_parse:query_object(<<"View">>, ViewsQuery) of
                    {ok, #{<<"results">> := View}} when length(View) > 0 ->
                        View;
                    _ -> []
                end,
            NewViews = lists:foldl(fun(X, Acc) ->
                Acc ++ [maps:get(<<"objectId">>, X)]
                                   end, [], Views),

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
                <<"views">> => NewViews,
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
            throw({error, Other})
    end.

get_menuviews_role(Menus) ->
    Where = #{<<"keys">> => [<<"meta">>], <<"where">> => #{<<"name">> => #{<<"$in">> => Menus}}},
    case dgiot_parse:query_object(<<"Menu">>, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects =
                lists:foldl(
                    fun
                        (#{<<"meta">> := #{<<"viewid">> := ViewId}}, Acc) when size(ViewId) > 0 ->
                            Acc ++ [#{
                                <<"__type">> => <<"Pointer">>,
                                <<"className">> => <<"View">>,
                                <<"objectId">> => ViewId
                            }];
                        (_, Acc) ->
                            Acc
                    end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"View">>,
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
            throw({error, Other})
    end.

get_views_role(Views) ->
    Where = #{<<"keys">> => [<<"meta">>], <<"where">> => #{<<"objectId">> => #{<<"$in">> => Views}}},
    case dgiot_parse:query_object(<<"View">>, Where) of
        {ok, #{<<"results">> := Results}} ->
            Objects =
                lists:foldl(
                    fun(#{<<"objectId">> := ObjectId}, Acc) ->
                            Acc ++ [#{
                                <<"__type">> => <<"Pointer">>,
                                <<"className">> => <<"View">>,
                                <<"objectId">> => ObjectId
                            }];
                        (_, Acc) ->
                            Acc
                    end, [], Results),
            case Objects of
                [] -> #{
                    <<"__op">> => <<"AddRelation">>,
                    <<"objects">> => [#{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"View">>,
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
            {error, Other}
    end.

get_acl(Role) ->
    lists:foldl(fun
                    (#{<<"name">> := Rolename}, Acc) ->
                        Acc#{<<"role:", Rolename/binary>> => #{<<"read">> => true, <<"write">> => true}};
                    (_, Acc) ->
                        Acc
                end, #{}, maps:values(Role)).


get_rolenames(Roles) ->
    lists:foldl(fun
                    (#{<<"name">> := Rolename}, Acc) ->
                        Acc ++ [Rolename];
                    (_, Acc) ->
                        Acc
                end, [], maps:values(Roles)).

save_role_view(RoleId) ->
    Where = #{<<"$relatedTo">> => #{
        <<"object">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_Role">>,
            <<"objectId">> => RoleId},
        <<"key">> => <<"views">>
    }},
    Query = #{<<"where">> => Where, <<"order">> => <<"-updatedAt">>},
    case dgiot_parse:query_object(<<"View">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            ViewIds =
                lists:foldl(
                    fun(#{<<"objectId">> := ObjectId}, Acc) ->
                        Acc ++ [ObjectId]
                    end, [], Results),
            dgiot_data:insert(?ROLE_VIEWS_ETS, RoleId, ViewIds);
        _ -> pass
    end.

save_role_menuview(RoleId) ->
    Where = #{<<"$relatedTo">> => #{
        <<"object">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_Role">>,
            <<"objectId">> => RoleId},
        <<"key">> => <<"menuviews">>
    }},
    Query = #{<<"where">> => Where, <<"order">> => <<"-updatedAt">>},
    case dgiot_parse:query_object(<<"View">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            ViewIds =
                lists:foldl(
                    fun(#{<<"objectId">> := ObjectId}, Acc) ->
                        Acc ++ [ObjectId]
                    end, [], Results),
            dgiot_data:insert(?ROLE_MENUVIEWS_ETS, RoleId, ViewIds);
        _ -> pass
    end.

get_role_views(RoleId) ->
    case dgiot_data:get(?ROLE_VIEWS_ETS, RoleId) of
        not_find ->
            not_find;
        ViewIds ->
            ViewIds
    end.

get_role_view(RoleId, ViewId) ->
    case dgiot_data:get(?ROLE_VIEWS_ETS, RoleId) of
        not_find ->
            not_find;
        ViewIds ->
            AllViewIds =
                case dgiot_data:get(?ROLE_MENUVIEWS_ETS, RoleId) of
                    not_find ->
                        ViewIds;
                    MenuviewIds ->
                        ViewIds ++ MenuviewIds
                end,
            case lists:member(ViewId, AllViewIds) of
                true ->
                    ViewId;
                _ ->
                    not_find
            end
    end.
