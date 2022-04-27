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

-module(dgiot_parse_auth).
-author("dgiot").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    login/2,
    get_role/2,
    add_to_role/4,
    load_roleuser/0,
    create_session/3,
    check_session/1,
    refresh_session/1,
    save_User_Role/2,
    del_User_Role/2,
    put_User_Role/3,
    put_roleuser/2,
    post_roleuser/2,
    get_roleuser/2,
    del_roleuser/2
]).
-export([create_user/2, delete_user/2, put_user/2, disableusere/3, check_roles/1]).
-export([login_by_account/2, login_by_token/2, login_by_mail_phone/1, do_login/1]).
-export([create_user_for_app/1, get_token/1, set_cookies/3, add_acl/5]).
%% 登录
login(UserName, Password) ->
    login(?DEFAULT, UserName, Password).
login(Name, UserName, Password) ->
    Path = <<"/login">>,
    Args = #{<<"username">> => UserName, <<"password">> => Password},
    dgiot_parse:request_rest(Name, 'GET', [], Path, Args, [{from, rest}]).

load_roleuser() ->
    dgiot_data:delete_all_objects(?PARENT_ROLE_ETS),
    Success = fun(Page) ->
        lists:map(fun
                      (#{<<"objectId">> := RoleId}) ->
                          role_ets(RoleId);
                      (_) ->
                          pass
                  end, Page)
              end,
    Query = #{<<"keys">> => <<"parent">>},
    dgiot_parse_loader:start(<<"_Role">>, Query, 0, 10, 10000, Success).

role_ets(RoleId) ->
    UsersQuery =
        #{<<"keys">> => <<"objectId">>,
            <<"where">> => #{<<"$relatedTo">> => #{
                <<"object">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"_Role">>,
                    <<"objectId">> => RoleId},
                <<"key">> => <<"users">>}
            }},
    case dgiot_parse:query_object(<<"_User">>, UsersQuery) of
        {ok, #{<<"results">> := Users}} when length(Users) > 0 ->
            UserIds =
                lists:foldl(fun(#{<<"objectId">> := UserId}, Acc) ->
                    save_RoleIds(UserId, RoleId),
                    Acc ++ [UserId]
                            end, [], Users),
            dgiot_data:insert(?ROLE_USER_ETS, RoleId, UserIds);
        _ -> pass
    end.

save_RoleIds(UserId, RoleId) ->
    case dgiot_data:get(?USER_ROLE_ETS, UserId) of
        not_find ->
            dgiot_data:insert(?USER_ROLE_ETS, UserId, [RoleId]);
        RoleIds ->
            New_RoleIds = dgiot_utils:unique_2(RoleIds ++ [RoleId]),
            dgiot_data:insert(?USER_ROLE_ETS, UserId, New_RoleIds)
    end.

save_User_Role(UserId, RoleId) ->
    case dgiot_data:get(?USER_ROLE_ETS, UserId) of
        not_find ->
            dgiot_data:insert(?USER_ROLE_ETS, UserId, [RoleId]);
        RoleIds ->
            New_RoleIds = dgiot_utils:unique_2(RoleIds ++ [RoleId]),
            dgiot_data:insert(?USER_ROLE_ETS, UserId, New_RoleIds)
    end,

    case dgiot_data:get(?ROLE_USER_ETS, RoleId) of
        not_find ->
            dgiot_data:insert(?ROLE_USER_ETS, RoleId, [UserId]);
        UserIds ->
            New_UserIds = dgiot_utils:unique_2(UserIds ++ [UserId]),
            dgiot_data:insert(?ROLE_USER_ETS, RoleId, New_UserIds)
    end.


del_User_Role(UserId, RoleId) ->
    case dgiot_data:get(?USER_ROLE_ETS, UserId) of
        not_find ->
            pass;
        RoleIds when length(RoleIds) > 0 ->
            dgiot_data:delete(?USER_ROLE_ETS, UserId);
        _ ->
            pass
    end,
    case dgiot_data:get(?ROLE_USER_ETS, RoleId) of
        not_find ->
            pass;
        UserIds when length(UserIds) > 0 ->
            New_UserIds = lists:delete(UserId, UserIds),
            dgiot_data:insert(?ROLE_USER_ETS, RoleId, New_UserIds);
        _ ->
            pass
    end.

put_User_Role(UserId, OldRoleId, NewRoleId) ->
    case dgiot_data:get(?USER_ROLE_ETS, UserId) of
        not_find ->
            pass;
        RoleIds when length(RoleIds) > 0 ->
            Old_RoleIds = lists:delete(OldRoleId, RoleIds),
            New_RoleIds = dgiot_utils:unique_2(Old_RoleIds ++ [NewRoleId]),
            dgiot_data:insert(?USER_ROLE_ETS, UserId, New_RoleIds);
        _ ->
            pass
    end,
    case dgiot_data:get(?ROLE_USER_ETS, OldRoleId) of
        not_find ->
            pass;
        OldUserIds when length(OldUserIds) > 0 ->
            Old_UserIds = lists:delete(UserId, OldUserIds),
            dgiot_data:insert(?ROLE_USER_ETS, OldRoleId, Old_UserIds);
        _ ->
            pass
    end,
    case dgiot_data:get(?ROLE_USER_ETS, NewRoleId) of
        not_find ->
            pass;
        NewUserIds when length(NewUserIds) > 0 ->
            New_UserIds = dgiot_utils:unique_2(NewUserIds ++ [UserId]),
            dgiot_data:insert(?ROLE_USER_ETS, NewRoleId, New_UserIds);
        _ ->
            pass
    end.

%% 更新Session
create_session(UserId, TTL, Name) ->
    Token = new_token(),
    SessionToken = <<"r:", Token/binary>>,
    case create_session_(UserId, SessionToken, TTL) of
        {ok, UserInfo} ->
            case do_login(UserInfo, TTL) of
                {ok, NewUserInfo} ->
                    {ok, NewUserInfo#{
                        <<"sessionToken">> => SessionToken,
                        <<"access_token">> => SessionToken,
                        <<"expires_in">> => TTL,
                        <<"desc">> => Name,
                        <<"name">> => Name
                    }};
                {error, ErrMsg} ->
                    {error, ErrMsg}
            end;
        {error, #{<<"code">> := Code, <<"error">> := Err}} ->
            {error, #{<<"code">> => Code, <<"error">> => Err}};
        {error, Reason} ->
            ?LOG(error, "refresh_token ~p, ~p~n", [UserId, Reason]),
            {error, #{<<"code">> => 1, <<"error">> => <<"Internal server error.">>}}
    end.

create_session_(UserId, SessionToken, TTL) ->
    create_session(?DEFAULT, UserId, SessionToken, TTL).
create_session(Name, UserId, SessionToken, TTL) ->
    case dgiot_parse:get_object(Name, <<"_User">>, binary:replace(UserId, <<" ">>, <<>>, [global])) of
        {ok, #{<<"objectId">> := UserId} = UserInfo} ->
            Now = dgiot_datetime:nowstamp() + dgiot_utils:to_int(TTL) - 8 * 60 * 60,
            SessionId = dgiot_parse_id:get_sessionId(SessionToken),
            Map = #{
                <<"objectId">> => SessionId,
                <<"sessionToken">> => SessionToken,
                <<"restricted">> => false,
                <<"installationId">> => <<>>,
                <<"expiresAt">> => #{
                    <<"__type">> => <<"Date">>,
                    <<"iso">> => dgiot_datetime:format(Now, <<"YY-MM-DDTHH:NN:SS.000Z">>)
                },
                <<"user">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"_User">>,
                    <<"objectId">> => UserId
                },
                <<"createdWith">> => #{
                    <<"action">> => <<"login">>,
                    <<"authProvider">> => <<"token">>
                }
            },
            case dgiot_parse:create_object(Name, <<"_Session">>, Map) of
                {ok, #{<<"objectId">> := _SessionId}} ->
                    {ok, UserInfo#{<<"sessionToken">> => SessionToken}};
                {error, Why} ->
                    {error, Why}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

check_session(Token) ->
    check_session(?DEFAULT, Token).
check_session(Name, Token) ->
    Now = dgiot_datetime:nowstamp() - 8 * 60 * 60,
    Where = #{
        <<"objectId">> => #{
            <<"$select">> => #{
                <<"key">> => <<"user">>,
                <<"query">> => #{
                    <<"className">> => <<"_Session">>,
                    <<"where">> => #{
                        <<"sessionToken">> => Token,
                        <<"expiresAt">> => #{
                            <<"$gte">> => #{
                                <<"__type">> => <<"Date">>,
                                <<"iso">> => dgiot_datetime:format(Now, <<"YY-MM-DDTHH:NN:SS.000Z">>)
                            }
                        }
                    }
                }
            }
        }
    },
    case dgiot_parse:query_object(Name, <<"_User">>, #{<<"where">> => Where, <<"limit">> => 1}) of
        {ok, #{<<"results">> := [User]}} ->
            {ok, User};
        {ok, #{<<"results">> := []}} ->
            {error, #{<<"code">> => 101, <<"error">> => <<"Object not found.">>}};
        {error, Reason} ->
            {error, Reason}
    end.

refresh_session(Token) ->
    SessionId = dgiot_parse_id:get_sessionId(Token),
    Now = dgiot_datetime:nowstamp() + dgiot_auth:ttl(),
    dgiot_parse:update_object(<<"_Session">>, SessionId, #{
        <<"expiresAt">> => #{
            <<"__type">> => <<"Date">>,
            <<"iso">> => dgiot_datetime:format(Now, <<"YY-MM-DDTHH:NN:SS.000Z">>)
        }
    }).


get_roleuser(Filter, SessionToken) ->
    case dgiot_parse:query_object(<<"_Role">>, Filter,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Roles}} ->
            Users =
                lists:foldl(fun(#{<<"objectId">> := RoleId} = Role, Acc) ->
                    UsersQuery =
                        #{<<"where">> => #{<<"$relatedTo">> => #{
                            <<"object">> => #{
                                <<"__type">> => <<"Pointer">>,
                                <<"className">> => <<"_Role">>,
                                <<"objectId">> => RoleId
                            },
                            <<"key">> => <<"users">>}
                        }
                        },
                    case dgiot_parse:query_object(<<"_User">>, UsersQuery) of
                        {ok, #{<<"results">> := Results}} ->

                            Acc ++ lists:foldl(fun(X, Acc2) ->
                                Acc2 ++ [X#{<<"role">> => maps:with([<<"org_type">>, <<"tag">>, <<"depname">>], Role)}]
                                               end, [], Results);
                        _ -> Acc
                    end
                            end, [], Roles),
            NewUsers =
                lists:foldl(fun(#{<<"username">> := UsrName} = User, Acc1) ->
                    case UsrName of
                        <<"user_for", _/binary>> -> Acc1;
                        _ -> Acc1 ++ [User]
                    end
                            end, [], dgiot_utils:unique_1(Users)),
%%            ?LOG(info,"NewUsers ~p ", [NewUsers]),
            {ok, #{<<"results">> => NewUsers}};
        Error ->
            Error
    end.

put_roleuser(#{<<"userid">> := UserId} = Body, SessionToken) ->
    R1 =
        case maps:is_key(<<"delfilter">>, Body) of
            true ->
                DelFilter = maps:get(<<"delfilter">>, Body),
                case dgiot_parse:query_object(<<"_Role">>, DelFilter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                    {ok, #{<<"results">> := DelRoles}} ->
                        lists:foldl(
                            fun(#{<<"objectId">> := RoleId}, Acc) ->
                                {_, R0} =
                                    dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"users">> => #{
                                        <<"__op">> => <<"RemoveRelation">>,
                                        <<"objects">> => [
                                            #{
                                                <<"__type">> => <<"Pointer">>,
                                                <<"className">> => <<"_User">>,
                                                <<"objectId">> => UserId
                                            }
                                        ]}
                                    }),
                                Acc ++ [#{<<"del">> => R0}]
                            end, [], DelRoles);
                    _ -> []
                end;
            _ -> []
        end,
    ?LOG(info, "Body ~p ", [Body]),
    R2 =
        case maps:is_key(<<"addfilter">>, Body) of
            true ->
                AddFilter = maps:get(<<"addfilter">>, Body),
                ?LOG(info, "AddFilter ~p ", [AddFilter]),
                case dgiot_parse:query_object(<<"_Role">>, AddFilter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                    {ok, #{<<"results">> := AddRoles}} ->
                        lists:foldl(fun(#{<<"objectId">> := RoleId}, Acc1) ->
                            {_, R3} =
                                dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"users">> => #{
                                    <<"__op">> => <<"AddRelation">>,
                                    <<"objects">> => [#{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"_User">>,
                                        <<"objectId">> => UserId}]}}),
                            Acc1 ++ [#{<<"add">> => R3}]
                                    end, [], AddRoles);
                    _ -> []
                end;
            Error ->
                ?LOG(info, "Error ~p ", [Error]),
                []
        end,
    {ok, #{<<"result">> => R1 ++ R2}}.



post_roleuser(#{<<"userid">> := UserId} = Body, SessionToken) ->
    R2 =
        case maps:is_key(<<"addfilter">>, Body) of
            true ->
                AddFilter = maps:get(<<"addfilter">>, Body),
                case dgiot_parse:query_object(<<"_Role">>, AddFilter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                    {ok, #{<<"results">> := AddRoles}} ->
                        lists:foldl(fun(#{<<"objectId">> := RoleId}, Acc1) ->
                            {_, R3} =
                                dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"users">> => #{
                                    <<"__op">> => <<"AddRelation">>,
                                    <<"objects">> => [#{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"_User">>,
                                        <<"objectId">> => UserId}]}}),
                            Acc1 ++ [#{<<"add">> => R3}]
                                    end, [], AddRoles);
                    _ -> []
                end;
            _ -> []
        end,
    load_roleuser(),
    {ok, #{<<"result">> => R2}}.

del_roleuser(#{<<"userid">> := UserId} = Body, SessionToken) ->
    R1 =
        case maps:find(<<"filter">>, Body) of
            error ->
                [];
            {ok, Filter} ->
                case dgiot_parse:query_object(<<"_Role">>, jsx:decode(Filter, [{labels, binary}, return_maps]), [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                    {ok, #{<<"results">> := DelRoles}} ->
                        lists:foldl(
                            fun(#{<<"objectId">> := RoleId}, Acc) ->
                                {_, R0} =
                                    dgiot_parse:update_object(<<"_Role">>, RoleId, #{<<"users">> => #{
                                        <<"__op">> => <<"RemoveRelation">>,
                                        <<"objects">> => [
                                            #{
                                                <<"__type">> => <<"Pointer">>,
                                                <<"className">> => <<"_User">>,
                                                <<"objectId">> => UserId
                                            }
                                        ]}
                                    }),
                                Acc ++ [#{<<"del">> => R0}]
                            end, [], DelRoles);
                    _ -> []
                end
        end,
    load_roleuser(),
    {ok, #{<<"result">> => R1}}.

%% 查取角色
get_role(UserId, SessionToken) ->
    get_role(?DEFAULT, UserId, SessionToken).
get_role(Name, UserId, SessionToken) ->
    Query = #{
        <<"keys">> => [<<"name">>, <<"alias">>, <<"org_type">>, <<"tag">>],
        <<"where">> => #{
            <<"users">> => #{
                <<"className">> => <<"_User">>,
                <<"objectId">> => UserId,
                <<"__type">> => <<"Pointer">>
            }
        }
    },
    case dgiot_parse:query_object(Name, <<"_Role">>, Query) of
        {ok, #{<<"results">> := RoleResults}} ->
            Roles =
                lists:foldr(
                    fun(#{<<"objectId">> := RoleId, <<"name">> := Name1, <<"alias">> := Alias, <<"org_type">> := Org_type} = X, Acc) ->
                        Role = #{<<"objectId">> => RoleId, <<"name">> => Name1, <<"alias">> => Alias, <<"org_type">> => Org_type, <<"tag">> => maps:get(<<"tag">>, X, #{})},
                        Acc#{RoleId => Role}
                    end, #{}, RoleResults),
            RoleIds =
                lists:foldr(
                    fun(#{<<"objectId">> := RoleId}, Acc) ->
                        Acc ++ [RoleId]
                    end, [], RoleResults),
            case get_rules(Name, RoleIds, SessionToken) of
                {ok, Rules} ->
                    case get_menus(Name, RoleIds, SessionToken) of
                        {ok, Menus} ->
                            Info = #{
                                <<"rules">> => Rules,
                                <<"roles">> => Roles,
                                <<"menus">> => Menus
                            },
                            {ok, Info};
                        {error, Reason1} ->
                            {error, Reason1}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% 根据角色获取API权限
get_rules(Name, RoleIds, SessionToken) ->
    Requests = [#{
        <<"method">> => <<"GET">>,
        <<"path">> => <<"/classes/Permission">>,
        <<"body">> => #{
            <<"keys">> => [<<"name">>],
            <<"where">> => #{
                <<"$relatedTo">> => #{
                    <<"key">> => <<"rules">>,
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId
                    }
                }
            }
        }
    } || RoleId <- RoleIds],
    case dgiot_parse:batch(Name, Requests, [{"X-Parse-Session-Token", binary_to_list(SessionToken)}], [{from, rest}]) of
        {ok, Results1} ->
            {ok, lists:foldr(
                fun(#{<<"success">> := #{<<"results">> := R}}, Acc1) ->
                    lists:foldr(fun(#{<<"name">> := Name1}, Acc2) ->
                        case lists:member(Name, Acc2) of
                            true ->
                                Acc2;
                            false ->
                                [Name1 | Acc2]
                        end
                                end, Acc1, R)
                end, [], Results1)};
        {error, Reason} ->
            {error, Reason}
    end.

%% 根据角色获取菜单权限
get_menus(Name, RoleIds, SessionToken) ->
    Requests = [#{
        <<"method">> => <<"GET">>,
        <<"path">> => <<"/classes/Menu">>,
        <<"body">> => #{
            <<"keys">> => [<<"name">>],
            <<"where">> => #{
                <<"$relatedTo">> => #{
                    <<"key">> => <<"menus">>,
                    <<"object">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_Role">>,
                        <<"objectId">> => RoleId
                    }
                }
            }
        }
    } || RoleId <- RoleIds],
    case dgiot_parse:batch(Name, Requests, [{"X-Parse-Session-Token", binary_to_list(SessionToken)}], [{from, rest}]) of
        {ok, Results1} ->
            {ok, lists:foldr(
                fun(#{<<"success">> := #{<<"results">> := R}}, Acc1) ->
                    lists:foldr(fun(#{<<"name">> := Name1}, Acc2) ->
                        case lists:member(Name, Acc2) of
                            true ->
                                Acc2;
                            false ->
                                [Name1 | Acc2]
                        end
                                end, Acc1, R)
                end, [], Results1)};
        {error, Reason} ->
            {error, Reason}
    end.

add_to_role(Info, Field, Class, ObjectIds) ->
    add_to_role(?DEFAULT, Info, Field, Class, ObjectIds).

add_to_role(Name, #{<<"objectId">> := RoleId} = Info, Field, Class, ObjectIds) ->
    Users = [#{
        <<"__type">> => <<"Pointer">>,
        <<"className">> => Class,
        <<"objectId">> => ObjectId
    } || ObjectId <- ObjectIds],
    case dgiot_parse:update_object(Name, <<"_Role">>, RoleId, Info#{Field => #{
        <<"__op">> => <<"AddRelation">>,
        <<"objects">> => Users
    }}) of
        {ok, #{<<"updatedAt">> := _}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
add_to_role(Name, #{<<"name">> := <<"role:", RoleName/binary>>} = Role, Field, Class, ObjectIds) ->
    add_to_role(Name, Role#{<<"name">> => RoleName}, Field, Class, ObjectIds);
add_to_role(Name, #{<<"name">> := RoleName} = Role, Field, Class, ObjectIds) ->
    case dgiot_parse:query_object(Name, <<"_Role">>, #{<<"where">> => #{<<"name">> => RoleName}}) of
        {ok, #{<<"results">> := []}} ->
            {error, #{error => <<"Role: ", RoleName/binary, " not find!">>}};
        {ok, #{<<"results">> := [#{<<"objectId">> := RoleId}]}} ->
            Info = maps:without([<<"name">>], Role),
            add_to_role(Name, Info#{<<"objectId">> => RoleId}, Field, Class, ObjectIds);
        {error, Reason} ->
            {error, Reason}
    end.

disableusere(UserId, DisUserId, Action) ->
    case find_role(UserId, DisUserId) of
        false ->
            {error, <<"do not have permission">>};
        true ->
            case Action of
                <<"enable">> ->
                    dgiot_parse:update_object(<<"_User">>, DisUserId, #{<<"emailVerified">> => true});
                <<"disable">> ->
                    dgiot_parse:update_object(<<"_User">>, DisUserId, #{<<"emailVerified">> => false})
            end
    end.

find_role(UserId, DisUserId) ->
    case UserId of
        DisUserId ->
            false;
        _ ->
            case dgiot_data:get(?USER_ROLE_ETS, UserId) of
                not_find ->
                    false;
                RoleIds ->
                    case dgiot_data:get(?USER_ROLE_ETS, DisUserId) of
                        not_find ->
                            false;
                        DisRoleIds ->
                            lists:foldl(fun(DisRoleId, _Acc) ->
                                find_parent(RoleIds, DisRoleId)
                                        end, <<>>, DisRoleIds)
                    end
            end
    end.

find_parent(RoleIds, DisRoleId) ->
    case dgiot_data:get(?ROLE_PARENT_ETS, DisRoleId) of
        not_find ->
            fasle;
        ParentId ->
            case lists:member(ParentId, RoleIds) of
                true ->
                    true;
                false ->
                    find_parent(RoleIds, ParentId)
            end
    end.

get_token(App) ->
    Now = dgiot_datetime:nowstamp(),
    case dgiot_data:get({token, App}) of
        not_find ->
            get_token_(App);
        {_Token, Expire, TTL} when Now > Expire + TTL ->
            get_token_(App);
        {Token, _Expire, _TTL} -> Token
    end.

get_token_(App) ->
    case dgiot_parse:query_object(<<"_Role">>, #{
        <<"limit">> => 1,
        <<"where">> => #{<<"name">> => App}
    }) of
        {ok, #{<<"results">> := [#{<<"objectId">> := AppId,
            <<"tag">> := #{<<"appconfig">> := #{<<"secret">> := Secret}}}]}} ->
            case login_by_token(AppId, Secret) of
                {ok, #{<<"access_token">> := Token, <<"expires_in">> := TTL}} ->
                    dgiot_data:insert({token, App}, {Token, dgiot_datetime:nowstamp(), dgiot_utils:to_int(TTL)}),
                    Token;
                Error -> Error
            end;
        Err ->
            ?LOG(error, "login by token: AppId:~p, Err ~p~n", [App, Err]),
            {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}}
    end.

%% 创建app角色用户
create_user_for_app(App) ->
    AppUser = <<"user_for_", App/binary>>,
    Query = #{
        <<"order">> => <<"-updatedAt,-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"$or">> => [#{<<"email">> => AppUser}, #{<<"phone">> => AppUser}, #{<<"username">> => AppUser}]
        }
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            {ok, AppUser};
        {ok, #{<<"results">> := []}} ->
            Data = #{
                <<"username">> => AppUser,
                <<"nick">> => AppUser,
                <<"password">> => new_token(),
                <<"phone">> => AppUser,
                <<"email">> => <<AppUser/binary, "@iotn2n.com">>
            },
            case dgiot_parse:create_object(<<"_User">>, Data) of
                {ok, #{<<"objectId">> := UserId}} ->
                    dgiot_parse:update_object(<<"_User">>, UserId, #{
                        <<"ACL">> => #{
                            UserId => #{<<"read">> => true, <<"write">> => true}
                        }}),
                    {ok, AppUser};
                Error ->
                    ?LOG(info, "Error ~p", [Error]),
                    Error
            end;
        Reason -> Reason
    end.

%% 创建企业内部用户
%%[{"X-Parse-Session-Token", Session}], [{from, rest}]
create_user(#{<<"username">> := UserName, <<"department">> := RoleId} = Body, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := RoleId}} ->
            Query = #{
                <<"order">> => <<"-updatedAt,-createdAt">>,
                <<"limit">> => 1,
                <<"where">> => #{
                    <<"$or">> => [#{<<"email">> => UserName}, #{<<"phone">> => UserName}, #{<<"username">> => UserName}]
                }
            },
            case dgiot_parse:query_object(<<"_User">>, Query) of
                {ok, #{<<"results">> := Results}} when length(Results) == 0 ->
                    case dgiot_parse:create_object(<<"_User">>, maps:without([<<"department">>], Body)) of
                        {ok, #{<<"objectId">> := UserId}} ->
                            dgiot_parse:update_object(<<"_User">>, UserId, #{
                                <<"ACL">> => #{UserId => #{
                                    <<"read">> => true,
                                    <<"write">> => true
                                }},
                                <<"emailVerified">> => true,
                                <<"roles">> => #{
                                    <<"__op">> => <<"AddRelation">>,
                                    <<"objects">> => [#{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"_Role">>,
                                        <<"objectId">> => RoleId
                                    }]}
                            }),
                            dgiot_parse:update_object(<<"_Role">>, RoleId, #{
                                <<"users">> => #{
                                    <<"__op">> => <<"AddRelation">>,
                                    <<"objects">> => [#{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"_User">>,
                                        <<"objectId">> => UserId
                                    }]
                                }}),
                            R = dgiot_parse_auth:save_User_Role(UserId, RoleId),
                            {ok, #{<<"result">> => R}};
                        Error ->
                            ?LOG(info, "Error ~p", [Error]),
                            {error, Error}
                    end;
                {ok, #{<<"results">> := [_Info]}} ->
                    ?LOG(info, "_Info ~p", [_Info]),
                    {error, #{<<"result">> => <<"User exist  ", UserName/binary>>}};
                {error, Error} ->
                    ?LOG(info, "Error ~p", [Error]),
                    {error, Error}
            end;
        _ -> {error, <<"token fail">>}
    end.

%% 删除企业内部用户
%%[{"X-Parse-Session-Token", Session}], [{from, rest}]
delete_user(#{<<"username">> := UserName, <<"department">> := RoleId}, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := RoleId}} ->
            Query = #{
                <<"where">> => #{
                    <<"$relatedTo">> => #{
                        <<"object">> => #{
                            <<"__type">> => <<"Pointer">>,
                            <<"className">> => <<"_Role">>,
                            <<"objectId">> => RoleId},
                        <<"key">> => <<"users">>
                    }
                }
            },
            case dgiot_parse:query_object(<<"_User">>, Query) of
                {ok, #{<<"results">> := Resulst}} when length(Resulst) > 0 ->
                    R = lists:map(fun(#{<<"username">> := Name, <<"objectId">> := ObjectId}) ->
                        case Name of
                            UserName ->
                                dgiot_parse:del_object(<<"_User">>, ObjectId),
                                dgiot_parse_auth:del_User_Role(ObjectId, RoleId);
                            _ -> pass
                        end
                                  end, Resulst),
                    {ok, #{<<"result">> => R}};
                _ -> {error, <<"not find user">>}
            end;
        _ -> {error, <<"token fail">>}
    end.


%% 修改企业内部用户
%%[{"X-Parse-Session-Token", Session}], [{from, rest}]
put_user(#{<<"username">> := UserName, <<"department">> := RoleId} = Body, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := RoleId}} ->
            Query = #{
                <<"where">> => #{
                    <<"$relatedTo">> => #{
                        <<"object">> => #{
                            <<"__type">> => <<"Pointer">>,
                            <<"className">> => <<"_Role">>,
                            <<"objectId">> => RoleId},
                        <<"key">> => <<"users">>
                    }
                }
            },
            case dgiot_parse:query_object(<<"_User">>, Query) of
                {ok, #{<<"results">> := Resulst}} when length(Resulst) > 0 ->
                    R = lists:foldl(fun(#{<<"username">> := Name, <<"objectId">> := ObjectId}, Acc) ->
                        case Name of
                            UserName ->
                                {ok, Result} = dgiot_parse:update_object(<<"_User">>, ObjectId,
                                    maps:without([<<"department">>], Body)),
                                dgiot_parse_auth:put_User_Role(ObjectId, RoleId, RoleId),
                                Result;
                            _ -> Acc
                        end
                                    end, #{}, Resulst),
                    ?LOG(info, "R ~p", [R]),
                    {ok, #{<<"result">> => R}};
                _ -> {error, <<"not find user">>}
            end;
        _ -> {error, <<"token fail">>}
    end.

%% 用户名和密码登录
login_by_account(UserName, Password) ->
    case dgiot_parse_auth:login(UserName, Password) of
        {ok, #{<<"objectId">> := UserId} = _UserInfo} ->
            create_session(UserId, dgiot_auth:ttl(), UserName);
%%            do_login(UserInfo);
        {error, Msg} ->
            {error, Msg}
    end.


%%[{"X-Parse-Session-Token", Session}], [{from, rest}])
login_by_token(AppId, Secret) ->
    case dgiot_parse:get_object(<<"_Role">>, AppId) of
        {ok, #{<<"name">> := Name, <<"tag">> := #{<<"appconfig">> := Config}}} ->
            case maps:find(<<"secret">>, Config) of
                error ->
                    {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}};
                {ok, Secret} ->
                    AppUser = <<"user_for_", Name/binary>>,
                    Query = #{
                        <<"order">> => <<"-updatedAt,-createdAt">>,
                        <<"limit">> => 1,
                        <<"where">> => #{
                            <<"$or">> => [#{<<"email">> => AppUser}, #{<<"phone">> => AppUser}, #{<<"username">> => AppUser}]
                        }
                    },
                    case dgiot_parse:query_object(<<"_User">>, Query) of
                        {ok, #{<<"results">> := [#{<<"objectId">> := UserId} | _]}} ->
                            TTL = maps:get(<<"expires">>, Config, dgiot_auth:ttl()),
                            create_session(UserId, TTL, Name);
                        _ -> {error, #{<<"code">> => 101, <<"error">> => <<"User not found.">>}}
                    end;
                _ ->
                    {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}}
            end;
        Err ->
            ?LOG(error, "login by token: AppId:~p, Secret:~p ~p~n", [AppId, Secret, Err]),
            {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}}
    end.

%% 邮箱/手机验证后的登录
login_by_mail_phone(Account) ->
    Query = #{
        <<"order">> => <<"-updatedAt,-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"$or">> => [#{<<"email">> => Account}, #{<<"phone">> => Account}]
        }
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := [Info]}} ->
            ?LOG(info, "Info ~p", [Info]),
            #{<<"objectId">> := UserId, <<"username">> := Desc} = Info,
            Data = #{<<"emailVerified">> => true, <<"phone">> => Account},
            case dgiot_parse:update_object(<<"_User">>, UserId, Data) of
                {ok, #{<<"updatedAt">> := _}} ->
                    create_session(UserId, dgiot_auth:ttl(), Desc);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, #{<<"results">> := []}} ->
            {error, #{<<"code">> => 101, <<"error">> => <<"User not found by ", Account/binary>>}};
        {error, Reason} ->
            {error, Reason}
    end.


set_cookies(Key, Value, Req) ->
    dgiot_req:set_cookie(Key, Value, Req, #{max_age => dgiot_auth:ttl()}).

check_roles(Name) ->
    Query = #{
        <<"order">> => <<"-updatedAt,-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{<<"username">> => <<"user_for_", Name/binary>>}
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := [#{<<"objectId">> := UserId} | _]}} ->
            {ok, #{<<"access_token">> := Access_Token}} =
                create_session(UserId, dgiot_auth:ttl(), Name),
            {200, #{
                <<"desc">> => Name,
                <<"expires_in">> => dgiot_auth:ttl(),
                <<"access_token">> => Access_Token}};
        _ -> {404, #{<<"error">> => <<"User not found.">>}}
    end.

%% 随机产生token
new_token() ->
    GUID = dgiot_utils:guid(),
    dgiot_utils:to_md5(GUID).

do_login(UserInfo) ->
    do_login(UserInfo, dgiot_auth:ttl()).


do_login(#{<<"objectId">> := UserId, <<"sessionToken">> := SessionToken} = UserInfo, TTL) ->
    case catch dgiot_parse_auth:get_role(UserId, SessionToken) of
        {ok, #{<<"roles">> := Roles, <<"rules">> := Rules, <<"menus">> := Menus}} ->
            NewRules =
                lists:foldl(
                    fun(Rule, Acc) ->
                        [Rule | lists:delete(Rule, Acc)]
                    end, Rules, [<<"GET_CLASSES_NAVIGATION">>, <<"GET_USERS_ME">>]),
            dgiot_auth:put_session(UserInfo#{<<"roles">> => Roles, <<"rules">> => NewRules, <<"menus">> => Menus}, TTL),
            {ok, maps:without([<<"_account_lockout_expires_at">>, <<"_failed_login_count">>, <<"_email_verify_token">>], UserInfo#{
                <<"rules">> => Rules,
                <<"menus">> => Menus,
                <<"roles">> => maps:values(Roles)})};
        {error, ErrMsg} ->
            {error, ErrMsg};
        {'EXIT', Reason} ->
            {error, #{<<"code">> => 1, <<"error">> => dgiot_utils:format("~p", [Reason])}};
        _Other ->
            ?LOG(info, "_Other ~p", [_Other])
    end.


add_acl(_, _, Records, _NewItems, Acc) when Records == [] ->
    Acc;
add_acl(Class, RoleName, [#{<<"objectId">> := Id, <<"ACL">> := ACLMap} | Records], NewItems, Acc) ->
    Path = <<"/classes/", Class/binary, "/", Id/binary>>,
    case maps:get(RoleName, ACLMap, no) of
        no ->
            case get_value(Id, NewItems) of
                false ->
                    add_acl(Class, RoleName, Records, NewItems, Acc);
                Id ->
                    NewAcl = #{<<"ACL">> => ACLMap#{RoleName => #{<<"read">> => true, <<"write">> => true}}},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), NewAcc);
                ACL ->
                    NewAcl = #{<<"ACL">> => ACLMap#{RoleName => ACL}},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), NewAcc)
            end;
        OldACL ->
            case get_value(Id, NewItems) of
                false ->
                    NewAcl = #{<<"ACL">> => maps:without([RoleName], ACLMap)},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, NewItems, NewAcc);
                O when O == Id; O == OldACL ->
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), Acc);
                ACL ->
                    NewAcl = #{<<"ACL">> => ACLMap#{RoleName => ACL}},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), NewAcc)
            end
    end.


get_value(_, []) -> false;
get_value(Key, [{Key, Value}]) -> Value;
get_value(Key, [Key | _]) -> Key;
get_value(Key, [_ | L]) -> get_value(Key, L).