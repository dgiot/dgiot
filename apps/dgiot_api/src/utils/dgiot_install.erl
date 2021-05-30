%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 五月 2019 20:37
%%%-------------------------------------------------------------------
-module(dgiot_install).
-author("kenneth").
-include("dgiot_api.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([start/1, start_link/0, get_install_cfg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    init_database/1,
    generate_root/1,
    generate_admin/1,
    generate_role/2,
    generate_role/1,
    generate_approle/1,
    generate_users/1,
    generate_menus/1,
    generate_rule/1,
    get_roletemp/1,
    save_tables/1,
    save_menu/3]).

-define(SERVER, ?MODULE).

-record(state, {result}).

start(#{product := <<"rule">>, webserver := ServerName}) ->
    case get_install_cfg(<<"rule">>) of
        {error, Reason} ->
            {error, Reason};
        {Actions, Path} ->
            R = lists:foldl(
                fun(Mod, Result) ->
                    case Mod of
                        {M, F} -> apply(M, F, [Result]);
                        Fun -> apply(?MODULE, Fun, [Result])
                    end
                end, [{<<"dir">>, Path}, {<<"webname">>, ServerName}], Actions),
            R
    end;

start(#{product := Product, webserver := ServerName}) ->
    Root = get_lock_path(),
    LockFile = filename:join([Root, <<Product/binary, "_", "install.lock">>]),
    case file:read_file(LockFile) of
        {ok, _} ->
            {error, <<"You have installed, please delete ", LockFile/binary>>};
        _ when Product == <<"clean">> ->
            clean_database([<<"*">>]),
            filelib:ensure_dir(LockFile),
            ok = file:write_file(LockFile, <<Product/binary, " install OK.">>),
            #{<<"result">> => <<"SUCCESS">>};
        _ ->
            case get_install_cfg(Product) of
                {error, Reason} ->
                    {error, Reason};
                {Actions, Path} ->
                    R = lists:foldl(
                        fun(Mod, Result) ->
                            case Mod of
                                {M, F} -> apply(M, F, [Result]);
                                Fun -> apply(?MODULE, Fun, [Result])
                            end
                        end, [{<<"dir">>, Path}, {<<"webname">>, ServerName}], Actions),
                    filelib:ensure_dir(LockFile),
                    ok = file:write_file(LockFile, <<Product/binary, " install OK.">>),
                    R
            end
    end.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({setup, Fun, Result}, _From, State) ->
    case catch apply(?MODULE, Fun, [Result]) of
        {'EXIT', Reason} ->
            {reply, {error, Reason}, State};
        Result ->
            {reply, {ok, Result}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% 初始化函数
%%%===================================================================

get_lock_path() ->
    case os:type() of
        {unix, _} -> <<"/etc/dgiot">>;
        {win32, _} -> <<"c:\\dgiot">>
    end.

%% 清库
clean_database(List) ->
    NotDelTabs = application:get_env(dgiot_web_manager, not_delete_table, []),
    case dgiot_parse:get_schemas() of
        {ok, #{<<"results">> := Tables}} ->
            lists:foreach(
                fun(#{<<"className">> := Table}) ->
                    case lists:member(binary_to_list(Table), NotDelTabs) of
                        false ->
                            case List == [<<"*">>] orelse lists:member(Table, List) of
                                true ->
                                    dgiot_parse:del_trigger(Table),
                                    case dgiot_parse:del_table(Table) of
                                        ok ->
                                            case dgiot_parse:del_schemas(Table) of
                                                ok ->
                                                    ok;
                                                {error, Reason} ->
                                                    throw({error, Reason})
                                            end;
                                        {error, Reason} ->
                                            throw({error, Reason})
                                    end;
                                false ->
                                    ok
                            end;
                        true ->
                            ok
                    end
                end, Tables);
        {error, Reason} ->
            throw({error, Reason})
    end.


%% 初始化库
init_database(Result) ->
    Dir = proplists:get_value(<<"dir">>, Result),
    dgiot_parse:init_database([binary_to_list(Dir)], merge),
    #{name := ServerName} = proplists:get_value(<<"webname">>, Result),
    dgiot_http_server:reload_paths(?APP, ServerName),
    Result.

generate_root(Result) ->
    Dir = proplists:get_value(<<"dir">>, Result),
    [Root] = load_config(Dir, "root"),
    NewRoot = Root#{
        <<"depname">> => <<"root">>,
        <<"org_type">> =>  <<"SW">>,
        <<"order">> =>  0,
        <<"level">> =>  0,
        <<"leafnode">> => false
    },
    generate_role([NewRoot], Result).


generate_admin(Result) ->
    Dir = proplists:get_value(<<"dir">>, Result),
    [Admin] = load_config(Dir, "admin"),
    NewAdmin = Admin#{
        <<"depname">> => <<"admin">>,
        <<"org_type">> =>  <<"SW">>,
        <<"order">> =>  1,
        <<"level">> =>  1,
        <<"leafnode">> => false
    },
    generate_role([NewAdmin], Result).

%% 产生角色
generate_role(Result) ->
    ?LOG(info,"Result ~p", [Result]),
    Dir = proplists:get_value(<<"dir">>, Result),
    AppName = proplists:get_value(<<"appname">>, Result, <<"developer">>),
    AppId = proplists:get_value(<<"appid">>, Result, <<"developer">>),
    Desc = proplists:get_value(<<"desc">>, Result, <<"开发者"/utf8>>),
    UserName = proplists:get_value(<<"username">>, Result, <<"test">>),
    [Role] = load_config(Dir, "role"),
    NewRoles = Role#{
        <<"name">> => AppName,
        <<"alias">> => AppId,
        <<"desc">> => <<Desc/binary, ""/utf8>>,
        <<"ACL">> => #{
            <<"role:", AppName/binary>> => #{<<"read">> => true}
        },
        <<"users">> => [UserName],
        <<"depname">> => AppName,
        <<"org_type">> =>  <<"SW">>,
        <<"order">> =>  2,
        <<"level">> =>  2,
        <<"leafnode">> => true
    },
    generate_role([NewRoles], Result).

get_roletemp(Role) ->
    Role1 =
        case maps:get(<<"menus">>, Role, []) of
            [] -> Role;
            _ -> Role#{<<"menus">> => dgiot_role:get_menus_role(maps:get(<<"menus">>, Role, []))}
        end,
    Role2 =
        case maps:get(<<"roles">>, Role, []) of
            [] -> Role1;
            _ -> Role1#{
                <<"roles">> =>  dgiot_role:get_roles_role(maps:get(<<"roles">>, Role1, [])),
                <<"parent">> => dgiot_role:get_parent_role(maps:get(<<"roles">>, Role, []))
            }
        end,
    case maps:get(<<"rules">>, Role, []) of
        [] -> Role2;
        _ -> Role2#{<<"rules">> => dgiot_role:get_rules_role(maps:get(<<"rules">>, Role, []))}
    end.

generate_role(Roles, Result) ->
    Fun =
        fun(#{<<"name">> := Name} = Role) ->
            {ok, AppUser} = dgiot_parse_handler:create_user_for_app(Name),
            ?LOG(info,"AppUser ~p ", [AppUser]),
            NewUsers = maps:get(<<"users">>, Role, []) ++ [AppUser],
            NewRole = Role#{
                <<"users">> => dgiot_role:get_users_role(NewUsers),
                <<"menus">> => dgiot_role:get_menus_role(maps:get(<<"menus">>, Role, [])),
                <<"roles">> => dgiot_role:get_roles_role(maps:get(<<"roles">>, Role, [])),
                <<"parent">> => dgiot_role:get_parent_role(maps:get(<<"roles">>, Role, [])),
                <<"rules">> => dgiot_role:get_rules_role(maps:get(<<"rules">>, Role, []))
            },
            case dgiot_parse:query_object(<<"_Role">>, #{<<"where">> => #{<<"name">> => Name}}) of
                {ok, #{<<"results">> := R}} when length(R) == 0 ->
                    case dgiot_parse:create_object(<<"_Role">>, NewRole) of
                        {ok, [#{<<"objectId">> := RoleId1}]} ->
                            {<<"objectId">>, RoleId1};
                        {ok, #{<<"objectId">> := RoleId2}} ->
                            {<<"objectId">>, RoleId2};
                        {error, Reason} ->
                            throw({error, Reason})
                    end;
                {ok, #{<<"results">> := [#{<<"objectId">> := RoleId}]}} ->
                    dgiot_role:remove_menus_role(RoleId),
                    dgiot_role:remove_roles_role(RoleId),
                    dgiot_role:remove_rules_role(RoleId),
                    dgiot_role:remove_users_roles(RoleId),
                    case dgiot_parse:update_object(<<"_Role">>, RoleId, NewRole) of
                        {ok, _R2} ->
                            {<<"objectId">>, RoleId};
                        {error, Reason} ->
                            throw({error, Reason})
                    end;
                {error, Reason} ->
                    throw({error, Reason})
            end
        end,
    NewResult = lists:map(Fun, Roles) ++ Result,
    ?LOG(info,"NewResult ~p", [NewResult]),
    [{<<"roles">>, Roles} | NewResult].

%% 产生用户
generate_users(Result) ->
    Dir = proplists:get_value(<<"dir">>, Result),
    Users = load_config(Dir, "user"),
    UpdateAclFun =
        fun(_User, UserId, UserName, Acc) ->
            Acl = #{
                UserId => #{<<"read">> => true, <<"write">> => true}
            },
            case dgiot_parse:update_object(<<"_User">>, UserId, #{<<"ACL">> => Acl, <<"emailVerified">> => true}) of
                {ok, #{<<"updatedAt">> := _}} ->
                    Acc#{UserName => UserId};
                Err ->
                    throw(Err)
            end
        end,
    UsersMap =
        lists:foldl(
            fun(#{<<"username">> := UserName} = User, Acc) ->
                case dgiot_parse:query_object(<<"_User">>, #{<<"where">> => #{<<"username">> => UserName}}) of
                    {ok, #{<<"results">> := []}} ->
                        case dgiot_parse:create_object(<<"_User">>, maps:without([<<"roles">>], User)) of
                            {ok, #{<<"objectId">> := UserId}} ->
                                UpdateAclFun(User, UserId, UserName, Acc);
                            Err ->
                                throw(Err)
                        end;
                    {ok, #{<<"results">> := [#{<<"objectId">> := UserId}]}} ->
                        UpdateAclFun(User, UserId, UserName, Acc);
                    Err ->
                        throw(Err)
                end
            end, #{}, Users),
    [{<<"users">>, UsersMap} | Result].


%% 权限入库
generate_rule(Result) ->
    ?LOG(info,"Result ~p",[Result]),
    #{name := ServerName} = proplists:get_value(<<"webname">>, Result,  #{name => dgiot_rest}),
    ?LOG(info,"ServerName ~p",[ServerName]),
    Rules =
        case dgiot_swagger:read(ServerName, #{}) of
            {ok, Schema} ->
                BaseSecurity = maps:get(<<"security">>, Schema, []),
                Tags =
                    lists:foldl(
                        fun(#{<<"name">> := Name, <<"description">> := Desc}, Acc) ->
                            Acc#{Name => Desc}
                        end, #{}, maps:get(<<"tags">>, Schema, [])),
                Paths = maps:get(<<"paths">>, Schema, #{}),
                maps:fold(
                    fun(_Path, Methods, Acc) ->
                        maps:fold(
                            fun(_Method, #{<<"operationId">> := OperationId} = Info, Acc1) ->
                                case maps:get(<<"security">>, Info, BaseSecurity) of
                                    [] ->
                                        Acc1;
                                    _ ->
                                        RuleName = list_to_binary(string:to_upper(binary_to_list(OperationId))),
                                        R = case maps:get(<<"tags">>, Info, []) of
                                                [] ->
                                                    save_rule(RuleName, <<"0">>, Info);
                                                [Tag | _] ->
                                                    PId = re:replace(Tag, <<"_">>, <<>>, [{return, binary}]),
                                                    PName = maps:get(Tag, Tags, unicode:characters_to_binary(<<PId/binary, <<" Manager"/utf8>>/binary>>)),
                                                    PRuleName = <<PId/binary, "_ALL">>,
                                                    Parent = #{
                                                        <<"summary">> =>  PName,
                                                        <<"description">> => PName,
                                                        <<"tags">> => [PId]
                                                    },
                                                    case save_rule(PRuleName, <<"0">>, Parent) of
                                                        {ok, _PName, PRuleId} ->
                                                            save_rule(RuleName, PRuleId, Info);
                                                        {error, Why} ->
                                                            {error, Why}
                                                    end
                                            end,
                                        case R of
                                            {ok, Name, RuleId} ->
                                                Acc1#{Name => RuleId};
                                            {error, Reason} ->
                                                throw({error, Reason})
                                        end
                                end
                            end, Acc, Methods)
                    end, #{}, Paths);
            {error, Reason} ->
                throw({error, Reason})
        end,
    [{<<"rules">>, Rules} | Result].


%% 加菜单
generate_menus(Result) ->
    Dir = proplists:get_value(<<"dir">>, Result),
    Menus = save_menu(<<"0">>, load_config(Dir, "menu"), #{}),
    [{<<"menus">>, Menus} | Result].

save_menu(_, [], Acc) -> Acc;
save_menu(Parent, [Menu0 | Menus], Acc) ->
    Menu = maps:without([<<"show">>], Menu0),
    Url = maps:get(<<"url">>, Menu, <<"#">>),
    Name = maps:get(<<"name">>, Menu),
    Children = maps:get(<<"children">>, Menu, []),
    Where = #{
        <<"url">> => Url,
        <<"name">> => Name
    },
    ?LOG(info,"Where ~p", [Where]),
    NewMenu = maps:without([<<"children">>], Menu#{
        <<"url">> => Url,
        <<"ACL">> => #{
            <<"*">> => #{<<"read">> => true}
        },
        <<"parent">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"Menu">>,
            <<"objectId">> => Parent
        }
    }),
    case dgiot_parse:query_object(<<"Menu">>, #{<<"where">> => Where}) of
        {ok, #{<<"results">> := []}} ->
            case dgiot_parse:create_object(<<"Menu">>, NewMenu) of
                {ok, #{<<"objectId">> := MenuId}} ->
                    NewAcc = save_menu(MenuId, Children, Acc#{Name => MenuId}),
                    save_menu(Parent, Menus, NewAcc);
                {error, Reason} ->
                    throw({error, Reason})
            end;
        {ok, #{<<"results">> := [#{<<"objectId">> := MenuId}]}} ->
            case dgiot_parse:update_object(<<"Menu">>, MenuId, NewMenu) of
                {ok, #{<<"updatedAt">> := _}} ->
                    NewAcc = save_menu(MenuId, Children, Acc#{Name => MenuId}),
                    save_menu(Parent, Menus, NewAcc);
                {error, Reason} ->
                    throw({error, Reason})
            end;
        {error, Reason} ->
            throw({error, Reason})
    end.

save_rule(Name, Parent, Info) ->
    #{
        <<"summary">> := Summary,
        <<"description">> := Description,
        <<"tags">> := Tags
    } = Info,
    Rule = #{
        <<"name">> => Name,
        <<"alias">> => Summary,
        <<"tags">> => Tags,
        <<"description">> => Description
    },
    Query = #{<<"where">> => #{<<"name">> => Name}},
    case dgiot_parse:query_object(<<"Permission">>, Query) of
        {ok, #{<<"results">> := []}} ->
            RuleInfo = Rule#{
                <<"parent">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Permission">>,
                    <<"objectId">> => Parent
                },
                <<"ACL">> => #{
                    <<"*">> => #{<<"read">> => true}
                }
            },
            case dgiot_parse:create_object(<<"Permission">>, RuleInfo) of
                {ok, #{<<"objectId">> := ObjectId}} ->
                    {ok, Name, ObjectId};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId}]}} ->
            RuleInfo = maps:without([<<"alias">>, <<"description">>], Rule),
            case dgiot_parse:update_object(<<"Permission">>, ObjectId, RuleInfo) of
                {ok, #{<<"updatedAt">> := _UpdateAt}} ->
                    {ok, Name, ObjectId};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


save_tables(Result) ->
    Dir = proplists:get_value(<<"dir">>, Result),
    #{<<"tables">> := Tables} = load_config(Dir, "config"),
    maps:fold(
        fun(ClassName, Files, NewRes) ->
            Count =
                lists:foldl(
                    fun(File, Acc) ->
                        Path = filename:join([Dir, File]),
                        Callback =
                            fun(Results, Count) ->
                                Count + length(Results)
                            end,
                        case dgiot_parse:import(ClassName, {json, Path}, 1000, Callback, Acc) of
                            {error, Reason} ->
                                throw({error, Reason});
                            Other ->
                                Other
                        end
                    end, 0, Files),
            [{ClassName, Count} | NewRes]
        end, Result, Tables).

%% 产生应用角色
generate_approle(Result) ->
    Dir = maps:get(<<"dir">>, Result, <<"role">>),
    AppId = maps:get(<<"appid">>, Result, <<"developer">>),
    AppName = maps:get(<<"name">>, Result, <<"开发者"/utf8>>),
    Desc = maps:get(<<"desc">>, Result, <<"开发者"/utf8>>),
    UserName = maps:get(<<"username">>, Result, <<"test">>),
    [Role] = load_config(Dir, "role"),
    RoleName = <<"role:", AppName/binary>>,
    NewRoles = Role#{
        <<"name">> => AppName,
        <<"alias">> => AppId,
        <<"desc">> => <<Desc/binary, ""/utf8>>,
        <<"ACL">> => #{
            RoleName => #{<<"read">> => true, <<"write">> => true}
        },
        <<"users">> => [UserName],
        <<"depname">> => AppName,
        <<"org_type">> =>  <<"SW">>,
        <<"order">> =>  2,
        <<"level">> =>  2,
        <<"leafnode">> => true
    },
    generate_role([NewRoles], Result).

load_config(Dir, Name) when is_binary(Dir) ->
    load_config(binary_to_list(Dir), Name);
load_config(Dir, Name) ->
    case filename:extension(Name) of
        [] ->
            {ok, Bin} = file:read_file(Dir ++ Name ++ ".json"),
            jsx:decode(Bin, [{labels, binary}, return_maps]);
        _ ->
            {ok, Bin} = file:read_file(Dir ++ Name),
            jsx:decode(Bin, [{labels, binary}, return_maps])
    end.


get_install_cfg(Product) when is_binary(Product) ->
    get_install_cfg(binary_to_list(Product));
get_install_cfg(Product) ->
    Mod = list_to_atom("dgiot_" ++ Product ++ "_install"),
    case erlang:function_exported(Mod, actions, 0) of
        true ->
            case Mod:actions() of
                {Actions, Path} ->
                    {Actions, Path};
                Actions ->
                    {file, Here} = code:is_loaded(Mod),
                    Dir = filename:dirname(filename:dirname(Here)),
                    {Actions, list_to_binary(Dir ++ "/priv/install/" ++ Product ++ "/")}
            end;
        false ->
            {error, <<"Not Find Install Script.">>}
    end.
