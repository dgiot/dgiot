%% dgiot_role v2 — Precomputed role tree
%% Fix #2: childrole() does recursive dgiot_data:get, now precomputed at startup

-define(ROLE_TREE_ETS, dgiot_role_tree_cache).  %% RoleId -> [AllChildRoleIds]

%% ——— 启动时预计算 ———
init_role_tree() ->
    ets:new(?ROLE_TREE_ETS, [named_table, public, {keypos, 1}]),
    Roles = dgiot_parse:query_object(<<"_Role">>, #{}, [], [{from, master}]),  %% 不加过滤:全部角色
    case Roles of
        {ok, #{<<"results">> := RoleList}} ->
            lists:foreach(fun(Role) ->
                #{<<"objectId">> := RoleId} = Role,
                AllChildren = compute_children(RoleId, []),
                ets:insert(?ROLE_TREE_ETS, {RoleId, AllChildren})
            end, RoleList);
        _ -> ok
    end.

compute_children(RoleId, Acc) ->
    {ok, #{<<"results">> := Children}} =
        dgiot_parse:query_object(<<"_Join:roles:_Role">>,
            #{<<"owningId">> => #{<<"objectId">> => RoleId}},  %% 查此角色的所有子角色
            [], [{from, master}]),
    NewAcc = Acc ++ [maps:get(<<"relatedId">>, C) || C <- Children],
    lists:foldl(fun(ChildId, A) -> compute_children(ChildId, A) end, NewAcc, Children).

%% ——— childrole v2: O(1) ETS lookup ———
childrole(RoleIds) ->
    lists:foldl(fun(RoleId, Acc) ->
        case ets:lookup(?ROLE_TREE_ETS, RoleId) of
            [{RoleId, Children}] -> Acc ++ Children;
            [] -> Acc
        end
    end, RoleIds, RoleIds).  %% 包含自身

%% ——— 角色变更时刷新 (增量) ———
refresh_role(RoleId) ->
    Children = compute_children(RoleId, []),
    ets:insert(?ROLE_TREE_ETS, {RoleId, Children}),
    ok.
