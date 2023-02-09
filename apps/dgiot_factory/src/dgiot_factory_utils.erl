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
-export([get_usertree/2, getalluser/1, clear_cache/2]).
-export([get_zero_list/1, get_zero_binary/1]).
-export([get_worker/1, get_children/1, check_workteam/1]).
-export([get_sum/1]).
-export([batch_create_worker/3]).
-export([get_json_file/1]).




get_usertree(#{<<"id">> := undefined}, SessionToken) ->
    case get_same_level_role(SessionToken) of
        RoleTree when length(RoleTree) > 0 ->
            get_children(RoleTree);
        _ ->
            []
    end;
get_usertree(#{<<"id">> := Id}, _) ->
    Tree = get_worker(Id),
    {NewTree, List} = format_tree(Tree),
    record_workteam(List),
    NewTree.



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
    case dgiot_parse:query_object(<<"_Role">>, #{<<"where">> => #{<<"objectId">> => #{<<"$in">> => ChildroleIds}}}) of
        {ok, #{<<"results">> := RoleList}} ->
            get_children(dgiot_parse_utils:create_tree(RoleList, <<"parent">>));
        _ ->
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
                #{<<"objectId">> := Id, <<"nick">> := Nick, <<"username">> := UserName} ->
                    update_worker_ets(Id, UserName, Nick, Depname),
%%                    Acc ++ [#{<<"label">> => Nick, <<"value">> => <<UserName/binary, "_", Nick/binary>>}];
                    Acc ++ [#{<<"label">> => <<UserName/binary, "_", Nick/binary>>, <<"value">> => UserName}];
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





%%dgiot_data:insert(?WORKER, Id, {UserName, Nick,1}),
update_worker_ets(Id, UserName, Nick, Depname) ->
    case dgiot_data:lookup(?WORKER, Id) of
        {ok, {_, _, State}} ->
            dgiot_data:insert(?WORKER, UserName, {Id, Depname, Nick, State});
        {error, not_find} ->
            dgiot_data:insert(?WORKER, UserName, {Id, Depname, Nick, 1})
    end.

record_workteam(List) ->
    WorkTeamMember = lists:foldl(
        fun(X, Acc) ->
            Team = lists:nth(1, maps:keys(X)),
            OldList = maps:get(Team, Acc, []),
            WorkerList = lists:flatten(maps:get(Team, X, [])),
            lists:foldl(
                fun(Worker, Acc1) ->
                    case dgiot_data:get(?WORKER, Worker) of
                        not_find ->
                            Acc1 ++ [Worker];
                        {Id, _, Name, State} ->
                            dgiot_data:insert(?WORKER, Worker, {Id, Team, Name, State}),
                            Acc1 ++ [Worker];
                        _ ->
                            Acc1 ++ [Worker]

                    end
                end, [], WorkerList),
            maps:merge(Acc, #{Team => OldList ++ WorkerList})
        end, #{}, List),
    dgiot_data:insert(?WORKER, workteam, WorkTeamMember).

check_workteam(Worker) ->
    WorkerList = lists:delete(<<>>, re:split(Worker, <<" ">>)),
    TeamMap = case dgiot_data:get(?WORKER, workteam) of
                  not_find ->
                      #{};
                  L ->
                      L
              end,
    TeamList = maps:keys(TeamMap),
    Res = lists:foldl(
        fun(Shift, Acc) ->
            case lists:member(Shift, TeamList) of
                true ->
                    Workers = maps:get(Shift, TeamMap, <<"">>),
                    BinWorkers = turn_workes2binary(Workers),
                    <<Acc/binary, BinWorkers/binary>>;
                _ ->
                    <<Acc/binary, " ", Shift/binary>>
            end
        end, <<"">>, WorkerList),
    case Res of
        <<" ", R/binary>> ->
            R;
        _ ->
            <<"">>

    end.

turn_workes2binary(Workers) ->
    FlatternList = lists:flatten(Workers),
    lists:foldl(
        fun(X, Acc) ->
            Bin = dgiot_utils:to_binary(X),
            <<Acc/binary, " ", Bin/binary>>
        end, <<"">>, FlatternList).

format_tree(Tree) ->
    lists:foldl(
        fun(Team, {NewTree, List}) ->
            case Team of
                #{<<"name">> := Value, <<"children">> := Child} ->
                    {AllChild, ChildList} = get_all_worker(Child),
                    {NewTree ++ [#{<<"label">> => Value, <<"value">> => Value, <<"children">> => AllChild}], List ++ [#{Value => ChildList}]};
                #{<<"name">> := Lable, <<"value">> := Value} ->
                    {NewTree ++ [#{<<"label">> => Lable, <<"value">> => Value}], List ++ [Value]};
                _ ->
                    {NewTree, List}
            end
        end, {[], []}, Tree).

get_all_worker(Child) when is_list(Child) ->
    lists:foldl(
        fun(X, {NewTree, List}) ->
            case X of
                #{<<"children">> := SubChild} ->
                    {SubTree, SubList} = get_all_worker(SubChild),
                    {NewTree ++ SubTree, List ++ [SubList]};
                #{<<"label">> := Lable, <<"value">> := Value} ->
                    {NewTree ++ [#{<<"label">> => Lable, <<"value">> => Value}], List ++ [Value]};
                _ ->
                    {NewTree, List}
            end
        end, {[], []}, Child);

get_all_worker(Child) ->
    io:format("~s ~p here ~n", [?FILE, ?LINE]),
    {Child, []}.
clear_cache(BatchProduct, BatchDeviceId) ->
    DeviceTypeList = case dgiot_product:get_devicetype(BatchDeviceId) of
                         not_find ->
                             [];
                         L ->
                             L
                     end,
    lists:foldl(
        fun(Type, _) ->
            dgiot_data:delete(?FACTORY_ORDER, {BatchProduct, BatchDeviceId, Type})
        end, [], DeviceTypeList).

get_sum(BatchList) when is_list(BatchList) ->
    lists:foldl(
        fun
            (#{<<"PickNum">> := PickNum}, Acc) ->
                Acc + dgiot_utils:to_int(PickNum);
            (_, Acc) ->
                Acc
    end,0,BatchList);

get_sum(_) ->
    0.
%%InitNum = 10001,ProductId = <<"5cd9ae6a63">>,WorkerList = re:split(<<"朱文波,周海明,祁新军,王信红,胡大芳,陈卫红,吴正碧,王虎,王东华,陈绿娟,冷群龙,邹白云,金爱莲,陈继强,赵洪彬,张洪平,李国苹,曾天权,潘宗文,张珍群,胡剑波,张晓兰"/utf8>>,<<",">>).
%%dgiot_parse:update_object(<<"Product">>, ProductId,  #{<<"ACL">> => #{<<"*">> => #{<<"read">> => true}, <<"role:生产计划部门"/utf8>> => #{<<"read">> => true, <<"write">> => true}}})
batch_create_worker(ProductId, WorkerList, InitNum) ->
    lists:foldl(
        fun(Worker, Num) ->
            dgiot_factory_channel:init_worker_device(ProductId, Num, Worker),
            Num + 1
        end, InitNum, WorkerList).


get_json_file(FileName) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/json/"]),
    Name = dgiot_utils:to_list(FileName),
    NewName =
        case filename:extension(Name) of
            [] ->
                Name ++ ".json";
            _ ->
                Name
        end,
    Path = Dir ++ NewName,
    case catch file:read_file(Path) of
        {Err, _Reason} when Err == 'EXIT'; Err == error ->
%%                        ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
            #{};
        {ok, Bin} ->
%%                        jsx:decode(Bin, [{labels, binary}, return_maps])
            jsx:decode(Bin)
    end.
