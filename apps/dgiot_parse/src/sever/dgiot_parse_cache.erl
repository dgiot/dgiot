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

-module(dgiot_parse_cache).
-author("johnliu").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_data("ets").
-export([test/0, init_ets/0, cache_classes/1, add_count/1, lookup_count/1, lookup_count/2, get_count/2, get_count/3, loop_count/1, get_roleids/1, get_alcname/1, get_acls/1]).
-export([do_save/1, save_to_cache/1, save_to_cache/2, save_test/1]).

init_ets() ->
    dgiot_data:init(?DGIOT_PARSE_ETS),
    dgiot_data:init(?ROLE_ETS),
    dgiot_data:init(?ROLE_USER_ETS),
    dgiot_data:init(?USER_ROLE_ETS),
    dgiot_data:init(?ROLE_PARENT_ETS),
    dgiot_data:init(?NAME_ROLE_ETS),
    dgiot_data:init(?CLASS_COUNT_ETS),
    dgiot_data:init(?PARENT_ROLE_ETS, [public, named_table, bag, {write_concurrency, true}, {read_concurrency, true}]).

%% 先缓存定时存库
save_to_cache(Requests) ->
    save_to_cache(?DEFAULT, Requests).
save_to_cache(Channel, Requests) when is_list(Requests) ->
    dgiot_dcache:insert(?CACHE(Channel), Requests),
    case check_cache(Channel) of
        true ->
            dgiot_dcache:save(?CACHE(Channel)),
            ok;
        false ->
            ok
    end;
save_to_cache(Channel, Request) when is_map(Request) ->
    save_to_cache(Channel, [Request]).

check_cache(Channel) ->
    Info = dgiot_dcache:info(?CACHE(Channel)),
    {size, Size} = lists:keyfind(size, 1, Info),
    case Size >= 200 of
        true ->
            true;
        false ->
            {memory, Memory} = lists:keyfind(memory, 1, Info),
            MaxSize = application:get_env(dgiot_parse, cache_max_size, 50000),
            MaxMemory = application:get_env(dgiot_parse, cache_max_memory, 102400),
            Size >= MaxSize orelse Memory >= MaxMemory
    end.

do_save(Channel) ->
    Fun =
        fun({Idx, Requests}, Acc) ->
            do_save(Idx, Channel, Requests, Acc)
        end,
    save_to_parse(Channel, dgiot_dcache:search(?CACHE(Channel), Fun)).

do_save(Idx, Channel, Requests, Acc) ->
    true = dgiot_dcache:delete(?CACHE(Channel), Idx),
    NewRequests = lists:foldl(fun(Request, Acc1) -> [Request | Acc1] end, Acc, Requests),
    case length(NewRequests) < 1000 of
        true ->
            {true, NewRequests};
        false ->
            ok = save_to_parse(Channel, NewRequests),
            {true, []}
    end.

save_to_parse(_, []) -> ok;
save_to_parse(Channel, Requests) ->
    case dgiot_parse:batch(Channel, Requests) of
        {ok, Results} ->
            dgiot_metrics:inc(dgiot_parse, <<"parse_save_success">>, length(Requests)),
            do_result(Requests, Results);
        %%  错误报文应该丢弃，不是所有报文都应该重新缓存
        #{<<"code">> := 100, <<"error">> := _Error} ->
            dgiot_metrics:inc(dgiot_parse, <<"parse_save_fail">>, length(Requests)),
            save_to_cache(Channel, Requests);
        Result ->
            log(Requests, Result),
            dgiot_metrics:inc(dgiot_parse, <<"parse_save_fail">>, length(Requests)),
            save_to_cache(Channel, Requests)
    end.

do_result([], []) ->
    ok;
do_result([Request | Requests], [Result | Results]) ->
    case maps:get(pid, Request, no) of
        no ->
            log(Request, Result);
        Pid ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                false ->
                    log(Request, Result);
                true ->
                    Pid ! {batch, maps:remove(pid, Request), Result}
            end
    end,
    do_result(Requests, Results).

log(_Request, #{<<"success">> := _}) -> ok;
log(Request, #{<<"error">> := Error}) ->
    io:format("save ~p, cache,~p~n", [Request, Error]).

save_test(Count) ->
    [save_to_cache(#{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Test">>,
        <<"body">> => #{
            <<"i">> => I
        }
    }) || I <- lists:seq(1, Count)], ok.

cache_classes(Order) ->
    case dgiot_parse:get_schemas() of
        {ok, #{<<"results">> := Results}} ->
            lists:map(fun
                          (#{<<"className">> := <<"Device">>}) ->
                              Success = fun(Page) ->
                                  lists:map(fun(Device) ->
                                      dgiot_device:save(Device)
                                            end, Page)
                                        end,
                              Query = #{
                                  <<"order">> => Order,
                                  <<"keys">> => [<<"ACL">>, <<"devaddr">>, <<"product">>, <<"deviceSecret">>],
                                  <<"where">> => #{}
                              },
                              dgiot_parse_loader:start(<<"Device">>, Query, 0, 500, 1000000, Success);
                          (#{<<"className">> := CLasseName}) ->
                              Success = fun(Page) ->
                                  lists:map(fun
                                                (#{<<"objectId">> := ObjectId} = Class) ->
                                                    dgiot_mnesia:insert(ObjectId, [binary_to_atom(CLasseName), get_acls(Class)]);
                                                (_) ->
                                                    pass
                                            end, Page)
                                        end,
                              Query = #{
                                  <<"keys">> => [<<"ACL">>],
                                  <<"order">> => Order,
                                  <<"where">> => #{}
                              },
                              dgiot_parse_loader:start(CLasseName, Query, 0, 500, 1000000, Success);
                          (_) ->
                              pass
                      end, Results);
        _ ->
            pass
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
                (RoleName,  Acc) when is_atom(RoleName) ->
                    Acc ++ [RoleName];
                (RoleName,  Acc) when is_binary(RoleName)->
                    Acc ++ [?ACL(RoleName)]
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
                    case dgiot_data:get(?NAME_ROLE_ETS, ?ACL(RoleName)) of
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
    case dgiot_data:get(?ROLE_ETS, dgiot_utils:to_binary(RoleId)) of
        #{<<"name">> := Name} ->
            ?ACL(<<"role:", Name/binary>>);
        _ -> %%  * or userid
            ?ACL(RoleId)
    end.

%% dgiot_parse_cache:get_count(<<"Device">>, [<<"role:root">>],acl).
%% dgiot_parse_cache:get_count(<<"ProductTemplet">>, [<<"role:root">>],acl).
get_count(Class, Acls, acl) ->
    ChildAcls =
        lists:foldl(
            fun
                (AclName, Acc) ->
                    Acc ++ dgiot_role:get_childacl(AclName)
            end, [], Acls),
    get_count(Class, dgiot_utils:unique_1(ChildAcls));

get_count(Class, RoleId, roleid) ->
    ChildRoleIds = dgiot_role:get_childrole(dgiot_utils:to_binary(RoleId)),
    Acls = lists:foldl(fun(ChildRoleId, Acc) ->
        Acc ++ [get_alcname(ChildRoleId)]
                       end, [get_alcname(RoleId)], ChildRoleIds),
    get_count(Class, Acls).

get_count({<<"Product">>}, Acls) ->
    lists:map(
        fun
            (ProductId) ->
                init_count(dgiot_utils:to_list(ProductId) ++ "_Device_true"),
                init_count(dgiot_utils:to_list(ProductId) ++ "_Device_false")
        end,
        dgiot_data:keys(dgiot_product)),
    init_count(<<"Product">>),
    get_count({<<"Device">>}, Acls),
    #{
        <<"count">> => lookup_count(<<"Product">>)
    };

get_count(<<"Device">>, Acls) ->
    init_count('Device'),
    init_count('Device_true'),
    init_count('Device_false'),
    loop_count(Acls ++ ['*']),
    #{
        <<"count">> => lookup_count(<<"Device">>),
        <<"online">> => lookup_count(<<"Device_true">>),
        <<"offline">> => lookup_count(<<"Device_false">>)
    };


get_count({ClassesName, Type}, Acls) ->
    Class = dgiot_utils:to_atom(ClassesName),
    init_count(Class),
    loop_count(Acls ++ ['*']),
    #{<<"count">> => get({parse_count, Class, Type})};

get_count(ClassesName, Acls) ->
    Class = dgiot_utils:to_atom(ClassesName),
    init_count(Class),
    loop_count(Acls ++ ['*']),
    #{<<"count">> => lookup_count(Class)}.

loop_count(CLasseName) when is_binary(CLasseName) ->
    Class = dgiot_utils:to_atom(CLasseName),
    init_count(Class),
    Success = fun(Page) ->
        lists:map(fun(_) ->
            add_count(Class)
                  end, Page)
              end,
    Query = #{
        <<"keys">> => [<<"objectId">>],
        <<"where">> => #{}
    },
    dgiot_parse_loader:start(CLasseName, Query, 0, 500, 1000000, Success);

loop_count(QueryAcls) ->
    AtomQueryAcls = get_acls(QueryAcls),
    Fun3 =
        fun
            ({_, _, ['Device', Acls, Status, _Time, _Devaddr, ProductId | _]}) ->
                case AtomQueryAcls -- Acls of
                    AtomQueryAcls ->
                        pass;
                    _ ->
                        add_count(<<"Device">>),
                        add_count("Device_" ++ dgiot_utils:to_list(Status)),
                        add_count("Device_" ++ dgiot_utils:to_list(Status),  dgiot_utils:to_list(ProductId))
                end;
            ({_, _, [ClassesName, Acls | _]}) ->
                case AtomQueryAcls -- Acls of
                    AtomQueryAcls ->
                        pass;
                    _ ->
                        add_count(ClassesName)
                end
        end,
    dgiot_mnesia:search(Fun3, #{<<"skip">> => 0, <<"limit">> => 1000000}).

init_count(Class) ->
    init_count(Class, all).
init_count(Class, Type) ->
    put({self(), dgiot_utils:to_atom(Class), dgiot_utils:to_atom(Type)}, 0).

add_count(Class) ->
    add_count(Class, all).
add_count(Class, Type) ->
    AtomClass = dgiot_utils:to_atom(Class),
    AtomType = dgiot_utils:to_atom(Type),
    case erlang:get({self(), AtomClass, AtomType}) of
        undefined ->
            erlang:put({self(), AtomClass, AtomType}, 1);
        Count ->
            NewCount = Count + 1,
            erlang:put({self(), AtomClass, AtomType}, NewCount)
    end.

lookup_count(Class) ->
    lookup_count(Class, all).
lookup_count(Class, Type) ->
    AtomClass = dgiot_utils:to_atom(Class),
    AtomType = dgiot_utils:to_atom(Type),
    erlang:get({self(), AtomClass, AtomType}).

test() ->
    Fun3 = fun
               ({_, _, ['Device' | _]} = X) ->
                   io:format("~p~n", [X]);
               ({_, _, [_ClassesName | _]}) ->
                   pass
           end,
    dgiot_mnesia:search(Fun3, #{<<"skip">> => 0, <<"limit">> => 1000000}).
