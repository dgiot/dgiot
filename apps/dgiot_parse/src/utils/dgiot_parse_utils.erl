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

-module(dgiot_parse_utils).
-author("zwx").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_parse.hrl").
-dgiot_swagger(<<"classes">>).

-define(RE_OPTIONS, [global, {return, binary}]).

%% API.
-export([swaggerApi/0, check_parse/2, transform_classes/2, get_paths/2, get_navigation/2, default_schemas/0, create_tree/2, get_classtree/4]).

check_parse(Tables, I) ->
    case catch dgiot_parse:health() of
        {ok, #{<<"status">> := <<"ok">>}} ->
            case dgiot_parse:get_schemas() of
                {ok, #{<<"results">> := Schemas}} ->
                    Fun =
                        fun(#{<<"className">> := ClassName} = Schema, Acc) ->
                            case Tables == "*" orelse lists:member(ClassName, Tables) of
                                true ->
                                    case catch dgiot_parse_utils:get_paths(Schema, Acc) of
                                        {'EXIT', Reason} ->
                                            ?LOG(warning, "~p,~p~n", [Schema, Reason]),
                                            Acc;
                                        NewAcc ->
                                            NewAcc
                                    end;
                                false ->
                                    Acc
                            end
                        end,
                    lists:foldl(Fun, dgiot_parse_utils:default_schemas(), Schemas);
                {error, Reason} ->
                    {error, Reason}
            end;
        {Type, Reason} when Type == error; Type == 'EXIT' ->
            case I > 5 of
                true ->
                    {error, Reason};
                false ->
                    ?LOG(error, "ParseServer is not health,~p!!!", [Reason]),
                    receive after 5000 -> check_parse(Tables, I + 1) end
            end
    end.

get_navigation(#{<<"user">> := #{<<"roles">> := Roles}}, Args) ->
    RoleList = maps:to_list(Roles),
    Requests = [#{
        <<"method">> => <<"GET">>,
        <<"path">> => <<"/classes/Menu">>,
        <<"body">> => Args#{
            <<"where">> => #{
                <<"$relatedTo">> => #{
                    <<"key">> => <<"menus">>,
                    <<"object">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"_Role">>, <<"objectId">> => RoleId}
                }
            }
        }
    } || {RoleId, _} <- RoleList],
    case dgiot_parse:batch(Requests) of
        {ok, Results} ->
            get_navigation_by_result(Results, RoleList);
        {error, Reason} ->
            {error, Reason}
    end.

get_navigation_by_result(Results, Roles) ->
    get_navigation_by_result(Results, Roles, #{}).
get_navigation_by_result([], [], Acc) ->
    {ok, Acc};
get_navigation_by_result([#{<<"error">> := Err} | _], _, _) ->
    {error, Err};
get_navigation_by_result([#{<<"success">> := #{<<"results">> := Result}} | Menus], [Role | Roles], Acc0) ->
    {RoleId, #{<<"name">> := RoleName, <<"alias">> := Alias}} = Role,
    Fun =
        fun(#{<<"objectId">> := MenuId} = Menu, NewAcc) ->
            Show = #{<<"roleId">> => RoleId, <<"roleName">> => RoleName, <<"alias">> => Alias},
            case maps:get(MenuId, NewAcc, no) of
                no ->
                    NewAcc#{MenuId => Menu#{<<"navShow">> => [Show]}};
                #{<<"navShow">> := RoleIds} ->
                    NewAcc#{MenuId => Menu#{<<"navShow">> => [Show | RoleIds]}}
            end
        end,
    get_navigation_by_result(Menus, Roles, lists:foldl(Fun, Acc0, Result)).

get_classtree(ClassName, Parent, Filter, SessionToken) ->
    NewFilter = dgiot_bamis:format(Filter),
    case dgiot_parse:query_object(ClassName, NewFilter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Classes}} when length(Classes) > 0 ->
            NewClasses =
                lists:foldl(fun(Class, Acc) ->
                    NewClasse = Class#{
                        <<"label">> => maps:get(<<"name">>, Class, <<"label">>),
                        <<"value">> => maps:get(<<"name">>, Class, <<"value">>),
                        Parent => maps:get(Parent, Class, <<"0">>)},
                    Acc ++ [maps:without([<<"createdAt">>, <<"updatedAt">>, <<"ACL">>], NewClasse)]
                            end, [], Classes),
            ClassTree = dgiot_parse_utils:create_tree(NewClasses, Parent),
            Len = length(ClassTree),
            Num = 1000,
            case Len =< Num of
                true ->
                    {200, #{<<"results">> => ClassTree}};
                false ->
                    ShowTrees = lists:sublist(ClassTree, 1, Num),
                    MoreTrees = lists:sublist(ClassTree, Num + 1, length(ClassTree) - Num),
                    {200, #{<<"results">> => ShowTrees ++ [#{
                        <<"children">> => MoreTrees,
                        <<"name">> => <<"...">>,
                        <<"label">> => <<"...">>,
                        <<"value">> => <<"...">>,
                        <<"icon">> => <<"More">>,
                        Parent => <<"0">>,
                        <<"url">> => <<"/more">>
                    }]}}
            end;
        _ -> {error, <<"1 not find">>}
    end.

create_tree(Items, Parent) ->
%%    ?LOG(info,"Items ~p", [Items]),
    NewItems = lists:foldl(
        fun(#{<<"objectId">> := Id} = Item, Acc) ->
            ParentId =
                case maps:get(Parent, Item, no) of
                    #{<<"objectId">> := PId} ->
                        binary:replace(PId, <<" ">>, <<>>, [global]);
                    _ ->
                        <<"0">>
                end,
            Node = Item#{Parent => ParentId},
            [{Id, ParentId, Node} | Acc]
        end, [], Items),
    Tree = lists:foldl(
        fun({Id, PId, Node}, Acc) ->
            case lists:keyfind(PId, 1, NewItems) of
                false ->
                    case get_children(Id, NewItems) of
                        [] ->
                            [Node | Acc];
                        Children ->
                            [Node#{<<"children">> => Children} | Acc]
                    end;
                _ ->
                    Acc
            end
        end, [], NewItems),
    lists:sort(fun children_sort/2, Tree).

get_children(CId, Items) ->
    Tree = lists:filtermap(
        fun({Id, PId, Node}) ->
            case PId == CId of
                true ->
                    case get_children(Id, Items) of
                        [] ->
                            {true, Node};
                        Children ->
                            {true, Node#{<<"children">> => Children}}
                    end;
                false ->
                    false
            end
        end, Items),
    lists:sort(fun children_sort/2, Tree).

children_sort(Node1, Node2) ->
    Order1 = maps:get(<<"orderBy">>, Node1, 0),
    Order2 = maps:get(<<"orderBy">>, Node2, 0),
    Order1 < Order2.


%% ==========================
%%  swagger
%% ==========================

default_schemas() ->
    NewSWSchemas =
        case dgiot_swagger:load_schema(?MODULE, "swagger_parse.json", [{labels, binary}, return_maps]) of
            {ok, SWSchemas} ->
                SWSchemas;
            _ ->
                #{}
        end,
    OldComponents = maps:get(<<"definitions">>, NewSWSchemas, #{}),
    NewSWSchemas#{
        <<"definitions">> => OldComponents
    }.

transform_classes(#{<<"className">> := ClassName, <<"fields">> := Fields}, ComSchemas) ->
    Fun =
        fun(Key, Map, Acc) ->
            Acc#{Key => to_swagger_type(Map#{<<"field">> => Key})}
        end,
    Properties = maps:fold(Fun, #{}, Fields),
    DelCols = [],
    Props = maps:without(DelCols, Properties),
    maps:merge(#{
        ClassName => #{
            <<"type">> => <<"object">>,
            <<"properties">> => Props
        }
    }, ComSchemas).


to_swagger_type(#{<<"type">> := Type}) when Type == <<"ACL">> ->
    #{
        <<"type">> => <<"object">>,
        <<"example">> => #{<<"*">> => #{<<"read">> => true, <<"write">> => false}},
        <<"properties">> => #{
        }
    };
to_swagger_type(#{<<"type">> := Type, <<"targetClass">> := Class}) when Type == <<"Pointer">> ->
    #{
        <<"type">> => <<"object">>,
        <<"description">> => <<"">>,
        <<"properties">> => #{
            <<"objectId">> => #{
                <<"type">> => <<"string">>
            },
            <<"__type">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Type
            },
            <<"className">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Class
            }
        }
    };
to_swagger_type(#{<<"type">> := Type, <<"targetClass">> := Class}) when Type == <<"Relation">> ->
    #{
        <<"type">> => <<"object">>,
        <<"description">> => <<"">>,
        <<"properties">> => #{
            <<"__type">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Type
            },
            <<"className">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Class
            }
        }
    };
to_swagger_type(#{<<"type">> := Type}) when Type == <<"File">>; Type == <<"Object">> ->
    #{<<"type">> => <<"object">>};
to_swagger_type(#{<<"type">> := Type}) when Type == <<"Number">>;Type == <<"Boolean">>;Type == <<"String">> ->
    #{<<"type">> => list_to_binary(string:to_lower(binary_to_list(Type)))};
to_swagger_type(#{<<"type">> := <<"Array">>}) ->
    #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
        }
    };
to_swagger_type(_) ->
    #{<<"type">> => <<"string">>}.

swaggerApi() ->
    Fun =
        fun({_App, _Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) of
                false ->
                    Acc;
                _ ->
                    case [SwaggerType || {dgiot_swagger, [SwaggerType]} <- Mod:module_info(attributes)] of
                        [] ->
                            Acc;
                        [SwaggerType | _] ->
                            [{SwaggerType, Mod}] ++ Acc
                    end
            end
        end,
    lists:sort(dgiot_plugin:check_module(Fun, [])).

get_paths(Schema, Acc) ->
    SwaggerApi = swaggerApi(),
    dgiot_data:insert(swaggerApi, SwaggerApi),
    lists:foldl(fun(Type, NewAcc) ->
        add_paths(Schema, NewAcc, Type)
                end, Acc, SwaggerApi).

add_paths(#{<<"className">> := ClassName} = Schema, Acc, {Type, Mod}) ->
    Definitions = maps:get(<<"definitions">>, Acc, #{}),
    Paths = maps:get(<<"paths">>, Acc, #{}),
    Tags = maps:get(<<"tags">>, Acc, []),
    CSchema = get_path(Tags, ClassName, Type, Mod),
    CDefinitions = maps:get(<<"definitions">>, CSchema, #{}),
    CPaths = maps:get(<<"paths">>, CSchema, #{}),
    CTags = maps:get(<<"tags">>, CSchema, []),
    Acc#{
        <<"definitions">> => transform_classes(maps:merge(CDefinitions, Schema), Definitions),
        <<"paths">> => maps:merge(Paths, CPaths),
        <<"tags">> => lists:foldl(
            fun(#{<<"name">> := TagName} = Tag0, Acc1) ->
                NewTags = lists:filtermap(
                    fun(#{<<"name">> := OldTagName}) ->
                        OldTagName =/= TagName
                    end, Acc1),
                [Tag0 | NewTags]
            end, Tags, CTags)
    }.

get_path(Tags, ClassName, Type, Mod) ->
    {ok, Bin} = dgiot_swagger:load_schema(Mod, "swagger_" ++ dgiot_utils:to_list(Type) ++ ".json", []),
    Data = re:replace(Bin, "\\{\\{className\\}\\}", ClassName, ?RE_OPTIONS),
    CTags = lists:filtermap(fun(#{<<"name">> := Name}) -> Name == ClassName end, Tags),
    Desc =
        case CTags of
            [] -> ClassName;
            [#{<<"description">> := Description} | _] -> Description
        end,
    Data1 = re:replace(Data, "\\{\\{tag\\}\\}", Desc, ?RE_OPTIONS),
    Data2 = re:replace(Data1, "\\{\\{classes\\}\\}", Type, ?RE_OPTIONS),
    jsx:decode(Data2, [{labels, binary}, return_maps]).




