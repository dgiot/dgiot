%%%-------------------------------------------------------------------
%%% @author dgiot
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 8月 2022 10:09
%%%-------------------------------------------------------------------
-module(dgiot_factory_repliceword).
-author("dgiot").

%% API
-export([
    requsts/1,
    template/1,
    handle_form/1
]).
handle_form({QueryData, _State}) ->
    template( QueryData  ).


requsts(#{<<"id">> := TaskId,
    <<"cmd">> := Printer, <<"data">> := Data, <<"Home">> := Home
} = Profile) ->
    Url = "http://127.0.0.1:8012/WordController/replaceWord",
    io:format(" TaskId= ~p ~n", [TaskId]),
    case dgiot_parse:get_object(<<"Device">>, TaskId) of
        {ok, #{<<"name">> := _TaskName, <<"basedata">> := _Basedata, <<"profile">> := Profiles = _Profile,
            <<"product">> := ProductId,
            <<"parentId">> := #{<<"__type">> := <<"Pointer">>, <<"className">> := <<"Device">>, <<"objectId">> := _ParentId}
        }} ->
            DictId = dgiot_parse_id:get_dictid(ProductId, <<"word">>, <<"Product">>, <<"worddict">>),
            case dgiot_parse:get_object(<<"Dict">>, DictId) of
                {ok, #{<<"data">> := #{<<"params">> := Params}}} ->
                    Lists =
                        lists:foldl(fun(Param, Acc) ->
                            Sources = maps:get(<<"sources">>, Param, <<"">>),
                            Type = maps:get(<<"type">>, Param, <<"">>),
                            Identifier = maps:get(<<"identifier">>, Param, <<"">>),
                            Value = maps:get(Identifier, Profiles, <<"">>),
                            case Sources of
                                <<"amis">> ->
                                    case Type of
                                        <<"text">> ->
                                            Acc ++ [#{
                                                <<"url">> => <<"">>,
                                                <<"header">> => <<"">>,
                                                <<"height">> => <<"">>,
                                                <<"source">> => <<"">>,
                                                <<"tag">> => <<"">>,
                                                <<"tablecolumn">> => 0,
                                                <<"type">> => <<"text">>,
                                                <<"tablerow">> => 0,
                                                <<"width">> => 0,
                                                <<"name">> => Identifier,
                                                <<"value">> => Value}];
                                        _ ->
                                            Acc
                                    end

                            end
                                    end, [], Params),
                    TemplateUrl = dgiot_utils:to_binary("http://" ++ Home ++ ":8012/wordServer/test.docx"),
                    Body = #{
                        <<"datas">> => Lists,
                        <<"path">> => <<"dgiot_file/device/topo">>,
                        <<"templateUrl">> => TemplateUrl,
                        <<"wordName">> => <<"test">>
                    },
                    Request = {Url, [], "application/json", jiffy:encode(Body)},
                    io:format(" Request= ~p ~n", [Request]),
                    Urlpath = case dgiot_http_client:request(post, Request) of
                                  {ok, #{<<"images">> := Img}} -> {
                                      maps:get(<<"url">>, lists:nth(1, Img))
                                  };
                                  _ -> {}
                              end,
                    #{
                        <<"cmd">> => Printer,
                        <<"data">> => Data,
                        <<"url">> => <<Home/binary, Urlpath/binary>>
                    };
                _Oth ->
                    Profile
            end;
        _Oth1 ->
            Profile
    end.

template(Profile) ->
    Age = maps:get(<<"profile">>, Profile),
    case maps:get(<<"cmd">>, Age, <<"">>) of
        <<"printer"/utf8>> ->
            Product = maps:get(<<"factoryid">>, Age, <<"">>),
            {ok, #{<<"product">> := #{<<"objectId">> := Productid}}} = dgiot_parse:get_object(<<"Device">>, maps:get(<<"id">>, Age)),
            NewStage = case dgiot_parse:query_object(<<"View">>, #{<<"limit">> => 1, <<"where">> => #{<<"key">> => Productid, <<"type">> => <<"amis">>, <<"class">> => <<"Product">>, <<"title">> => Product}}) of
                           {ok, #{<<"results">> := Views}} when length(Views) > 0 ->

                               lists:foldl(fun(View, Acc) ->
                                   case View of
                                       #{<<"data">> := ViewId2} ->
                                           Acc#{<<"data">> => ViewId2};
                                       _ ->
                                           Acc
                                   end
                                           end, #{}, Views);
                           _ -> #{}
                       end,
            Body = maps:get(<<"body">>, maps:get(<<"data">>, NewStage)),
            List = lists:foldl(
                fun(X, Acc) ->
                    Label = maps:get(<<"label">>, X),
                    Label1 = dgiot_utils:to_list(Label),
                    Replace = maps:fold(
                        fun(Key, V, Acc1) ->
                            K = lists:concat([" ", dgiot_utils:to_list(Key)]),
                            A = case re:replace(Label, K, dgiot_utils:to_binary(V), [global, {return, list}]) of
                                    Label1 -> [];
                                    Row -> Row
                                end,

%%                    io:format("~s ~p A= ~p ~n", [?FILE, ?LINE, A]),
                            A ++ Acc1
                        end, [], Age),
%%            io:format("~s ~p Replace = ~ts ~n", [?FILE, ?LINE, unicode:characters_to_list(dgiot_utils:to_binary(Replace))]),
                    case Replace of
                        [] ->
                            case re:run(Label, <<" ">>, [{capture, none}]) of
                                match -> Replaceddd = lists:nth(1, binary:split(Label, <<" ">>)),
                                    Acc#{dgiot_utils:to_binary(maps:get(<<"num">>, X)) => maps:update(<<"label">>, dgiot_utils:to_binary(Replaceddd), X)};
                                nomatch -> Acc#{dgiot_utils:to_binary(maps:get(<<"num">>, X)) => X}
                            end;

                        _ ->
                            Acc#{dgiot_utils:to_binary(maps:get(<<"num">>, X)) => maps:update(<<"label">>, dgiot_utils:to_binary(Replace), X)}
                    end

                end, #{}, Body
            ),

            maps:update(<<"profile">>, #{<<"data">> => List,
                <<"cmd">> => <<"printer">>
            }, Profile);
        <<"">> -> Profile
    end.



