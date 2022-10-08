%%%-------------------------------------------------------------------
%%% @author dgiot
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 8月 2022 10:09
%%%-------------------------------------------------------------------
-module(dgiot_factory_printer).
-author("dgiot").

%% API
-export([
    requsts/1,
    template/2,
    handle_form/1,
    start_hooks/1,
    stop_hooks/1
]).
handle_form({QueryData, ProductId, _State}) ->
    template(QueryData, ProductId).

template(Profile, ProductId) ->
    ViewId = dgiot_parse_id:get_viewid(ProductId, <<"topo">>, <<"Product">>, ProductId),
    case dgiot_parse:get_object(<<"View">>, ViewId) of
        {ok, #{<<"data">> := #{<<"konva">> := #{<<"Stage">> := Stage}}}} ->
            io:format("~s ~p Profile= ~p Stage ~p ~n", [?FILE, ?LINE, Profile, Stage]),
            Profile;
        _ ->
            Profile
    end.
%%    Profile#{<<"profile">> => #{<<"data">> => List, <<"cmd">> => <<"printer">>}},


start_hooks(#{<<"product">> := Products}) ->
    lists:map(fun(X) ->
        case X of
            {ProductId, _} ->
                dgiot_hook:add(one_for_one, {sync_parse, before, put, ProductId}, fun dgiot_factory_printer:handle_form/1);
            _ ->
                pass
        end
              end, Products).

stop_hooks(#{<<"product">> := Products}) ->
    lists:map(fun(X) ->
        case X of
            {ProductId, _} ->
                dgiot_hook:remove({sync_parse, before, put, ProductId});
            _ ->
                pass
        end
              end, Products).


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
