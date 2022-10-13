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
-module(dgiot_printer).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").

-export([
    handle_profile/1,
    get_topo/2,
    get_word/1,
    start_hooks/1,
    stop_hooks/1
]).

handle_profile({QueryData, ProductId, _State}) ->
    get_topo(QueryData, ProductId).

get_topo(#{<<"profile">> := Profile} = QueryData, ProductId) ->
    case dgiot_product_knova:get_stage(ProductId) of
        {ok, Stage} ->
            StageMap = dgiot_product_knova:get_nodes(Stage, [<<"Text">>, <<"Rect">>]),
            NewProfile =
                maps:fold(fun(K, V, Acc) ->
                    case maps:find(K, Profile) of
                        error ->

                            Acc ++ [V];
                        {ok, Text} ->
                            Acc ++ [V#{<<"text">> => Text}]
                    end
                          end, [], StageMap),
            Cmd = maps:get(<<"cmd">>, Profile, <<"printer_barcode">>),
            QueryData#{<<"profile">> => #{<<"cmd">> => Cmd, <<"data">> => NewProfile}};
        _ ->
            QueryData
    end.

start_hooks(#{<<"product">> := Products}) ->
    lists:map(fun(X) ->
        case X of
            {ProductId, _} ->
                dgiot_hook:add(one_for_one, {sync_parse, before, put, ProductId}, fun dgiot_printer:handle_profile/1);
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

get_word(#{<<"id">> := TaskId, <<"cmd">> := Printer, <<"data">> := Data, <<"Home">> := Home} = Profile) ->
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
