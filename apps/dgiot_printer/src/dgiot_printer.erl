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
    get_topo/1,
    get_word/1
]).

get_topo(ProductId) ->
    case dgiot_product_knova:get_stage(ProductId) of
        {ok, Stage} ->
            Nodes = dgiot_product_knova:get_nodes(Stage, [<<"Text">>]),
            io:format("~s ~p  Nodes ~p ~n", [?FILE, ?LINE, Nodes]);
        _ ->
            ok
    end.

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