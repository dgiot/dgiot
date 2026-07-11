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

-module(dgiot_paper).
-author("johnliu").
-include("dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    get_paper/2
]).

get_paper(_ProductId, FileInfo) ->
    Path = maps:get(<<"fullpath">>, FileInfo),
    Fun = fun(Row) ->
        Map = jiffy:encode(#{<<"1">> => dgiot_utils:to_binary(Row)}),
        [V | _] = maps:values(jsx:decode(Map, [{labels, binary}, return_maps])),
        V
          end,
    List = dgiot_utils:read(Path, Fun, []),
%%    Title = lists:nth(1, List),
%    DeviceId = dgiot_parse_id:get_deviceid(ProductId, dgiot_utils:to_md5(Title)),
    Single = split_list(<<"一、单选题"/utf8>>, <<"二、多选题"/utf8>>, false, List, []),
    Multiple = split_list(<<"二、多选题"/utf8>>, <<"三、判断题"/utf8>>, false, List, []),
    Judge = split_list(<<"三、判断题"/utf8>>, <<"四、案例题"/utf8>>, false, List, []),
    Cases = split_list(<<"四、案例题"/utf8>>, <<"四、案例题222"/utf8>>, false, List, []),
    Cases1 = get_case(Cases, {<<"">>, []}, []),
    {Single_question, _} = get_simple(Single, {[], #{}}),
    {Multiple_question, _} = get_simple(Multiple, {[], #{}}),
    {Judge_question, _} = get_simple(Judge, {[], #{}}),
    Paper = Single_question ++ Multiple_question ++ Judge_question ++ Cases1,
%    create_device(DeviceId, ProductId, Title, Paper),
    #{
        <<"paper">> => Paper
    }.

get_simple([], {Acc, Map}) ->
    {Acc, Map};
get_simple([Row | List], {Acc, Map}) ->
    case Row of
        <<"A."/utf8, _Result/binary>> ->
            get_simple(List, {Acc, Map#{<<"A"/utf8>> => Row}});
        <<"B."/utf8, _Result/binary>> ->
            get_simple(List, {Acc, Map#{<<"B"/utf8>> => Row}});
        <<"C."/utf8, _Result/binary>> ->
            get_simple(List, {Acc, Map#{<<"C"/utf8>> => Row}});
        <<"D."/utf8, _Result/binary>> ->
            get_simple(List, {Acc, Map#{<<"D"/utf8>> => Row}});
        <<"E."/utf8, _Result/binary>> ->
            get_simple(List, {Acc, Map#{<<"E"/utf8>> => Row}});
        <<"F."/utf8, _Result/binary>> ->
            get_simple(List, {Acc, Map#{<<"F"/utf8>> => Row}});
        <<"答案："/utf8, Result/binary>> ->
            R1 = re:replace(Result, <<"\n">>, <<>>, [{return, binary}]),
            R = re:replace(R1, <<" ">>, <<>>, [{return, binary}]),
            get_simple(List, {Acc ++ [Map#{<<"Answer"/utf8>> => R}], #{}});
        <<"答案:"/utf8, Result/binary>> ->
            R1 = re:replace(Result, <<"\n">>, <<>>, [{return, binary}]),
            R = re:replace(R1, <<" ">>, <<>>, [{return, binary}]),
            get_simple(List, {Acc ++ [Map#{<<"Answer"/utf8>> => R}], #{}});
        <<"\n"/utf8, _/binary>> ->
            get_simple(List, {Acc, Map});
        R when size(R) > 6 ->
            get_simple(List, {Acc, Map#{<<"Question"/utf8>> => Row, <<"type">> => get_type(R)}});
        _ ->
            get_simple(List, {Acc, Map})
    end.

get_type(Question) ->
%%    io:format("~ts", [unicode:characters_to_list(Question)]),
    case re:run(Question, <<"判断"/utf8>>, [{capture, none}]) of
        match ->
            <<"判断题"/utf8>>;
        _ ->
            case re:run(Question, <<"多选"/utf8>>, [{capture, none}]) of
                match ->
                    <<"多选题"/utf8>>;
                _ ->
                    <<"单选题"/utf8>>
            end
    end.

get_case([], {Title, Acc}, Result) ->
    {Single_question, _} = get_simple(Acc, {[], #{}}),
    Result ++ [#{<<"type">> => <<"材料题"/utf8>>, <<"Question"/utf8>> => Title, <<"questions"/utf8>> => Single_question}];
get_case([Row | List], {Title, Acc}, Result) ->
    case re:run(Row, <<"背景材料"/utf8>>, [{capture, none}]) of
        match ->
            case Title of
                <<"">> ->
                    get_case(List, {Row, Acc}, Result);
                _ ->
                    {Single_question, _} = get_simple(Acc, {[], #{}}),
                    get_case(List, {Row, []}, Result ++ [#{<<"type">> => <<"材料题"/utf8>>, <<"Question"/utf8>> => Title, <<"questions"/utf8>> => Single_question}])
            end;
        _ ->
            get_case(List, {Title, Acc ++ [Row]}, Result)
    end.

split_list(_Start, _End, _Flag, [], Result) ->
    Result;
split_list(Start, End, Flag, [Row | Acc], Result) ->
    case re:run(Row, Start, [{capture, first, list}]) of
        {match, [_]} ->
            split_list(Start, End, true, Acc, Result);
        _ ->
            case re:run(Row, End, [{capture, first, list}]) of
                {match, [_]} ->
                    Result;
                _ ->
                    case Flag of
                        true ->
                            split_list(Start, End, Flag, Acc, Result ++ [Row]);
                        _ ->
                            split_list(Start, End, Flag, Acc, Result)
                    end
            end
    end.

