-module(rulesql_tests).

-include_lib("eunit/include/eunit.hrl").

-include("sql_lex.hrl").

-compile(export_all).
-compile(nowarn_export_all).

keyword_1_test() ->
    [?assert(rulesql:is_reserved(Key)) || Key <- ?RESERVED_KEYS].

select_test_() ->
    [
        %% basic select
        ?_assertMatch(
            {ok,{select,
                    [{fields,['*']},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT * FROM \"abc\"">>)),

        %% select caluse with a single variable
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{var, <<"x">>}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT x FROM abc">>)),

        %% select caluse with a single const
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{const, <<"x">>}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT 'x' FROM abc">>)),

        %% select caluse with some variables
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{var, <<"x">>}, {var, <<"y">>}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT x,y FROM abc">>)),

        %% select caluse with both '*' and variables
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{var, <<"x">>}, {var, <<"y">>}, '*']},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT x,y,* FROM abc">>)),

        %% multiple variables will keep the order
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{var, <<"x">>}, '*', {var, <<"y">>}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT x,*,y FROM abc">>)),

        %% operator precedences
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as,{'+', {var, <<"z">>},
                                  {'*', {var, <<"x">>}, {var, <<"y">>}}},
                            {var, <<"z">>}}
                      ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT z + x * y as z FROM abc">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as,{'*', {'+', {var, <<"z">>}, {var, <<"x">>}},
                                  {var, <<"y">>}},
                            {var, <<"z">>}}
                      ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT (z + x) * y as z FROM abc">>))
    ].

vars_and_consts_test_() ->
    [
        %% identifiers without single quotes are vars
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{'=', {var, <<"a">>}, {var, <<"b">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a = b FROM abc">>)),

        %% identifiers wrapped by single quotes are consts
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{'=', {var, <<"a">>}, {const, <<"b">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a = 'b' FROM abc">>)),

        %% numbers without single quotes are consts
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{'=', {const, 1}, {const, 1.1}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT 1 = 1.1 FROM abc">>)),

        %% numbers wrapped by single quotes are strings
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{'=', {const, <<"1">>}, {const, <<"1.1">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT '1' = '1.1' FROM abc">>))
    ].

maps_get_test_() ->
    [
        %% one-depth maps get
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>}, {key,<<"b">>}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.b FROM abc">>)),

        %% multiple-depth maps get
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{path, [{key,<<"a">>},{key,<<"b">>},{key,<<"c">>},{key,<<"d">>},{key,<<"e">>}]}
                        ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.b.c.d.e FROM abc">>)),

        %% multiple-depth maps get and a single var
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{path, [{key,<<"a">>},{key,<<"b">>},{key,<<"c">>},{key,<<"d">>},{key,<<"e">>}]},
                         {var,<<"e">>}
                        ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.b.c.d.e, e FROM abc">>)),

        %% mixed maps get with index get
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>},{index,{const,1}},{key,<<"b">>}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[1].b FROM abc">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>},{key,<<"b">>},{index,{const,1}}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.b[1] FROM abc">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>},{key,<<"b">>},{key,<<"c">>},{index,{const,1}}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.b.c[1] FROM abc">>)),

        %% mixed maps get with index get
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>},{index,{const,-1}},{key,<<"b">>}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[-1].b FROM abc">>)),

        %% mixed maps get with multiple depth index get
        ?_assertMatch(
            {ok,{select,
                    [{fields, [{path, [{key,<<"a">>}, {index,{const,1}}, {index,{const,2}}, {key, <<"b">>}, {index,{var,<<"c">>}}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[1][2].b[c] FROM abc">>))
    ].

maps_get_2_test_() ->
    [
        %% maps get with numeric string keys
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>}, {key,<<"1">>}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.1 FROM abc">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>}, {key,<<"1">>}, {key,<<"b">>}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.1.'b' FROM abc">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path, [{key,<<"a">>}, {key,<<"1.5">>}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.'1.5' FROM abc">>)),
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT 1.b FROM abc">>)),
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT [1].b FROM abc">>)),

        %% index get cannot follow '.'
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT a[1].[2] FROM abc">>)),

        %% 1.100 is a float number
        ?_assertMatch(
            {ok,{select,[{fields,[{const,1.1}]},
                {from,[<<"abc">>]},
                {where,{}}]}},
            rulesql:parsetree(<<"SELECT 1.100 FROM abc">>)),

        %% a.1.100 is invalid because it is ambiguous:
        %% a.'1'.'100' or a.'1.100' ?
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT a.1.100 FROM abc">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{path,
                        [{key,<<"a">>},
                         {key,<<"b">>},
                         {key,<<"c">>},
                         {index,{const,1}},
                         {key,<<"1">>},
                         {key,<<"100">>}]}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.b.c[1].'1'.100 FROM abc">>))
    ].

maps_put_test_() ->
    [
        %% one-depth maps put
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {'as',{var, <<"e">>},
                              {path, [{key,<<"c">>}, {key,<<"d">>}]}
                        }
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT e as c.d FROM abc">>)),

        %% one-depth maps put
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {'as',{path, [{key,<<"e">>}, {key,<<"f">>}]},
                              {path, [{key,<<"c">>}, {key,<<"d">>}]}
                        }
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT e.f as c.d FROM abc">>)),

        %% mixed maps put and array index put
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {'as',{path, [{key,<<"e">>}, {key,<<"f">>}]},
                              {path, [{key,<<"c">>}, {key,<<"d">>}, {index,{const,1}}]}
                        }
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT e.f as c.d[1] FROM abc">>))
    ].

array_index_get_() ->
    [
        %% one-depth array index get
        ?_assertMatch(
            {ok,{select,
                    [{fields,{path, [{key,<<"a">>}, {index,{const,1}}]}},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[1] FROM abc">>)),

        %% variable index
        ?_assertMatch(
            {ok,{select,
                    [{fields,{path, [{key,<<"a">>}, {index,{var,<<"e">>}}]}},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[e] FROM abc">>)),

        %% path index
        ?_assertMatch(
            {ok,{select,
                    [{fields,{path, [{key,<<"a">>}, {path, [{key, <<"e">>}, {key, <<"f">>}]}]}},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[e.f] FROM abc">>)),

        %% path index
        ?_assertMatch(
            {ok,{select,
                    [{fields,{path, [{var,<<"a">>}, {path, [{var, <<"e">>}, {index, {const,1}}]}]}},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[e[1]] FROM abc">>)),

        %% multi-depth array index get
        ?_assertMatch(
            {ok,{select,
                    [{fields,{path, [{var,<<"a">>}, {index,{const,1}}, {index,{const,2}}, {index,{var,<<"e">>}}]}},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[1][2][e] FROM abc">>))
    ].

range_test_() ->
    [
        %% make an integer range
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as, {range, {1, 3}}, {var, <<"a">>}}
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT [1..3] as a FROM abc">>)),

        %% get range
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as, {get_range, {1, 3}, {var, <<"a">>}}, {var, <<"b">>}}
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[1..3] as b FROM abc">>)),

        %% get range also supports negitive integers
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as, {get_range, {1, -1}, {var, <<"a">>}}, {var, <<"b">>}}
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[1..-1] as b FROM abc">>)),

        %% range may or may not have 'as' clause
        ?_assertMatch(
            {ok,{select,[{fields,[{range,{1,3}}]},
                         {from,[<<"abc">>]},
                         {where,{}}]}},
            rulesql:parsetree(<<"SELECT [1..3] FROM abc">>)),
        ?_assertMatch(
            {ok,{select,[{fields,[{get_range,{1,3},{var,<<"a">>}}]},
                                  {from,[<<"abc">>]},
                                  {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[1..3] FROM abc">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{get_range,
                            {1,3},
                            {path,[{key,<<"a">>},{key,<<"e">>}]}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a.e[1..3] FROM abc">>)),

        %% range can be applied on a path
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as, {const, <<"hah">>}, {var, <<"c">>}},
                        {as, {get_range, {1, 3}, {path, [{key, <<"a">>}, {key, <<"b">>}]}},
                             {var, <<"d">>}}
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT 'hah' as c, a.b[1..3] as d FROM abc">>)),

        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as, {get_range, {1, 3}, {path, [{key, <<"a">>}, {index, {var, <<"b">>}}]}},
                             {var, <<"r">>}}
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a[b][1..3] as r FROM abc">>)),

        %% but can not be used as part of the path..
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT a.b[1..3].c FROM abc">>)),
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT a.[1][1..3].c FROM abc">>)),

        %% range can not be used in AS clause
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT a as [1..3] FROM abc">>)),
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT a as e[1..3] FROM abc">>))
    ].

as_test_() ->
    [
        %% basic as
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{as, {var, <<"a">>}, {var, <<"b">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a as b FROM \"abc\"">>)),

        %% basic as another way
        ?_assertMatch(
            {ok,{select,
                    [{fields,[{as, {var, <<"a">>}, {var, <<"b">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a b FROM \"abc\"">>)),

        %% as clause can be used along with the ordinary varibales
        ?_assertMatch(
            {ok,{select,
                    [{fields,[
                        {as, {var, <<"a">>}, {var, <<"b">>}},
                        {var, <<"x">>},
                        '*'
                     ]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a as b, x, * FROM \"abc\"">>))
    ].

non_as_test_() ->
    [
        %% expressions without 'as' is also allowed

        ?_assertMatch(
            {ok,{select,[{fields,[{'-',{const,1}}]},
                         {from,[<<"abc">>]},
                         {where,{}}]}},
            rulesql:parsetree(<<"SELECT -1 FROM \"abc\"">>)),
        ?_assertMatch(
            {ok,{select,[{fields,[{as,{'-',{const,1}},{var,<<"a">>}}]},
                         {from,[<<"abc">>]},
                         {where,{}}]}},
            rulesql:parsetree(<<"SELECT -1 as a FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,[{fields,[{'+',{const,1},{const,1}}]},
                         {from,[<<"abc">>]},
                         {where,{}}]}},
            rulesql:parsetree(<<"SELECT 1 + 1 FROM \"abc\"">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{'+',{const,1},{const,1}},{var,<<"a">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT 1 + 1 as a FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,[{fields,[{'/',{var,<<"x">>},{var,<<"y">>}}]},
                         {from,[<<"abc">>]},
                         {where,{}}]}},
            rulesql:parsetree(<<"SELECT x / y FROM \"abc\"">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{'/',{var,<<"x">>},{var,<<"y">>}},
                            {var,<<"e">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT x / y as e FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{'fun',
                            {var,<<"nth">>},
                            [{const,1},{var,<<"a">>}]}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT nth(1,a) FROM \"abc\"">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{'fun',
                                {var,<<"nth">>},
                                [{const,1},{var,<<"a">>}]},
                            {var,<<"a">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT nth(1,a) as a FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,[{fields,[{'=',{var,<<"a">>},{const,<<"1">>}}]},
                         {from,[<<"abc">>]},
                         {where,{}}]}},
            rulesql:parsetree(<<"SELECT a = '1' FROM \"abc\"">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{'=',{var,<<"a">>},{const,<<"1">>}},
                             {var,<<"a">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT a = '1' as a FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{'case',<<>>,
                            [{{'>',{var,<<"a">>},{var,<<"b">>}},
                                {const,1}}],
                            {}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT case "
                                    "when a > b then 1 "
                                "end FROM \"abc\"">>)),
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{'case',<<>>,
                                [{{'>',{var,<<"a">>},{var,<<"b">>}},
                                    {const,1}}],
                                {}},
                            {var,<<"d">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT case "
                                    "when a > b then 1 "
                                "end as d FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{'case',<<>>,
                            [{{'>',{var,<<"a">>},{var,<<"b">>}},
                                {var,<<"a">>}},
                            {{'<',{var,<<"a">>},{var,<<"b">>}},
                                {const,1}}],
                            {}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT case "
                                    "when a > b then a "
                                    "when a < b then 1 "
                                "end FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{'case',<<>>,
                            [{{'>',{var,<<"a">>},{var,<<"b">>}},
                                {var,<<"a">>}},
                            {{'<',{var,<<"a">>},{var,<<"b">>}},
                                {const,1}}],
                            {const,2}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT case "
                                    "when a > b then a "
                                    "when a < b then 1 "
                                    "else 2 "
                                "end FROM \"abc\"">>)),

        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{'case',<<>>,
                                [{{'>',{var,<<"a">>},{var,<<"b">>}},
                                    {var,<<"a">>}},
                                {{'<',{var,<<"a">>},{var,<<"b">>}},
                                    {const,1}}],
                                {}},
                            {var,<<"d">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT case "
                                    "when a > b then a "
                                    "when a < b then 1 "
                                "end as d FROM \"abc\"">>))
    ].

from_test_() ->
    [
        %% from clause without double quotes is allowed
        ?_assertMatch(
            {ok,{select,
                    [{fields,['*']},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT * FROM abc">>)),

        %% from clause with single quotes is not allowed
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT * FROM 'abc'">>)),

        %% from clause with more than one event is allowed
        ?_assertMatch(
            {ok,{select,
                    [{fields,['*']},
                     {from,[<<"abc">>,<<"e">>,<<"f">>,<<"g">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"SELECT * FROM abc, e, f, g">>)),

        %% from clause events should be separated by commas.
        ?_assertMatch(
            {parse_error, _},
            rulesql:parsetree(<<"SELECT * FROM abc ec">>))
    ].

where_test_() ->
    [
        %% basic where
        ?_assertMatch(
            {ok,{select,
                    [{fields,['*']},
                     {from,[<<"abc">>]},
                     {where,{'=', {const, 1}, {const, 1}}}]}},
            rulesql:parsetree(<<"SELECT * FROM \"abc\" WHERE 1 = 1">>)),

        %% where clause with conditions
        ?_assertMatch(
            {ok,{select,
                    [{fields,['*']},
                     {from,[<<"abc">>]},
                     {where,
                        {'and', {'=', {const, 1}, {const, 1}},
                                {'<', {var, <<"a">>}, {const, 2}}}}
                    ]}},
            rulesql:parsetree(<<"SELECT * FROM \"abc\" "
                                "WHERE 1 = 1 and a < 2"
                              >>)),

        %% where clause with multiple conditions has associativity
        ?_assertMatch(
            {ok,{select,
                    [{fields,['*']},
                     {from,[<<"abc">>]},
                     {where,
                        {'or',
                            {'and', {'=', {const, 1}, {const, 1}},
                                    {'<', {var, <<"a">>}, {const, 2}}},
                            {'!=', {const, 3}, {const, 3}}}}
                    ]}},
            rulesql:parsetree(<<"SELECT * FROM \"abc\" "
                                "WHERE 1 = 1 and a < 2 or 3 != 3"
                              >>)),

        %% force conditions priority using '()'
        ?_assertMatch(
            {ok,{select,
                    [{fields,['*']},
                     {from,[<<"abc">>]},
                     {where,
                        {'and',
                            {'=', {const, 1}, {const, 1}},
                            {'or', {'<', {var, <<"a">>}, {const, 2}},
                                   {'!=', {const, 3}, {const, 3} }}
                        }}
                    ]}},
            rulesql:parsetree(<<"SELECT * FROM \"abc\" "
                                "WHERE 1 = 1 and (a < 2 or 3 != 3)"
                              >>))
    ].

case_when_test_() ->
    [
        %% case when
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{'case',<<>>,
                            [{{'>',{var,<<"a">>},{var,<<"b">>}},
                                {var,<<"a">>}},
                            {{'<=',{var,<<"a">>},{var,<<"b">>}},
                                {var,<<"b">>}}],
                            {}}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT "
                                "  case "
                                "     when a > b then a "
                                "     when a <= b then b "
                                "  end "
                                "FROM abc">>)),
        %% case when else
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{'case',<<>>,
                            [{{'=',{var,<<"a">>},{const,1}},
                                {var,<<"a">>}},
                            {{'=',{var,<<"a">>},{const,2}},
                                {var,<<"a">>}}],
                            {'-',{var,<<"a">>},{const,1}}}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT "
                                "  case "
                                "     when a = 1 then a "
                                "     when a = 2 then a "
                                "     else a-1 "
                                "  end "
                                "FROM abc">>))
    ].

foreach_test_() ->
    [
        %% foreach on a single object
        ?_assertMatch(
            {ok,{foreach,
                    [{fields,[{var,<<"a">>}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"FOREACH a FROM abc">>)),

        %% foreach contains multiple lines
        ?_assertMatch(
            {ok,{foreach,
                    [{fields,
                        [{as,{'fun',
                                {var,<<"range">>},
                                [{const,1},{const,2}]},
                            {var,<<"list">>}},
                        {var,<<"list">>}]},
                    {from,[<<"abc">>]},
                    {where,{'=',{const,1},{const,1}}}]}},
            rulesql:parsetree(<<"FOREACH range(1,2) as list, list FROM \"abc\" WHERE 1 = 1">>))
    ].

foreach_do_test_() ->
    [
        %% foreach do
        ?_assertMatch(
            {ok,{foreach,
                    [{fields,[{var,<<"a">>}]},
                     {do,[{var,<<"clientid">>}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"FOREACH a DO clientid FROM abc">>)),

        %% foreach do contains as
        ?_assertMatch(
            {ok,{foreach,
                    [{fields,[{var,<<"a">>}]},
                     {do,[{as,{path,[{key,<<"item">>},{key,<<"a">>}]},
                              {var,<<"a">>}}]},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"FOREACH a DO item.a as a FROM abc">>))
    ].

foreach_do_incase_test_() ->
    [
        %% foreach incase
        ?_assertMatch(
            {ok,{foreach,[
                    {fields,[{var,<<"a">>}]},
                    {incase,{'<>',{var,<<"a">>},{const,1}}},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"FOREACH a incase a <> 1 FROM abc">>)),

        %% foreach incase multiple conditions
        ?_assertMatch(
            {ok,{foreach,
                    [{fields,[{var,<<"a">>}]},
                     {incase,
                         {'or',
                             {'>',{var,<<"a">>},{const,1.2}},
                             {'<',{var,<<"a">>},{const,0}}}},
                     {from,[<<"abc">>]},
                     {where,{}}]}},
            rulesql:parsetree(<<"FOREACH a incase a > 1.2 or a < 0 FROM abc">>)),

        %% foreach do incase
        ?_assertMatch(
            {ok,{foreach,[{fields,[{var,<<"a">>}]},
                          {do,[{var,<<"a">>}]},
                          {incase,{'<>',{var,<<"a">>},{const,1}}},
                          {from,[<<"abc">>]},
                          {where,{}}]}},
            rulesql:parsetree(<<"FOREACH a DO a INCASE a <> 1 FROM abc">>))

    ].

array_index_head_tail_test_() ->
    [
        %% tail
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{var,<<"e">>},
                             {path,
                                [{key,<<"a">>},
                                 {index,{const,tail}}]}}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT e as a[-0] FROM abc">>)),
        %% heads
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{var,<<"e">>},
                             {path,
                                [{key,<<"a">>},
                                 {index,{const,head}}]}}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT e as a[0] FROM abc">>)),
        %% from end of the array
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as,{var,<<"e">>},
                             {path,
                                [{key,<<"a">>},
                                 {index,{const,-1}}]}}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT e as a[-1] FROM abc">>))
    ].

list_literal_test_() ->
    [
        %% empty list construct
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{list, []}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT [] FROM abc">>)),
        %% simple list construct with single elements
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{list,[{const,1}]}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT [1] FROM abc">>)),
        %% simple list construct with multiple elements
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{list,[{const,1},{const,2},{const,3}]}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT [1,2,3] FROM abc">>)),
        %% simple list construct with multiple elements with as
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{as, {list,[{const,1},{const,2},{const,3}]},
                              {var, <<"list">>}}]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT [1,2,3] as list FROM abc">>)),
        %% list construct with different types of elements
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [{list,
                            [{var,<<"a">>},
                             {const,1},
                             {const,<<"b">>},
                             {list,[{const,3},{const,3.4},{var,<<"e">>}]}
                            ]}
                        ]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT [a,1,'b',[3,3.4,e]] FROM abc">>)),
        %% use list in func
        ?_assertMatch(
            {ok,{select,
                    [{fields,
                        [
                            {'fun',{var,<<"abc">>},[{list,[{const,1}]}]}
                        ]},
                    {from,[<<"abc">>]},
                    {where,{}}]}},
            rulesql:parsetree(<<"SELECT abc([1]) FROM abc">>))
    ].
