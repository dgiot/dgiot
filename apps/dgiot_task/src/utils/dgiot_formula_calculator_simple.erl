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

%% @doc 公式计算器简化模块
-module(dgiot_formula_calculator_simple).

%% API导出
-export([calculate_formula/3]).
-export([extract_variables/1, validate_formula/2]).
-export([replace_variables/2, evaluate_expression/1]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 计算公式
%% @param Collection 公式字符串
%% @param Variables 变量映射
%% @param Prop 属性配置
%% @return 计算结果或undefined
calculate_formula(Collection, Variables, Prop) ->
    try
        %% 1. 验证公式
        case validate_formula(Collection, Variables) of
            {ok, ValidVariables} ->
                %% 2. 替换变量
                Expression = replace_variables(Collection, ValidVariables),
                
                %% 3. 执行计算
                Result = evaluate_expression(Expression),
                
                %% 4. 应用类型转换
                apply_type_conversion(Result, Prop);
            {error, _Reason} ->
                undefined
        end
    catch
        _:_ ->
            undefined
    end.

%% @doc 提取公式中的变量
%% @param Collection 公式字符串
%% @return 变量列表
extract_variables(Collection) when is_binary(Collection) ->
    extract_variables(binary_to_list(Collection));
extract_variables(Collection) when is_list(Collection) ->
    %% 匹配 %%{variable_name} 格式
    Pattern = "%%\\{([^}]+)\\}",
    case re:run(Collection, Pattern, [global, {capture, all, list}]) of
        {match, Matches} ->
            %% 提取变量名并去重
            Vars = lists:map(fun([_, Var]) -> list_to_binary(Var) end, Matches),
            lists:usort(Vars);
        nomatch ->
            []
    end.

%% @doc 验证公式和变量
%% @param Collection 公式字符串
%% @param Variables 变量映射
%% @return {ok, ValidVariables} | {error, Reason}
validate_formula(Collection, Variables) ->
    %% 1. 提取变量
    RequiredVars = extract_variables(Collection),
    
    %% 2. 检查变量是否存在
    MissingVars = lists:filter(fun(Var) ->
        not maps:is_key(Var, Variables)
    end, RequiredVars),
    
    case MissingVars of
        [] ->
            %% 3. 提取有效变量值
            ValidVariables = maps:with(RequiredVars, Variables),
            {ok, ValidVariables};
        _ ->
            {error, {missing_variables, MissingVars}}
    end.

%% @doc 替换变量
replace_variables(Collection, Variables) when is_binary(Collection) ->
    replace_variables(binary_to_list(Collection), Variables);
replace_variables(Collection, Variables) when is_list(Collection) ->
    lists:foldl(fun({VarName, VarValue}, Acc) ->
        Pattern = "%%\\{" ++ binary_to_list(VarName) ++ "\\}",
        Replacement = value_to_string(VarValue),
        re:replace(Acc, Pattern, Replacement, [global, {return, list}])
    end, Collection, maps:to_list(Variables)).

%% @doc 计算表达式
evaluate_expression(Expression) ->
    try
        {ok, Tokens, _} = erl_scan:string(Expression ++ "."),
        {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
        Bindings = erl_eval:new_bindings(),
        {value, Value, _} = erl_eval:expr(Expr, Bindings),
        Value
    catch
        _:_ -> 
            undefined
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 应用类型转换
apply_type_conversion(Value, Prop) ->
    case Prop of
        #{<<"dataType">> := #{<<"type">> := Type, <<"specs">> := Specs}} ->
            apply_type_conversion_impl(Value, Type, Specs);
        #{<<"dataType">> := #{<<"type">> := Type}} ->
            apply_type_conversion_impl(Value, Type, #{});
        _ ->
            Value
    end.

%% @doc 类型转换实现
apply_type_conversion_impl(Value, <<"INT">>, _Specs) ->
    round(Value);
apply_type_conversion_impl(Value, <<"FLOAT">>, Specs) ->
    Precision = maps:get(<<"precision">>, Specs, 3),
    round_to_precision(Value, Precision);
apply_type_conversion_impl(Value, <<"DOUBLE">>, Specs) ->
    Precision = maps:get(<<"precision">>, Specs, 6),
    round_to_precision(Value, Precision);
apply_type_conversion_impl(Value, <<"TEXT">>, _Specs) ->
    value_to_binary(Value);
apply_type_conversion_impl(Value, _Type, _Specs) ->
    Value.

%% @doc 四舍五入到指定精度
round_to_precision(Value, Precision) when is_integer(Precision) ->
    Multiplier = math:pow(10, Precision),
    round(Value * Multiplier) / Multiplier.

%% @doc 值转字符串
value_to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
value_to_string(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 6}, compact]);
value_to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
value_to_string(Value) when is_list(Value) ->
    Value;
value_to_string(Value) ->
    lists:flatten(io_lib:format("~p", [Value])).

%% @doc 值转二进制
value_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
value_to_binary(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 6}, compact]);
value_to_binary(Value) when is_binary(Value) ->
    Value;
value_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
value_to_binary(Value) ->
    list_to_binary(lists:flatten(io_lib:format("~p", [Value]))).
