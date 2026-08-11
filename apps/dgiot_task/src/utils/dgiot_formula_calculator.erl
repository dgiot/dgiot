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

%% @doc 公式计算器模块
%% 增强的公式计算能力，支持多变量、条件表达式、数据块访问等高级功能
-module(dgiot_formula_calculator).

%% API导出
-export([calculate_formula/3, calculate_formula/4]).
-export([extract_variables/1, validate_formula/2]).
-export([precompile_formula/1, batch_calculate/2]).
-export([register_custom_function/3, call_custom_function/2]).

%% 内部函数导出（用于测试）
-export([replace_variables/2, evaluate_expression/1]).

%% 私有函数（用于模块内部）
-export([init_predefined_functions/0, round_to_precision/2, average/1, if_then_else/3, start/0, stop/0]).

-include_lib("dgiot/include/logger.hrl").

%% 自定义函数表
-define(CUSTOM_FUNCTIONS_TABLE, custom_functions_table).

%% 公式缓存表
-define(FORMULA_CACHE_TABLE, formula_cache_table).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 计算公式（基础版本）
%% @param Collection 公式字符串
%% @param Variables 变量映射
%% @param Prop 属性配置
%% @return 计算结果或undefined
calculate_formula(Collection, Variables, Prop) ->
    calculate_formula(Collection, Variables, Prop, #{}).

%% @doc 计算公式（完整版本）
%% @param Collection 公式字符串
%% @param Variables 变量映射
%% @param Prop 属性配置
%% @param Options 选项（如precision, timeout等）
%% @return 计算结果或undefined
calculate_formula(Collection, Variables, Prop, Options) ->
    try
        %% 1. 检查是否已预编译
        case get_cached_formula(Collection) of
            undefined ->
                %% 2. 提取变量并验证
                case validate_formula(Collection, Variables) of
                    {ok, ValidVariables} ->
                        %% 3. 替换变量
                        Expression = replace_variables(Collection, ValidVariables),
                        
                        %% 4. 执行计算
                        Result = evaluate_expression_with_options(Expression, Options),
                        
                        %% 5. 应用精度和类型转换
                        apply_precision_and_type(Result, Prop, Options);
                    {error, ValidationReason} ->
                        ?LOG(error, "公式验证失败: ~p, 公式: ~p", [ValidationReason, Collection]),
                        undefined
                end;
            CachedFun ->
                %% 使用预编译函数
                Result = apply_cached_function(CachedFun, Variables),
                apply_precision_and_type(Result, Prop, Options)
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG(error, "公式计算异常: Class=~p, Reason=~p, Stacktrace=~p, Formula=~p", 
                 [Class, Reason, Stacktrace, Collection]),
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
            lists:map(fun([_, Var]) -> list_to_binary(Var) end, Matches);
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

%% @doc 预编译公式
%% @param Collection 公式字符串
%% @return {ok, CompiledFun} | {error, Reason}
precompile_formula(Collection) ->
    try
        %% 1. 提取变量
        Variables = extract_variables(Collection),
        
        %% 2. 创建函数体
        FunBody = create_function_body(Collection, Variables),
        
        %% 3. 编译函数
        {ok, Tokens, _} = erl_scan:string(FunBody),
        {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
        
        %% 4. 创建函数
        CompiledFun = fun(Vars) ->
            Bindings = create_bindings(Vars, Variables),
            {value, Value, _} = erl_eval:expr(Expr, Bindings),
            Value
        end,
        
        %% 5. 缓存函数
        cache_formula(Collection, CompiledFun),
        {ok, CompiledFun}
    catch
        Class:Reason:Stacktrace ->
            ?LOG(error, "公式预编译失败: Class=~p, Reason=~p, Stacktrace=~p, Formula=~p", 
                 [Class, Reason, Stacktrace, Collection]),
            {error, {compilation_failed, Reason}}
    end.

%% @doc 批量计算公式
%% @param Formulas 公式列表 [{Identifier, Collection}]
%% @param Variables 变量映射
%% @return 计算结果映射
batch_calculate(Formulas, Variables) ->
    %% 1. 提取所有变量
    AllVariables = lists:foldl(fun({_, Collection}, Acc) ->
        Vars = extract_variables(Collection),
        sets:union(Acc, sets:from_list(Vars))
    end, sets:new(), Formulas),
    
    %% 2. 批量获取变量值
    VariableValues = get_batch_variable_values(
        sets:to_list(AllVariables), 
        Variables
    ),
    
    %% 3. 批量计算
    lists:foldl(fun({Identifier, Collection}, Acc) ->
        case calculate_formula(Collection, VariableValues, #{}) of
            undefined -> Acc;
            Value -> Acc#{Identifier => Value}
        end
    end, #{}, Formulas).

%% @doc 注册自定义函数
%% @param Name 函数名
%% @param Arity 参数个数
%% @param Fun 函数实现
%% @return ok
register_custom_function(Name, Arity, Fun) ->
    dgiot_data:insert(?CUSTOM_FUNCTIONS_TABLE, {Name, Arity}, Fun).

%% @doc 调用自定义函数
%% @param Name 函数名
%% @param Args 参数列表
%% @return 函数结果或{error, Reason}
call_custom_function(Name, Args) ->
    case dgiot_data:get(?CUSTOM_FUNCTIONS_TABLE, {Name, length(Args)}) of
        not_find -> {error, function_not_found};
        Fun -> 
            try
                apply(Fun, Args)
            catch
                Class:Reason:Stacktrace ->
                    ?LOG(error, "自定义函数调用失败: Name=~p, Args=~p, Error=~p", 
                         [Name, Args, {Class, Reason, Stacktrace}]),
                    {error, function_execution_failed}
            end
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 替换变量
replace_variables(Collection, Variables) when is_binary(Collection) ->
    replace_variables(binary_to_list(Collection), Variables);
replace_variables(Collection, Variables) when is_list(Collection) ->
    lists:foldl(fun({VarName, VarValue}, Acc) ->
        Pattern = "%%\\{" ++ binary_to_list(VarName) ++ "\\}",
        Replacement = dgiot_utils:to_list(VarValue),
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
            ?LOG(error, "表达式计算失败: ~p", [Expression]),
            undefined
    end.

%% @doc 带选项的表达式计算
evaluate_expression_with_options(Expression, Options) ->
    Timeout = maps:get(timeout, Options, 5000),  %% 默认5秒超时
    
    try
        Parent = self(),
        Pid = spawn_link(fun() ->
            Result = evaluate_expression(Expression),
            Parent ! {self(), Result}
        end),
        
        receive
            {Pid, Result} -> Result
        after Timeout ->
            exit(Pid, kill),
            ?LOG(error, "公式计算超时: ~p", [Expression]),
            undefined
        end
    catch
        _:_ -> undefined
    end.

%% @doc 应用精度和类型转换
apply_precision_and_type(Value, Prop, Options) ->
    case Prop of
        #{<<"dataType">> := #{<<"type">> := Type, <<"specs">> := Specs}} ->
            apply_type_conversion(Value, Type, Specs, Options);
        _ ->
            %% 默认使用选项中的精度
            Precision = maps:get(precision, Options, 3),
            dgiot_utils:to_float(Value, Precision)
    end.

%% @doc 应用类型转换
apply_type_conversion(Value, <<"INT">>, _Specs, _Options) ->
    round(Value);
apply_type_conversion(Value, <<"FLOAT">>, Specs, _Options) ->
    Precision = maps:get(<<"precision">>, Specs, 3),
    dgiot_utils:to_float(Value, Precision);
apply_type_conversion(Value, <<"DOUBLE">>, Specs, _Options) ->
    Precision = maps:get(<<"precision">>, Specs, 6),
    dgiot_utils:to_float(Value, Precision);
apply_type_conversion(Value, <<"TEXT">>, _Specs, _Options) ->
    dgiot_utils:to_binary(Value);
apply_type_conversion(Value, _Type, _Specs, Options) ->
    Precision = maps:get(precision, Options, 3),
    dgiot_utils:to_float(Value, Precision).

%% @doc 创建函数体
create_function_body(Collection, Variables) ->
    %% 将变量名转换为Erlang变量名
    VarNames = lists:map(fun(Var) ->
        binary_to_atom(Var, utf8)
    end, Variables),
    
    %% 创建参数列表
    ArgsStr = string:join(lists:map(fun atom_to_list/1, VarNames), ", "),
    
    %% 替换变量引用
    Body = lists:foldl(fun(Var, Acc) ->
        Pattern = "%%\\{" ++ binary_to_list(Var) ++ "\\}",
        Replacement = atom_to_list(binary_to_atom(Var, utf8)),
        re:replace(Acc, Pattern, Replacement, [global, {return, list}])
    end, Collection, Variables),
    
    %% 构建函数定义
    "fun(" ++ ArgsStr ++ ") -> " ++ Body ++ " end".

%% @doc 创建绑定
create_bindings(Vars, VariableNames) ->
    lists:foldl(fun(VarName, Bindings) ->
        VarAtom = binary_to_atom(VarName, utf8),
        VarValue = maps:get(VarName, Vars, 0),
        erl_eval:add_binding(VarAtom, VarValue, Bindings)
    end, erl_eval:new_bindings(), VariableNames).

%% @doc 获取缓存的公式
get_cached_formula(Collection) ->
    dgiot_data:get(?FORMULA_CACHE_TABLE, Collection).

%% @doc 缓存公式
cache_formula(Collection, CompiledFun) ->
    dgiot_data:insert(?FORMULA_CACHE_TABLE, Collection, CompiledFun).

%% @doc 应用缓存的函数
apply_cached_function(CompiledFun, Variables) ->
    try
        CompiledFun(Variables)
    catch
        _:_ -> undefined
    end.

%% @doc 批量获取变量值
get_batch_variable_values(VariableNames, Variables) ->
    lists:foldl(fun(VarName, Acc) ->
        case maps:get(VarName, Variables, undefined) of
            undefined -> Acc;
            Value -> Acc#{VarName => Value}
        end
    end, #{}, VariableNames).

%%%===================================================================
%%% 预定义的自定义函数
%%%===================================================================

%% @doc 初始化预定义函数
init_predefined_functions() ->
    %% 数学函数
    register_custom_function(<<"sqrt">>, 1, fun math:sqrt/1),
    register_custom_function(<<"abs">>, 1, fun erlang:abs/1),
    register_custom_function(<<"round">>, 2, fun round_to_precision/2),
    register_custom_function(<<"floor">>, 1, fun math:floor/1),
    register_custom_function(<<"ceil">>, 1, fun math:ceil/1),
    
    %% 统计函数
    register_custom_function(<<"avg">>, 1, fun average/1),
    register_custom_function(<<"sum">>, 1, fun lists:sum/1),
    register_custom_function(<<"min">>, 1, fun lists:min/1),
    register_custom_function(<<"max">>, 1, fun lists:max/1),
    
    %% 转换函数
    register_custom_function(<<"celsius_to_fahrenheit">>, 1, 
        fun(Celsius) -> Celsius * 1.8 + 32 end),
    register_custom_function(<<"fahrenheit_to_celsius">>, 1,
        fun(Fahrenheit) -> (Fahrenheit - 32) / 1.8 end),
    
    %% 条件函数
    register_custom_function(<<"if">>, 3, fun if_then_else/3),
    
    ok.

%% @doc 四舍五入到指定精度
round_to_precision(Value, Precision) when is_integer(Precision) ->
    Multiplier = math:pow(10, Precision),
    round(Value * Multiplier) / Multiplier.

%% @doc 计算平均值
average(List) when is_list(List) ->
    case List of
        [] -> 0;
        _ -> lists:sum(List) / length(List)
    end.

%% @doc 条件函数
if_then_else(Condition, Then, Else) ->
    case Condition of
        true -> Then;
        false -> Else;
        _ when Condition > 0 -> Then;
        _ -> Else
    end.

%%%===================================================================
%%% 模块初始化
%%%===================================================================

%% @doc 模块启动
start() ->
    init_predefined_functions(),
    ok.

%% @doc 模块停止
stop() ->
    ok.
