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

-module(modbus_util).
-export([
    binary_to_coils/1,
    binary_to_int16/1,
    binary_to_int16s/1,
    binary_to_int32/1,
    binary_to_int32s/1,
    binary_to_float32/1,
    binary_to_ascii/1,
    coils_to_binary/1,
    int16_to_binary/1,
    get_header/1,
    get_product_name/1,
    get_product_id/2,
    get_category_id/0,
    convert_pattern/1,
    convert_pattern_list/3,
    create_regex_part/1,
    find_product/2
]).

%% @doc Function to convert bytes to coils.
%% @end
-spec binary_to_coils(Bin::binary()) -> [0|1].
binary_to_coils(Bin) ->
    lists:append([ lists:reverse([ Y || <<Y:1>> <= <<X>>]) || <<X:8>> <= Bin]).

%% @doc Function to convert bytes to 16bits integer.
%% @end
-spec binary_to_int16(Bin::binary()) -> [integer()].
binary_to_int16(Bin) ->
    [ X || <<X:16/integer>> <= Bin ].

%% @doc Function to convert bytes to 16bits signed integer.
%% @end
-spec binary_to_int16s(Bin::binary()) -> [integer()].
binary_to_int16s(Bin) ->
    [ X || <<X:16/signed-integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits integer.
%% @end
-spec binary_to_int32(Bin::binary()) -> [integer()].
binary_to_int32(Bin) ->
    [ X || <<X:32/integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits signed integer.
%% @end
-spec binary_to_int32s(Bin::binary()) -> [integer()].
binary_to_int32s(Bin) ->
    [ X || <<X:32/signed-integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits float number.
%% @end
-spec binary_to_float32(Bin::binary()) -> [float()].
binary_to_float32(Bin) ->
    [ X || <<X:32/float>> <= Bin ].

%% @doc Function to convert bytes to ASCII.
%% @end
-spec binary_to_ascii(Bin::binary()) -> list().
binary_to_ascii(Bin) ->
    erlang:binary_to_list(Bin).

%% @doc Function to convert a list of coils to binary.
%% @end
-spec coils_to_binary(Values::list()) -> binary().
coils_to_binary(Values) ->
    coils_to_binary(Values, <<>>).

coils_to_binary([], Acc) ->
    Acc;
coils_to_binary([B0, B1, B2, B3, B4, B5, B6, B7 | T], Acc) ->
    coils_to_binary(T, <<Acc/binary, B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>);
coils_to_binary(Values, Acc) ->
    coils_to_binary(Values ++ [0], Acc).

%% @doc Function to convert a list of 16bits integer to binary.
%% @end
-spec int16_to_binary(Values::list()) -> binary().
int16_to_binary(Values) ->
    << <<X:16>> || X <- Values >>.

%% @doc 获取头部模式和长度
%% @spec get_header(Regular) -> {Header, Length}
get_header(Regular) ->
    lists:foldl(fun(X, {Header, Len}) ->
        case X of
            "**" -> {Header ++ X, Len + length(X)};
            "*" -> {Header ++ X, Len + length(X)};
            _ -> {Header ++ X, Len + length(X)}
        end
    end, {[], 0}, re:split(dgiot_utils:to_list(Regular), "-", [{return, list}])).

%% @doc 获取产品名称
%% @spec get_product_name(ProductId) -> ProductName
get_product_name(ProductId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"name">> := ProductName}} -> ProductName;
        _ -> <<"">>
    end.

%% @doc 根据产品名称和设备类型计算产品ID
%% @spec get_product_id(ProductName, DevType) -> ProductId
%% ProductName :: binary()
%% DevType :: binary()
%% ProductId :: binary()
get_product_id(ProductName, DevType) ->
    % 从配置读取分类ID
    CategoryId = get_category_id(),
    % 计算产品ID
    dgiot_parse_id:get_productid(CategoryId, DevType, ProductName).

%% @doc 从应用配置中获取DGIoT网关分类ID
%% 配置项: modbus.category
%% 默认值: <<"5ca6049839">>
%% @spec get_category_id() -> CategoryId
%% CategoryId :: binary()
get_category_id() ->
    case application:get_env(dgiot_modbus, category) of
        {ok, CategoryId} when is_binary(CategoryId) ->
            CategoryId;
        {ok, CategoryId} when is_list(CategoryId) ->
            list_to_binary(CategoryId);
        _ ->
            % 默认值
            <<"5ca6049839">>
    end.

%% @doc 将通配符模式转换为正则表达式模式
%% 支持的通配符：* 匹配任意字符序列
%% @spec convert_pattern(Pattern) -> ConvertedPattern
%% Pattern :: binary() | list()
%% ConvertedPattern :: binary()
convert_pattern(Pattern) when is_binary(Pattern) ->
    PatternList = binary_to_list(Pattern),
    ConvertedList = convert_pattern_list(PatternList, [], []),
    list_to_binary(ConvertedList);
convert_pattern(Pattern) when is_list(Pattern) ->
    ConvertedList = convert_pattern_list(Pattern, [], []),
    list_to_binary(ConvertedList).

%% @doc 修正后的递归处理函数 - 保持正确的字符顺序
%% @private
convert_pattern_list([], Acc, []) ->
    %% 直接返回Acc，不进行reverse
    Acc;
convert_pattern_list([], Acc, StarAcc) ->
    %% 处理末尾的星号序列
    RegexPart = create_regex_part(StarAcc),
    %% 直接拼接，保持顺序
    Acc ++ RegexPart;
convert_pattern_list([$* | Rest], Acc, StarAcc) ->
    %% 遇到星号，添加到星号累加器
    convert_pattern_list(Rest, Acc, [$* | StarAcc]);
convert_pattern_list([Char | Rest], Acc, []) ->
    %% 普通字符，没有待处理的星号
    convert_pattern_list(Rest, Acc ++ [Char], []);
convert_pattern_list([Char | Rest], Acc, StarAcc) ->
    %% 遇到普通字符，但有待处理的星号序列
    RegexPart = create_regex_part(StarAcc),
    %% 关键修正：先添加正则表达式部分，再添加当前字符
    convert_pattern_list(Rest, Acc ++ RegexPart ++ [Char], []).

%% @doc 创建正则表达式部分
%% @private
create_regex_part(StarAcc) ->
    Count = length(StarAcc),
    if
        Count > 0 ->
            % 允许字母、数字、连字符、下划线等常见字符
            "[a-zA-Z0-9\\-_]{" ++ integer_to_list(Count) ++ "}";
        true ->
            ""
    end.

%% @doc 根据DTU头查找产品
%% @spec find_product(DtuHeader, Products) -> ProductItem | not_found
%% DtuHeader :: binary()
%% Products :: list()
%% ProductItem :: map()
find_product(_DtuHeader, []) -> not_found;
find_product(DtuHeader, [OuterMap | Tail]) ->
    % 使用 maps:to_list/1 将外部Map转换为键值对列表，然后提取第一个（也是唯一一个）键值对
    case OuterMap of
        {ProductId, _DetailMap} ->
            case dgiot_product:local(ProductId) of
                {ok, ProductItem} ->
                    case ProductItem of
                        #{<<"content">> := #{<<"head">> := TmpHeader}} ->
                            {Header, Len} = get_header(TmpHeader),
                            ReHeader = convert_pattern(Header),
                            
                            case re:run(DtuHeader, ReHeader, [{capture, first, list}]) of
                                {match, [_DtuAddr]} when byte_size(DtuHeader) =:= Len ->
                                    ProductItem;  % 匹配成功，返回整个ProductItem
                                _ ->
                                    find_product(DtuHeader, Tail)  % 不匹配，继续遍历尾部
                            end;
                        _ ->
                            find_product(DtuHeader, Tail)  % 不匹配，继续遍历尾部
                    end;
                _ ->
                    find_product(DtuHeader, Tail)  % 找不到合适的ProductItem，继续遍历尾部
            end;
        _ ->
            not_found
    end;

find_product(_, _) ->
    not_found.
