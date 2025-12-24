# TDengine子表Tag更新示例

## 概述

本示例展示如何使用`dgiot_tdengine:update_table_tags/3`和`dgiot_tdengine:parse_relation_list/1`函数来更新TDengine子表的tag。

## 函数说明

### 1. parse_relation_list/1
解析relationList，提取area1, area2, area3的值。

```erlang
%% @doc 解析relationList，提取area1, area2, area3的值
%% @spec parse_relation_list(list()) -> #{area1 => binary(), area2 => binary(), area3 => binary()} | {error, Reason}
parse_relation_list(RelationList) when is_list(RelationList) ->
    parse_relation_list(RelationList, #{});
parse_relation_list(_) ->
    {error, invalid_relation_list}.
```

### 2. update_table_tags/3 和 update_table_tags/4
更新子表的tag。

```erlang
%% @doc 更新子表的tag（使用默认通道）
%% @spec update_table_tags(ProductId, DeviceId, Tags) -> {ok, Result} | {error, Reason}
update_table_tags(ProductId, DeviceId, Tags) ->
    update_table_tags(?DEFAULT, ProductId, DeviceId, Tags).

%% @doc 更新子表的tag
%% @spec update_table_tags(Channel, ProductId, DeviceId, Tags) -> {ok, Result} | {error, Reason}
update_table_tags(Channel, ProductId, DeviceId, Tags) when is_map(Tags) ->
    transaction(Channel,
        fun(Context) ->
            DB = dgiot_tdengine:get_database(Channel, ProductId),
            TableName = ?Table(DeviceId),
            
            % 构建SET TAG子句
            SetClauses = build_set_tag_clauses(Tags),
            case SetClauses of
                <<>> -> 
                    {error, <<"No valid tags to update">>};
                _ ->
                    Sql = <<"ALTER TABLE ", DB/binary, TableName/binary, " SET TAG ", SetClauses/binary, ";">>,
                    io:format("~s ~p Executing SQL: ~p~n", [?FILE, ?LINE, Sql]),
                    dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
            end
        end).
```

## 使用示例

### 示例1：解析relationList

```erlang
% 原始relationList数据
RelationList = [
    #{<<"id">> => 3, <<"type">> => <<"area3">>, <<"value">> => <<"活动区">>},
    #{<<"id">> => 2, <<"type">> => <<"area2">>, <<"value">> => <<"活动区">>},
    #{<<"id">> => 1, <<"type">> => <<"area1">>, <<"value">> => <<"B04">>},
    #{<<"id">> => 0, <<"type">> => <<"algorithm">>, <<"value">> => <<"跌倒">>}
],

% 解析relationList
case dgiot_tdengine:parse_relation_list(RelationList) of
    {ok, Tags} ->
        io:format("Parsed tags: ~p~n", [Tags]);
        % Tags = #{<<"area1">> => <<"B04">>, <<"area2">> => <<"活动区">>, <<"area3">> => <<"活动区">>}
    {error, Reason} ->
        io:format("Error parsing relation list: ~p~n", [Reason])
end.
```

### 示例2：更新TDengine子表tag

```erlang
% 假设参数
ProductId = <<"product_123">>,
DeviceId = <<"device_456">>,
Tags = #{<<"area1">> => <<"B04">>, <<"area2">> => <<"活动区">>, <<"area3">> => <<"活动区">>},

% 使用默认通道更新tag
case dgiot_tdengine:update_table_tags(ProductId, DeviceId, Tags) of
    {ok, Result} ->
        io:format("Tag update successful: ~p~n", [Result]);
    {error, Reason} ->
        io:format("Tag update failed: ~p~n", [Reason])
end.

% 或者使用特定通道
Channel = <<"td_channel_1">>,
case dgiot_tdengine:update_table_tags(Channel, ProductId, DeviceId, Tags) of
    {ok, Result} ->
        io:format("Tag update successful: ~p~n", [Result]);
    {error, Reason} ->
        io:format("Tag update failed: ~p~n", [Reason])
end.
```

### 示例3：完整流程 - 从relationList到TDengine更新

```erlang
% 完整流程示例
update_device_tags(ProductId, DeviceId, RelationList) ->
    % 1. 解析relationList
    case dgiot_tdengine:parse_relation_list(RelationList) of
        {ok, Tags} ->
            io:format("Parsed tags: ~p~n", [Tags]),
            
            % 2. 更新TDengine子表tag
            case dgiot_tdengine:update_table_tags(ProductId, DeviceId, Tags) of
                {ok, Result} ->
                    io:format("Tag update successful: ~p~n", [Result]),
                    {ok, #{tags => Tags, result => Result}};
                {error, Reason} ->
                    io:format("TDengine update failed: ~p~n", [Reason]),
                    {error, {tdengine_update_failed, Reason}}
            end;
        {error, Reason} ->
            io:format("Failed to parse relation list: ~p~n", [Reason]),
            {error, {parse_failed, Reason}}
    end.
```

## 生成的SQL示例

函数会生成类似以下的SQL语句：

```sql
ALTER TABLE _product_123._device_456 SET TAG area1='B04', area2='活动区', area3='活动区';
```

## 注意事项

1. **Tag字段名**：函数目前只处理area1、area2、area3这三个tag字段。如果需要其他tag字段，可以扩展`build_set_tag_clauses/1`函数。

2. **空值处理**：函数会跳过值为undefined或空二进制(<<>>)的tag字段。

3. **错误处理**：
   - 如果relationList中没有找到任何area tag，会返回错误`{error, <<"No area tags found">>}`
   - 如果Tags映射中没有有效的tag字段，会返回错误`{error, <<"No valid tags to update">>}`

4. **日志输出**：函数执行时会输出SQL语句到日志，便于调试。

## 扩展建议

如果需要支持更多tag字段，可以修改`parse_relation_list/1`和`build_set_tag_clauses/1`函数：

```erlang
% 在parse_relation_list/1中添加更多字段
case Type of
    <<"area1">> -> parse_relation_list(Rest, Acc#{<<"area1">> => Value});
    <<"area2">> -> parse_relation_list(Rest, Acc#{<<"area2">> => Value});
    <<"area3">> -> parse_relation_list(Rest, Acc#{<<"area3">> => Value});
    <<"area4">> -> parse_relation_list(Rest, Acc#{<<"area4">> => Value}); % 新增
    <<"area5">> -> parse_relation_list(Rest, Acc#{<<"area5">> => Value}); % 新增
    _ -> parse_relation_list(Rest, Acc)
end.

% 在build_set_tag_clauses/1中添加更多字段
build_set_tag_clauses([{<<"area4">>, Value} | Rest], Acc) when Value =/= undefined, Value =/= <<>> ->
    NewAcc = case Acc of
        <<>> -> <<"area4='", Value/binary, "'">>;
        _ -> <<Acc/binary, ", area4='", Value/binary, "'">>
    end,
    build_set_tag_clauses(Rest, NewAcc);
build_set_tag_clauses([{<<"area5">>, Value} | Rest], Acc) when Value =/= undefined, Value =/= <<>> ->
    NewAcc = case Acc of
        <<>> -> <<"area5='", Value/binary, "'">>;
        _ -> <<Acc/binary, ", area5='", Value/binary, "'">>
    end,
    build_set_tag_clauses(Rest, NewAcc);
```

## 总结

通过`dgiot_tdengine:update_table_tags/3`和`dgiot_tdengine:parse_relation_list/1`函数，可以方便地从relationList中提取tag信息并更新TDengine子表的tag，实现了设备区域信息的动态更新。
