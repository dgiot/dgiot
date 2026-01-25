# DGIOT对象管理技巧总结

## 概述

基于对`dgiot_parse:create_object`的全面搜索和分析，本技能总结了DGIOT系统中对象管理的最佳实践和技巧。涵盖了用户管理、会话管理、设备管理、测试数据管理等各个方面。

## 核心发现

### 1. `dgiot_parse:create_object`使用模式分析

通过搜索46个使用案例，发现以下主要使用模式：

#### 模式1：基础对象创建
```erlang
%% 简单对象创建
dgiot_parse:create_object(<<"Class">>, #{<<"field">> => Value})

%% 带objectId的对象创建
dgiot_parse:create_object(<<"Class">>, #{<<"objectId">> => Id, <<"field">> => Value})
```

#### 模式2：关联对象创建
```erlang
%% 创建关联对象（Pointer类型）
dgiot_parse:create_object(<<"Class">>, #{
    <<"related_field">> => #{
        <<"__type">> => <<"Pointer">>,
        <<"className">> => <<"RelatedClass">>,
        <<"objectId">> => RelatedId
    }
})
```

#### 模式3：批量对象创建
```erlang
%% 使用batch进行批量创建
Requests = [
    #{<<"method">> => <<"POST">>, <<"path">> => <<"/classes/Class1">>, <<"body">> => Data1},
    #{<<"method">> => <<"POST">>, <<"path">> => <<"/classes/Class2">>, <<"body">> => Data2}
],
dgiot_parse:batch(Requests)
```

### 2. 主要对象类别分析

#### 2.1 用户管理对象 (`_User`, `_Role`, `_Session`)
```erlang
%% 创建用户
dgiot_parse:create_object(<<"_User">>, #{
    <<"username">> => UserName,
    <<"password">> => Password,
    <<"email">> => Email,
    <<"phone">> => Phone
})

%% 创建角色
dgiot_parse:create_object(<<"_Role">>, #{
    <<"name">> => RoleName,
    <<"ACL">> => ACL,
    <<"users">> => UsersRelation
})

%% 创建会话
dgiot_parse:create_object(<<"_Session">>, #{
    <<"sessionToken">> => Token,
    <<"user">> => UserPointer,
    <<"expiresAt">> => ExpiryDate
})
```

#### 2.2 设备管理对象 (`Device`, `Product`)
```erlang
%% 创建设备
dgiot_parse:create_object(<<"Device">>, #{
    <<"devaddr">> => DeviceAddr,
    <<"product">> => ProductPointer,
    <<"status">> => <<"offline">>,
    <<"ACL">> => DeviceACL
})

%% 创建产品
dgiot_parse:create_object(<<"Product">>, #{
    <<"name">> => ProductName,
    <<"devType">> => DeviceType,
    <<"dynamicReg">> => true
})
```

#### 2.3 测试数据对象 (`UAVTestTask`, `UAVTestStep`, `UAVTestIndication`)
```erlang
%% 创建测试任务
dgiot_parse:create_object(<<"UAVTestTask">>, #{
    <<"device_id">> => DeviceId,
    <<"operator">> => Operator,
    <<"status">> => <<"not_started">>,
    <<"config">> => Config
})

%% 创建测试步骤
dgiot_parse:create_object(<<"UAVTestStep">>, #{
    <<"task_id">> => TaskPointer,
    <<"step_id">> => StepNumber,
    <<"name">> => StepName,
    <<"status">> => <<"pending">>
})

%% 创建测试指标
dgiot_parse:create_object(<<"UAVTestIndication">>, #{
    <<"step_id">> => StepPointer,
    <<"indication_id">> => IndicationNumber,
    <<"name">> => IndicationName,
    <<"qualified">> => false
})
```

#### 2.4 系统管理对象 (`Dict`, `Menu`, `Permission`, `Notification`)
```erlang
%% 创建字典
dgiot_parse:create_object(<<"Dict">>, #{
    <<"type">> => DictType,
    <<"key">> => DictKey,
    <<"value">> => DictValue
})

%% 创建菜单
dgiot_parse:create_object(<<"Menu">>, #{
    <<"name">> => MenuName,
    <<"path">> => MenuPath,
    <<"icon">> => MenuIcon
})

%% 创建权限
dgiot_parse:create_object(<<"Permission">>, #{
    <<"name">> => PermissionName,
    <<"description">> => Description,
    <<"rules">> => Rules
})

%% 创建通知
dgiot_parse:create_object(<<"Notification">>, #{
    <<"title">> => Title,
    <<"content">> => Content,
    <<"recipients">> => Recipients
})
```

## 最佳实践技巧

### 1. 对象ID管理技巧

#### 技巧1：自动生成objectId
```erlang
%% 使用dgiot_parse_id模块生成标准ID
ObjectId = dgiot_parse_id:get_objectid(Class, Data)
DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr)
SessionId = dgiot_parse_id:get_sessionId(SessionToken)
```

#### 技巧2：自定义objectId
```erlang
%% 当需要特定ID格式时
ObjectId = <<"custom_prefix_", Timestamp/binary, "_", Random/binary>>
dgiot_parse:create_object(<<"Class">>, #{<<"objectId">> => ObjectId, ...})
```

#### 技巧3：ID存在性检查
```erlang
%% 创建前检查对象是否已存在
case dgiot_parse:get_object(Class, ObjectId) of
    {ok, _} -> {error, already_exists};
    {error, _} -> create_new_object(Class, ObjectId, Data)
end
```

### 2. 时间戳管理技巧

#### 技巧1：统一时间格式
```erlang
%% 使用dgiot_datetime模块获取标准时间
Now = dgiot_datetime:now_secs()
NowISO = dgiot_datetime:format(Now, <<"YY-MM-DDTHH:NN:SS.000Z">>)

Data = #{
    <<"createdAt">> => Now,
    <<"updatedAt">> => Now,
    <<"expiresAt">> => #{
        <<"__type">> => <<"Date">>,
        <<"iso">> => NowISO
    }
}
```

#### 技巧2：时间字段标准化
```erlang
%% 推荐的时间字段命名
Data = #{
    <<"createdat">> => CreatedAt,    %% 创建时间（小写，无分隔）
    <<"updatedat">> => UpdatedAt,    %% 更新时间
    <<"start_time">> => StartTime,   %% 开始时间
    <<"end_time">> => EndTime,       %% 结束时间
    <<"last_modified">> => LastModified  %% 最后修改时间
}
```

### 3. 关联关系管理技巧

#### 技巧1：Pointer类型关联
```erlang
%% 创建Pointer关联
Pointer = #{
    <<"__type">> => <<"Pointer">>,
    <<"className">> => ClassName,
    <<"objectId">> => ObjectId
}

dgiot_parse:create_object(<<"Class">>, #{<<"related_field">> => Pointer})
```

#### 技巧2：Relation类型关联
```erlang
%% 创建Relation关联
Relation = #{
    <<"__op">> => <<"AddRelation">>,
    <<"objects">> => [
        #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Class">>, <<"objectId">> => Id1},
        #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Class">>, <<"objectId">> => Id2}
    ]
}

dgiot_parse:update_object(<<"Class">>, ObjectId, #{<<"related_field">> => Relation})
```

#### 技巧3：嵌套对象关联
```erlang
%% 嵌套对象结构
NestedData = #{
    <<"parent">> => ParentData,
    <<"children">> => [
        ChildData1,
        ChildData2
    ],
    <<"metadata">> => #{
        <<"version">> => <<"1.0">>,
        <<"author">> => Author
    }
}
```

### 4. 权限管理技巧

#### 技巧1：ACL设置
```erlang
%% 基础ACL设置
ACL = #{
    UserId => #{<<"read">> => true, <<"write">> => true},
    RoleId => #{<<"read">> => true, <<"write">> => false},
    <<"*">> => #{<<"read">> => true}  %% 公共读权限
}

dgiot_parse:create_object(<<"Class">>, #{<<"ACL">> => ACL, ...})
```

#### 技巧2：角色权限继承
```erlang
%% 基于角色的权限管理
get_role_acl(RoleId) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId) of
        {ok, #{<<"ACL">> := RoleACL}} -> RoleACL;
        _ -> #{}
    end.

create_object_with_role_acl(Class, Data, RoleId) ->
    RoleACL = get_role_acl(RoleId),
    NewData = Data#{<<"ACL">> => RoleACL},
    dgiot_parse:create_object(Class, NewData)
```

### 5. 数据验证技巧

#### 技巧1：必需字段验证
```erlang
validate_required_fields(Data, RequiredFields) ->
    lists:foldl(
        fun(Field, Acc) ->
            case maps:is_key(Field, Data) of
                true -> Acc;
                false -> {error, {missing_field, Field}}
            end
        end,
        ok,
        RequiredFields
    ).
```

#### 技巧2：数据类型验证
```erlang
validate_data_types(Data, Schema) ->
    maps:fold(
        fun(Field, ExpectedType, Acc) ->
            case maps:get(Field, Data, undefined) of
                undefined -> Acc;
                Value when ExpectedType == binary, is_binary(Value) -> Acc;
                Value when ExpectedType == integer, is_integer(Value) -> Acc;
                Value when ExpectedType == float, is_float(Value) -> Acc;
                Value when ExpectedType == boolean, is_boolean(Value) -> Acc;
                Value when ExpectedType == map, is_map(Value) -> Acc;
                Value when ExpectedType == list, is_list(Value) -> Acc;
                _ -> {error, {invalid_type, Field, ExpectedType}}
            end
        end,
        ok,
        Schema
    ).
```

#### 技巧3：业务规则验证
```erlang
validate_business_rules(Data) ->
    %% 检查唯一性约束
    case check_uniqueness(Data) of
        {error, Reason} -> {error, Reason};
        ok ->
            %% 检查业务逻辑约束
            case check_business_logic(Data) of
                {error, Reason} -> {error, Reason};
                ok -> {ok, Data}
            end
    end.
```

### 6. 错误处理技巧

#### 技巧1：统一错误处理模式
```erlang
handle_create_result(Result) ->
    case Result of
        {ok, #{<<"objectId">> := ObjectId}} ->
            ?LOG(info, "Object created successfully: ~p", [ObjectId]),
            {ok, ObjectId};
        {error, #{<<"code">> := 101, <<"error">> := <<"Object not found.">>}} ->
            ?LOG(warning, "Object not found"),
            {error, not_found};
        {error, #{<<"code">> := 137, <<"error">> := <<"Duplicate object.">>}} ->
            ?LOG(warning, "Duplicate object"),
            {error, duplicate};
        {error, Reason} ->
            ?LOG(error, "Create failed: ~p", [Reason]),
            {error, Reason}
    end.
```

#### 技巧2：重试机制
```erlang
create_with_retry(Class, Data, Retries) ->
    create_with_retry(Class, Data, Retries, 0).

create_with_retry(_Class, _Data, MaxRetries, Attempt) when Attempt >= MaxRetries ->
    {error, max_retries_exceeded};
create_with_retry(Class, Data, MaxRetries, Attempt) ->
    case dgiot_parse:create_object(Class, Data) of
        {ok, Result} -> {ok, Result};
        {error, _} ->
            timer:sleep(100 * Attempt),  %% 指数退避
            create_with_retry(Class, Data, MaxRetries, Attempt + 1)
    end.
```

### 7. 性能优化技巧

#### 技巧1：批量操作
```erlang
batch_create_objects(Class, Objects) ->
    Requests = [
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"/classes/", Class/binary>>,
            <<"body">> => Object
        }
        || Object <- Objects
    ],
    
    case dgiot_parse:batch(Requests) of
        {ok, Results} ->
            process_batch_results(Results);
        {error, Reason} ->
            {error, Reason}
    end.
```

#### 技巧2：异步创建
```erlang
async_create_object(Class, Data) ->
    spawn(fun() ->
        case dgiot_parse:create_object(Class, Data) of
            {ok, Result} ->
                handle_async_success(Result);
            {error, Reason} ->
                handle_async_error(Reason)
        end
    end).
```

#### 技巧3：缓存优化
```erlang
cached_create_object(Class, Data) ->
    Key = {create, Class, dgiot_utils:to_md5(dgiot_json:encode(Data))},
    
    case dgiot_data:get(create_cache, Key) of
        {ok, Result} ->
            Result;
        not_find ->
            Result = dgiot_parse:create_object(Class, Data),
            dgiot_data:insert(create_cache, Key, Result),
            Result
    end.
```

## 实际应用示例

### 示例1：完整的用户创建流程
```erlang
create_user_with_profile(UserName, Password, Email, Profile) ->
    %% 1. 验证输入数据
    case validate_user_data(UserName, Password, Email) of
        {ok, ValidatedData} ->
            %% 2. 检查用户是否已存在
            case check_user_exists(UserName, Email) of
                {ok, not_exists} ->
                    %% 3. 创建用户对象
                    UserData = ValidatedData#{
                        <<"profile">> => Profile,
                        <<"createdat">> => dgiot_datetime:now_secs(),
                        <<"updatedat">> => dgiot_datetime:now_secs()
                    },
                    
                    case dgiot_parse:create_object(<<"_User">>, UserData) of
                        {ok, #{<<"objectId">> := UserId}} ->
                            %% 4. 设置用户ACL
                            set_user_acl(UserId),
                            
                            %% 5. 创建用户会话
                            create_user_session(UserId),
                            
                            {ok, UserId};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {ok, exists} ->
                    {error, user_already_exists};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

### 示例2：测试数据管理
```erlang
create_test_pipeline(DeviceId, TestConfig) ->
    %% 1. 创建测试任务
    {ok, TaskId} = create_test_task(DeviceId, TestConfig),
    
    %% 2. 创建测试步骤
    Steps = maps:get(<<"steps">>, TestConfig, []),
    StepIds = lists:map(
        fun(StepConfig) ->
            {ok, StepId} = create_test_step(TaskId, StepConfig),
            StepId
        end,
        Steps
    ),
    
    %% 3. 创建测试指标
    lists:foreach(
        fun({StepId, StepConfig}) ->
            Indications = maps:get(<<"indications">>, StepConfig, []),
            lists:foreach(
                fun(IndicationConfig) ->
                    create_test_indication(StepId, IndicationConfig)
                end,
                Indications
            )
        end,
        lists:zip(StepIds, Steps)
    ),
    
    {ok, #{task_id => TaskId, step_ids => StepIds}}.
```

### 示例3：系统初始化
```erlang
init_system_tables() ->
    Tables = [
        #{
            <<"name">> => <<"SystemConfig">>,
            <<"fields">> => [
                #{<<"name">> => <<"key">>, <<"type">> => <<"String">>},
                #{<<"name">> => <<"value">>, <<"type">> => <<"Object">>},
                #{<<"name">> => <<"description">>, <<"type">> => <<"String">>}
            ]
        },
        #{
            <<"name">> => <<"AuditLog">>,
            <<"fields">> => [
                #{<<"name">> => <<"action">>, <<"type">> => <<
