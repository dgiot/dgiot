-module(dgiot_process).

-export([post_process/6, put_process/3, get_process/2, get_process/7]).


level_name_to_number(<<"二级"/utf8>>) ->
    <<"2">>;

level_name_to_number(<<"三级"/utf8>>) ->
    <<"3">>;

level_name_to_number(_) ->
    <<"1">>.


work_area_to_name(WorkArea) ->
    WorkAreas = dgiot_evidence:get_workareas(),
    case maps:find(WorkArea, WorkAreas) of
        {ok, Name} ->
            Name;
        _ ->
            not_find
    end.


%% 辅助函数：将二进制字符串值加1（例如 <<"01">> 变为 <<"02">>）
increment_value(Value) when is_binary(Value) ->
    try
        % 将二进制转换为整数
        Num = binary_to_integer(Value),
        % 值加1
        NewNum = Num + 1,
        % 格式化为两位字符串，用零填充，然后转回二进制
        Formatted = io_lib:format("~2..0B", [NewNum]),
        list_to_binary(Formatted)
    catch
        _:_ ->
            % 如果转换失败（如值非数字），返回默认值<<"01">>
            <<"01">>
    end.


%% @doc 主函数：根据规则处理 JSON 数据
%% 参数：JsonData - 原始JSON映射组，Dept - 部门键（二进制），Date - 日期键（二进制）
%% 返回：{Value, NewJsonData} - 处理后的值和更新后的JSON映射组
get_index(JsonData, Dept, Date) ->
    % 检查JsonData是否为空（未定义或空映射）
    case is_map(JsonData) =:= false orelse map_size(JsonData) == 0 of
        true ->
            % 规则6：JsonData为空，返回"01"并创建新结构
            NewValue = <<"01">>,
            NewDeptMap = #{Date => NewValue},
            NewJsonData = #{Dept => NewDeptMap},
            {NewValue, NewJsonData};
        false ->
            % JsonData不为空，正常处理
            case maps:find(Dept, JsonData) of
                {ok, DeptMap} ->
                    % 部门存在：检查日期是否存在
                    case maps:find(Date, DeptMap) of
                        {ok, Value} ->
                            % 规则2：日期存在，值加1并保持两位格式
                            NewValue = increment_value(Value),
                            % 规则5：清理部门映射，只保留当前日期条目
                            NewDeptMap = #{Date => NewValue},
                            NewJsonData = maps:put(Dept, NewDeptMap, JsonData),
                            {NewValue, NewJsonData};
                        error ->
                            % 规则4：日期不存在，返回"01"
                            NewValue = <<"01">>,
                            % 规则5：清理部门映射，只添加当前日期条目
                            NewDeptMap = #{Date => NewValue},
                            NewJsonData = maps:put(Dept, NewDeptMap, JsonData),
                            {NewValue, NewJsonData}
                    end;
                error ->
                    % 规则3：部门不存在，返回"01"，JsonData不变
                    NewValue = <<"01">>,
                    {NewValue, JsonData}
            end
    end.


%% @doc 从 <<"YYYY-MM-DD">> 格式的二进制字符串中移除连字符，变为 <<"YYYYMMDD">>。
remove_dashes(<<>>) ->
    <<>>;
remove_dashes(DateBinary) when is_binary(DateBinary) ->
    % 二进制推导：遍历二进制中的每个字节，只保留那些不等于连字符（45）的。
    << <<Byte>> || <<Byte>> <= DateBinary, Byte =/= 45 >>.


%% @doc 主函数：在JSON数据中找到所有name为"serial"的节点，设置或更新value字段
%% 参数：JsonData - 原始JSON数据（map或{obj, proplist}格式），Serial - 要设置的序列号值
%% 返回：更新后的JSON数据
fill_serial(JsonData, Serial) when is_map(JsonData) ->
    case maps:get(<<"paramList">>, JsonData, not_found) of
        not_found ->
            JsonData;
        ParamList when is_list(ParamList) ->
            NewParamList = update_param_list(ParamList, Serial),
            JsonData#{<<"paramList">> => NewParamList}
    end;
fill_serial({obj, Props}, Serial) ->
    case proplists:get_value("paramList", Props, not_found) of
        not_found ->
            {obj, Props};
        ParamList when is_list(ParamList) ->
            NewParamList = update_param_list(ParamList, Serial),
            NewProps = proplists:put("paramList", NewParamList, Props),
            {obj, NewProps}
    end;
fill_serial(JsonData, _Serial) ->
    JsonData.


%% @doc 更新paramList中的每个参数对象
update_param_list(ParamList, Serial) ->
    lists:map(fun(Param) -> update_param(Param, Serial) end, ParamList).


%% @doc 更新单个参数对象中的nodes
update_param(Param, Serial) when is_map(Param) ->
    case maps:get(<<"nodes">>, Param, not_found) of
        not_found -> Param;
        Nodes when is_list(Nodes) ->
            NewNodes = update_nodes(Nodes, Serial),
            Param#{<<"nodes">> => NewNodes}
    end;
update_param({obj, Props}, Serial) ->
    case proplists:get_value("nodes", Props, not_found) of
        not_found -> {obj, Props};
        Nodes when is_list(Nodes) ->
            NewNodes = update_nodes(Nodes, Serial),
            NewProps = proplists:put("nodes", NewNodes, Props),
            {obj, NewProps}
    end;
update_param(Param, _Serial) ->
    Param.


%% @doc 更新nodes列表，精准查找name为"serial"的节点
update_nodes(Nodes, Serial) ->
    lists:map(fun(Node) when is_map(Node) ->
                      case maps:get(<<"name">>, Node, undefined) of
                          <<"serial">> ->
                              % 找到目标节点，更新value字段
                              Node#{<<"value">> => Serial};
                          _ ->
                              Node
                      end;
                 ({obj, Props}) ->
                      case proplists:get_value("name", Props, undefined) of
                          "serial" ->
                              % 找到目标节点，更新value字段
                              NewProps = proplists:put("value", Serial, Props),
                              {obj, NewProps};
                          _ ->
                              {obj, Props}
                      end;
                 (Node) ->
                      Node
              end,
              Nodes).


dept_to_name([], _) ->
    not_find;

dept_to_name([#{<<"value">> := DeptCode} = Dept | _Rest], DeptCode) ->
    Dept;

dept_to_name([_ | Rest], DeptCode) ->
    dept_to_name(Rest, DeptCode).


check_unique(ProductId, DeptCode, WorkAreaCode, Date) ->
    Key = iolist_to_binary([ProductId, "-", DeptCode, "-", WorkAreaCode, "-", Date]),
    ProcessId = dgiot_parse_id:get_deviceid(ProductId, Key),
    case dgiot_parse:get_object(<<"Device">>, ProcessId) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            {true, ObjectId};
        _ ->
            false
    end.


post_process(ProductId, DeptCode, LevelName, WorkAreaCode, Date, SessionToken) ->

    Indexs = dgiot_evidence:get_indexs(),
    ShortDate = remove_dashes(Date),
    Res = get_index(Indexs, DeptCode, ShortDate),

    io:format("~s ~p Index ~p Res ~p~n", [?FILE, ?LINE, Indexs, Res]),

    {Index, JsonData} = Res,

    io:format("~s ~p Index ~p JsonData ~p~n", [?FILE, ?LINE, Index, JsonData]),

    dgiot_evidence:save_indexs(JsonData),

    check_unique(ProductId, DeptCode, WorkAreaCode, Date),

    LevelNumber = level_name_to_number(LevelName),

    % 同区域，同工作区域，同日期，允许申请相同的许可证，所以后面加上LevelNumber和Index的拼接
    DevAddr = iolist_to_binary([ProductId, "-", DeptCode, "-", WorkAreaCode, "-", Date, "-", LevelNumber, "-", Index]),

    WorkAreaName = work_area_to_name(WorkAreaCode),

    Name = iolist_to_binary(["TG-", DeptCode, "-", LevelName, "-", WorkAreaName]),

    SerialNo = iolist_to_binary(["TG-", DeptCode, "-", ShortDate, "-", Index]),

    io:format("~s ~p Name ~p SerialNo ~p DevAddr ~p~n", [?FILE, ?LINE, Name, SerialNo, DevAddr]),

    % io:format("~s ~p Name ~p ProductId ~p DevAddr ~p~n", [?FILE, ?LINE, Name, ProductId, DevAddr]),

    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"content">> := Content}} ->
            DevContent = maps:with([<<"templateList">>, <<"paramList">>], Content),
            NewContent = fill_serial(DevContent, SerialNo),

            #{<<"objectId">> := CreatorId, <<"nick">> := NickName, <<"roles">> := Roles} = dgiot_auth:get_session(SessionToken),

            % io:format("~s ~p Name ~p ProductId ~p Roles ~p~n", [?FILE, ?LINE, Name, ProductId, Roles]),
            [#{<<"name">> := _Role, <<"objectId">> := DeptId} | _] = maps:values(Roles),
            % io:format("~s ~p Name ~p ProductId ~p Role ~p  DeptId ~p~n", [?FILE, ?LINE, Name, ProductId, Role, DeptId]),

            % 创建者只有读的权限，上级有读写的权限
            Acl = assign_post_acl(CreatorId, DeptId),

            TemplateList = maps:get(<<"templateList">>, NewContent, []),
            ParamList = maps:get(<<"paramList">>, NewContent, #{}),
            ProcessId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),

            case query_report_service(TemplateList, ProcessId, ParamList) of
                error ->
                    {error, #{code => 1, <<"result">> => <<"failed">>, <<"msg">> => <<"failed to generate report">>}};
                OutputMap ->
                    % io:format("~s ~p ProcessId ~p OutputMap ~p ~n", [?FILE, ?LINE, ProcessId, OutputMap]),
                    Creator = #{
                                <<"objectId">> => CreatorId,
                                <<"nick">> => NickName
                               },
                    Level = #{
                              <<"value">> => LevelNumber,
                              <<"label">> => LevelName
                             },

                    WorkArea = #{
                                 <<"value">> => WorkAreaCode,
                                 <<"label">> => WorkAreaName
                                },

                    Department = dept_to_name(dgiot_evidence:get_areas(), DeptCode),

                    Data = #{
                             <<"devaddr">> => DevAddr,
                             <<"name">> => Name,
                             <<"product">> => ProductId,
                             <<"ACL">> => Acl,
                             <<"status">> => <<"ONLINE">>,
                             <<"state">> => 0,
                             <<"brand">> => <<"DGIOT流程"/utf8>>,
                             <<"devModel">> => <<"流程管理"/utf8>>,
                             <<"content">> => NewContent#{<<"output">> => OutputMap}#{<<"creator">> => Creator}#{<<"level">> => Level}#{<<"workArea">> => WorkArea}#{<<"department">> => Department}
                            },
                    % io:format("~s ~p ~p~n", [?FILE, ?LINE, Data]),
                    case dgiot_device:create_device(Data) of
                        {ok, #{<<"objectId">> := ObjectId, <<"createdAt">> := CreatedAt} = _Result} ->
                            {ok, #{code => 0, <<"objectId">> => ObjectId, <<"createdAt">> => CreatedAt}};
                        Error ->
                            {error, Error}
                    end
            end;
        Error ->
            io:format("~s ~p ~p~n", [?FILE, ?LINE, Error]),
            {error, Error}
    end.


assign_post_acl(CreatorId, DeptId) ->
    Acl = #{<<CreatorId/binary>> => #{<<"read">> => true}},
    case find_dept_manager(DeptId) of
        not_find ->
            Acl;
        ManagerId ->
            Acl#{<<ManagerId/binary>> => #{<<"read">> => true, <<"write">> => true}}
    end.


put_process(#{<<"test12345">> := _Test} = Id, ParamList, SessionToken) ->
    case dgiot_parse:get_object(<<"Device">>, Id) of
        {ok, Device} ->
            _NewDevice = maps:with([<<"objectId">>, <<"name">>, <<"createdAt">>, <<"updatedAt">>, <<"state">>], Device),
            Content = maps:get(<<"content">>, Device, #{}),
            #{<<"roles">> := Roles, <<"objectId">> := ObjectId} = dgiot_auth:get_session(SessionToken),
            % io:format("~s ~p Id ~p ObjectId ~p Roles ~p", [?FILE, ?LINE, Id, ObjectId, Roles]),
            [#{<<"name">> := RoleName, <<"objectId">> := RoleId} | _] = maps:values(Roles),
            ParamListFixed = filter_param_list(Content, ObjectId, RoleId, RoleName),
            % io:format("~s ~p Id ~p 111 paramList ~p~n", [?FILE, ?LINE, Id, ParamListFixed]),
            NamesFixed = extract_param(ParamListFixed),
            Names = extract_param(ParamList),
            io:format("~s ~p Id ~p NamesFixed ~p Names ~p~n", [?FILE, ?LINE, Id, NamesFixed, Names]),
            case check_param_auth(Names, NamesFixed) of
                true ->
                    ParamFixedMap = param_list_to_map(ParamListFixed),
                    % io:format("~s ~p Id ~p ParamFixedMap ~p ~n", [?FILE, ?LINE, Id, ParamFixedMap]),
                    NewParamMap = param_merge(ParamList, ParamFixedMap),
                    % io:format("~s ~p Id ~p NewParamMap ~p ~n", [?FILE, ?LINE, Id, NewParamMap]),
                    NewParamList = param_map_to_list(NewParamMap),
                    Status = calc_status(NewParamList),
                    NewContent = Content#{<<"paramList">> => NewParamList},
                    TemplateList = maps:get(<<"templateList">>, Content, []),
                    case query_report_service(TemplateList, Id, NewParamList) of
                        error ->
                            {error, #{<<"result">> => <<"failed">>, <<"msg">> => <<"审批流程失败：生成报告失败">>}};
                        OutputMap ->
                            % io:format("~s ~p Id ~p OutputMap ~p ~n", [?FILE, ?LINE, Id, OutputMap]),
                            case dgiot_parse:update_object(<<"Device">>, Id, #{<<"content">> => NewContent#{<<"output">> => OutputMap}, <<"state">> => Status}) of
                                {ok, Result} ->
                                    {ok, Result};
                                Error ->
                                    Error
                            end
                    end;
                _ ->
                    io:format("~s ~p Id ~p NamesFixed ~p Names ~p~n", [?FILE, ?LINE, Id, NamesFixed, Names]),
                    {error, #{<<"result">> => <<"failed">>, <<"msg">> => <<"审批流程失败">>}}
            end;
        % {ok, NewDevice#{<<"content">> => Content#{<<"paramList">> => ParamListFixed}, <<"role">> => RoleName}};
        Error -> Error
    end;

put_process(Id, ParamList, _SessionToken) ->
    try
        case dgiot_parse:get_object(<<"Device">>, Id) of
            {ok, Device} ->
                _NewDevice = maps:with([<<"objectId">>, <<"name">>, <<"createdAt">>, <<"updatedAt">>, <<"state">>], Device),
                Content = maps:get(<<"content">>, Device, #{}),
                NewContent = Content#{<<"paramList">> => ParamList},
                % io:format("~s ~p Id ~p NewContent ~p ~n", [?FILE, ?LINE, Id, NewContent]),
                TemplateList = maps:get(<<"templateList">>, Content, []),
                case query_report_service(TemplateList, Id, ParamList) of
                    error ->
                        {error, #{<<"result">> => <<"failed">>, <<"msg">> => <<"审批流程失败：生成报告失败">>}};
                    OutputMap ->
                        % io:format("~s ~p Id ~p OutputMap ~p ~n", [?FILE, ?LINE, Id, OutputMap]),
                        case dgiot_parse:update_object(<<"Device">>, Id, #{<<"content">> => NewContent#{<<"output">> => OutputMap}, <<"state">> => 1}) of
                            {ok, Result} ->
                                {ok, Result};
                            Error ->
                                Error
                        end
                end;
            % {ok, NewDevice#{<<"content">> => Content#{<<"paramList">> => ParamListFixed}, <<"role">> => RoleName}};
            Error -> Error
        end
    catch
        Throw:Reason ->
            io:format("~s ~p ~p ~p", [?FILE, ?LINE, Throw, Reason]),
            {error, Reason}
    end.


get_process(Id, SessionToken) ->
    case dgiot_parse:get_object(<<"Device">>, Id) of
        {ok, Device} ->
            NewDevice = maps:with([<<"objectId">>, <<"name">>, <<"createdAt">>, <<"updatedAt">>, <<"state">>], Device),
            Content = maps:get(<<"content">>, Device, #{}),
            #{<<"roles">> := Roles, <<"objectId">> := ObjectId} = dgiot_auth:get_session(SessionToken),
            % io:format("~s ~p Id ~p ObjectId ~p Roles ~p", [?FILE, ?LINE, Id, ObjectId, Roles]),
            [#{<<"name">> := RoleName, <<"objectId">> := RoleId} | _] = maps:values(Roles),
            _ParamList = filter_param_list(Content, ObjectId, RoleId, RoleName),
            % io:format("~s ~p Id ~p 111 paramList ~p~n", [?FILE, ?LINE, Id, ParamList]),
            % {ok, NewDevice#{<<"content">> => Content#{<<"paramList">> => ParamList}, <<"role">> => RoleName}};
            {ok, NewDevice#{<<"content">> => Content}};
        Error -> Error
    end.


get_process(Skip, Limit, Type, Where, Order, From, SessionToken) ->
    case dgiot_parsex:query_object(<<"Product">>, #{<<"keys">> => [<<"objectId">>, <<"name">>], <<"where">> => #{<<"devType">> => <<"COSL">>}}) of
        {ok, #{<<"results">> := Products}} ->
            ProductIds =
                case Type of
                    <<"regong">> ->
                        [<<"6d0fd0a154">>];
                    <<"lenggong">> ->
                        [<<"2c461b75eb">>];
                    _ ->
                        lists:foldl(fun(#{<<"objectId">> := Id}, Acc) ->
                                            [Id | Acc]
                                    end,
                                    [],
                                    Products)
                end,
            io:format("~s ~p ~p~n", [?FILE, ?LINE, ProductIds]),
            Query = #{
                      <<"count">> => <<"objectId">>,
                      <<"keys">> => [<<"objectId">>, <<"name">>, <<"state">>],
                      %   <<"excludeKeys">> => [<<"channel">>, <<"children">>, <<"config">>, <<"thing">>, <<"decoder">>, <<"data">>, <<"basedata">>, <<"content">>, <<"createdAt">>, <<"updatedAt">>, <<"detail">>],
                      <<"where">> => Where#{<<"product">> => #{<<"$in">> => ProductIds}},
                      <<"order">> => Order,
                      <<"limit">> => Limit,
                      <<"skip">> => Skip
                     },
            io:format("~s ~p Query ~p~n", [?FILE, ?LINE, Query]),
            io:format("~s ~p From ~p~n", [?FILE, ?LINE, From]),

            Result =
                case From of
                    <<"app">> ->
                        dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
                    _ ->
                        dgiot_parse:query_object(<<"Device">>, Query)
                end,
            io:format("~s ~p count: ~p~n", [?FILE, ?LINE, size(Result)]),
            case Result of
                {ok, #{<<"count">> := Count, <<"results">> := Devices}} ->
                    #{<<"count">> => Count, <<"results">> => Devices};
                Error ->
                    Error
            end;
        _ ->
            {error, <<"not find">>}
    end.


query_report_service([], _ProcessId, _ParamList) ->
    ok;

query_report_service([#{<<"path">> := _TemplateUrl, <<"type">> := _Type} | _Rest], _ProcessId, _ParamList) ->
    dgiot_evidence:get_report_data();

query_report_service([#{<<"path2">> := TemplateUrl, <<"type">> := Type} | Rest], ProcessId, ParamList) ->
    BaseUrl = application:get_env(evidence, process_baseurl, not_find),
    RequestUrl = BaseUrl ++ "/WordController/replaceWord",

    Headers = [{"Accept", "application/json"},
               {"User-Agent", "Http"}],

    ContentType = "application/json; charset=UTF-8",

    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, TemplateUrl]),
    io:format("~s ~p  ~p ~n", [?FILE, ?LINE, ParamList]),
    Datas = lists:foldl(fun(X, Acc) ->
                                case X of
                                    #{<<"name">> := Name, <<"value">> := Value, <<"type">> := Type1} ->
                                        % io:format("~s ~p ~ts ~p ~ts ~n", [?FILE, ?LINE, Name, Value, Type1]),
                                        Header = maps:get(<<"header">>, X, []),
                                        io:format("~s ~p ~ts ~p ~n", [?FILE, ?LINE, Name, Header]),

                                        Node =
                                            case Type1 of
                                                <<"text">> ->
                                                    #{
                                                      <<"name">> => Name,
                                                      <<"value">> => Value,
                                                      <<"type">> => <<"text">>
                                                     };
                                                <<"table">> ->
                                                    % 提取表头字符串：从Headers中获取所有"cname"，用逗号连接
                                                    HeaderList1 = get_table_headers(Header, []),
                                                    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, HeaderList]),

                                                    SeparatedList1 = lists:join(<<",">>, HeaderList1),  % 结果类似 [<<"型号">>, <<",">>, <<"姓名">>, <<",">>, <<"级别">>]
                                                    TableHeader = iolist_to_binary(SeparatedList1),

                                                    % io:format("~s ~p  ~ts ~n", [?FILE, ?LINE, TableHeader]),
                                                    % 1. 先获取Headers中"name"的顺序列表，作为键的顺序（确保与表头对应）
                                                    Keys1 = get_table_names(Header, []),  % 结果: ["xh", "xm"]

                                                    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, Keys]),

                                                    RowStrings = sort_table_values(Value, Keys1, []),

                                                    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, RowStrings]),
                                                    #{
                                                      <<"name">> => Name,
                                                      <<"data">> => RowStrings,
                                                      <<"type">> => <<"table">>,
                                                      <<"header">> => TableHeader
                                                     };
                                                <<"checkbox">> ->
                                                    % Keys2 = get_table_names(Header, []),  % 结果: ["xh", "xm"]
                                                    io:format("~s ~p  ~p ~n", [?FILE, ?LINE, Value]),
                                                    HeaderList2 = get_table_headers(Header, []),
                                                    io:format("~s ~p  ~p ~n", [?FILE, ?LINE, HeaderList2]),
                                                    CheckboxList1 = sort_checkbox_values(Header, Value, []),
                                                    SeparatedList2 = lists:join(<<",">>, CheckboxList1),
                                                    Strings = iolist_to_binary(SeparatedList2),
                                                    io:format("~s ~p  ~ts ~n", [?FILE, ?LINE, Strings]),

                                                    #{
                                                      <<"name">> => Name,
                                                      <<"value">> => Strings,
                                                      <<"type">> => <<"text">>
                                                     };
                                                <<"date">> ->
                                                    #{
                                                      <<"name">> => Name,
                                                      <<"value">> => Value,
                                                      <<"type">> => <<"text">>
                                                     };
                                                <<"datetime">> ->
                                                    #{
                                                      <<"name">> => Name,
                                                      <<"value">> => Value,
                                                      <<"type">> => <<"text">>
                                                     };
                                                <<"cascader">> ->

                                                    #{
                                                      <<"name">> => Name,
                                                      <<"value">> => Value,
                                                      <<"type">> => <<"text">>
                                                     };
                                                _ ->
                                                    io:format("~s ~p  ~ts ~n", [?FILE, ?LINE, Type1]),
                                                    #{}
                                            end,
                                        [Node | Acc];
                                    _ ->
                                        Acc
                                end
                        end,
                        [],
                        ParamList),
    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, Datas]),

    Body = #{
             <<"datas">> => format_datas(Datas),
             <<"templateUrl">> => TemplateUrl,
             <<"path">> => <<ProcessId/binary, <<"/">>/binary, Type/binary>>,
             <<"wordName">> => Type
            },
    Request = {RequestUrl, Headers, ContentType, dgiot_json:encode(Body)},
    io:format("~s ~p ~p ~p ~p ~ts ~n", [?FILE, ?LINE, RequestUrl, Headers, ContentType, dgiot_json:encode(Body)]),
    case dgiot_http_client:request(post, Request) of
        {ok, #{<<"code">> := 200, <<"images">> := Images, <<"path">> := OutputFilePath}} ->
            % io:format("~s ~p Images: ~p OutputFilePath: ~p ~n", [?FILE, ?LINE, Images, OutputFilePath]),
            OutputMap = #{Type => #{<<"path">> => OutputFilePath, <<"images">> => Images}},
            case query_report_service(Rest, ProcessId, ParamList) of
                error ->
                    error;
                ok ->
                    OutputMap;
                ResultMap ->
                    maps:merge(OutputMap, ResultMap)
            end;
        _Other ->
            io:format("~s ~p Data  ~p~n", [?FILE, ?LINE, _Other]),
            % query_report_service([TemplateUrl | Rest], ProcessId, ParamList, BaseUrl, OutputMap),
            error
    end.


filter_param_list(Content, UserId, RoleId, RoleName) ->
    TemplateList = maps:get(<<"paramList">>, Content, []),
    filter_param_list(TemplateList, UserId, RoleId, RoleName, []).


filter_param_list([], _UserId, _RoleId, _RoleName, Acc) ->
    Acc;
filter_param_list([#{<<"name">> := _Name, <<"desc">> := _Desc, <<"type">> := _Type, <<"roles">> := Roles} = Param | Rest], UserId, RoleId, RoleName, Acc) ->
    % io:format("~s ~p  222 UserId: ~p Param ~p~n", [?FILE, ?LINE, UserId, Param]),
    % io:format("~s ~p  333 Name ~p Desc ~ts Type ~p UserId ~p RoleId ~p Roles ~p~n", [?FILE, ?LINE, _Name, _Desc, _Type, UserId, RoleId, Roles]),
    % 根据用户角色过滤流程参数
    % case role_in_roless(RoleId, Roles) of
    %     true ->
    %         filter_param_list(Rest, UserId, RoleId, RoleName, [Param | Acc]);
    %     _ ->
    % 根据用户id过滤流程参数
    case user_in_roless(UserId, Roles) of
        true ->
            filter_param_list(Rest, UserId, RoleId, RoleName, [Param | Acc]);
        _ ->
            filter_param_list(Rest, UserId, RoleId, RoleName, Acc)
            % end
    end;

filter_param_list([_Param | Rest], UserId, RoleId, RoleName, Acc) ->
    filter_param_list(Rest, UserId, RoleId, RoleName, Acc).


% role_in_roless(_ObjectId, []) ->
%     false;
% role_in_roless(ObjectId, [Roles | Roless]) ->
%     case lists:member(ObjectId, Roles) of
%         true ->
%             true;
%         _ ->
%             role_in_roless(ObjectId, Roless)
%     end.


user_in_roless(_UserId, []) ->
    false;

user_in_roless(UserId, [Roles | Roless]) ->
    case user_in_roles(UserId, Roles) of
        true ->
            true;
        _ ->
            user_in_roless(UserId, Roless)
    end.


user_in_roles(_UserId, []) ->
    false;

user_in_roles(UserId, Roles) ->
    % UserId =:= lists:last(Roles).
    case lists:member(UserId, Roles) of
        true ->
            true;
        _ ->
            false
    end.


extract_param(ParamList) ->
    lists:foldl(
      fun(X, Acc) ->
              %   io:format("~s ~p X ~p~n", [?FILE, ?LINE, X]),
              case X of
                  #{<<"name">> := Name} ->
                      [Name | Acc];
                  _ ->
                      Acc
              end
      end,
      [],
      ParamList).


check_param_auth([], _NamesFixed) ->
    true;
check_param_auth([_Name | _Rest], _NamesFixed) ->
    % io:format("~s ~p  222 UserId: ~p Param ~p~n", [?FILE, ?LINE, UserId, Param]),
    % case lists:member(Name, NamesFixed) of
    %     true ->
    %         check_param_auth(Rest, NamesFixed);
    %     Reason ->
    %         io:format("~s ~p  555 Reason ~p Name ~p~n", [?FILE, ?LINE, Reason, Name]),
    %         false
    % end.
    true.


param_list_to_map(ParamList) ->
    lists:foldl(
      fun(X, Map) ->
              case X of
                  #{<<"name">> := Name} ->
                      Map#{Name => X};
                  _ ->
                      Map
              end
      end,
      #{},
      ParamList).


param_merge([], ParamMap) ->
    ParamMap;

param_merge([Param | Rest], ParamMap) ->
    NewParamMap =
        case Param of
            #{<<"name">> := Name, <<"value">> := Value} ->
                % io:format("~s ~p  222 Name ~p Value ~p~n", [?FILE, ?LINE, Name, Value]),

                case maps:find(Name, ParamMap) of
                    {ok, Map} ->
                        NewMap = Map#{<<"value">> => Value},
                        ParamMap#{Name => NewMap};
                    _ ->
                        ParamMap
                end;
            _ ->
                ParamMap
        end,
    param_merge(Rest, NewParamMap).


param_map_to_list(ParamMap) ->
    maps:fold(
      fun(_K, V, Acc) ->
              [V | Acc]
      end,
      [],
      ParamMap).


calc_status([]) ->
    8;

calc_status([Param | Rest]) ->
    case maps:find(<<"value">>, Param) of
        {ok, Value} when Value =/= <<"">> ->
            calc_status(Rest);
        _ ->
            1
    end.


format_datas(Datas) ->
    Datas.


get_table_headers([], Acc) ->
    Acc;

get_table_headers([Header | Rest], Acc) ->
    case maps:find(<<"cname">>, Header) of
        {ok, CName} ->
            get_table_headers(Rest, [CName | Acc]);
        _ ->
            get_table_headers(Rest, Acc)
    end.


get_table_names([], Acc) ->
    Acc;

get_table_names([Header | Rest], Acc) ->
    case maps:find(<<"name">>, Header) of
        {ok, Name} ->
            get_table_names(Rest, [Name | Acc]);
        _ ->
            get_table_names(Rest, Acc)
    end.


sort_table_values([], _Keys, Acc) ->
    Acc;

sort_table_values([Value | Rest], Keys, Acc) ->
    % 提取行数据列表：

    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, Keys]),

    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, Value]),

    V1 = [ maps:get(Key, Value, "") || Key <- Keys ],

    % io:format("~s ~p  ~p ~n", [?FILE, ?LINE, V1]),

    % 2. 遍历Values，对每个映射按Keys顺序提取值（忽略Values中的键顺序），并用逗号连接

    SeparatedList = lists:join(<<",">>, V1),
    RowString = iolist_to_binary(SeparatedList),

    % io:format("~s ~p  ~ts ~n", [?FILE, ?LINE, RowString]),

    sort_table_values(Rest, Keys, [RowString | Acc]).


sort_checkbox_values([], _Values, Acc) ->
    Acc;

sort_checkbox_values([Header | Rest], Values, Acc) ->
    io:format("~s ~p  ~p ~p ~n", [?FILE, ?LINE, Header, Values]),

    CName = maps:get(<<"cname">>, Header, <<"">>),

    String =
        case maps:find(<<"name">>, Header) of
            {ok, Name} ->
                case lists:member(Name, Values) of
                    true ->
                        <<16#2611/utf8, CName/binary>>;
                    false ->
                        <<16#2610/utf8, CName/binary>>
                end;
            _ ->
                <<16#2610/utf8, CName/binary>>
        end,
    sort_checkbox_values(Rest, Values, [String | Acc]).


find_dept_manager(DeptId) ->
    Depts = dgiot_evidence:get_depts(),
    case is_map(Depts) of
        true ->
            case maps:find(DeptId, Depts) of
                {ok, #{<<"manager">> := ManagerId}} ->
                    %% 找到键，Value 是对应的值，可以在这里进行后续处理
                    io:format("~s ~p Found value: ~p~n", [?FILE, ?LINE, ManagerId]),
                    ManagerId;
                _ ->
                    %% 没有找到键，处理键不存在的情况
                    io:format("~s ~p Key ~p not found in Depts.~n", [?FILE, ?LINE, DeptId]),
                    not_find
            end;
        false ->
            io:format("~s ~p Depts is not a map.~n", [?FILE, ?LINE]),
            not_find
    end.
