-module(dgiot_factory_meter).



-export([
    get_body/1,
    test/2,
    get_header/0,
    updata/2


]).
%%






get_body(FileName) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/json/"]),
    Name = dgiot_utils:to_list(FileName),
    NewName =
        case filename:extension(Name) of
            [] ->
                Name ++ ".json";
            _ ->
                Name
        end,
    Path = Dir ++ NewName,
    case catch file:read_file(Path) of
        {Err, _Reason} when Err == 'EXIT'; Err == error ->
%%                        ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
            #{};
        {ok, Bin} ->
%%                        jsx:decode(Bin, [{labels, binary}, return_maps])
            Bin
    end.

test(Payload, ObjectId) ->
    case updata(Payload, ObjectId) of
        {ok, Body} ->
%%            io:format("~s ~p Body= ~p ~n",[?FILE,?LINE,Body]),
    Url = "http://king.jeenor.com:8008/k3cloud/Kingdee.BOS.WebApi.ServicesStub.DynamicFormService.Save.common.kdsvc",
    Timestamp = dgiot_utils:to_list(dgiot_datetime:nowstamp()),
%%    Body= get_body("Newsave"),
%%    Path_url = "%2Fk3cloud%2FKingdee.BOS.WebApi.ServicesStub.DynamicFormService.ExecuteBillQuery.common.kdsvc",
    Path_url = "%2Fk3cloud%2FKingdee.BOS.WebApi.ServicesStub.DynamicFormService.Save.common.kdsvc", %save路径
    App_data = "62a939f4df3245,Administrator,2052,0",
    Client_sec = "bddc299e0a7d95cef5515adf3a8c1314",
    AppId = "229973_Xedp77lt6Jm+waUtS61C3cVv5Mx94BlL",
    AppSecret = "f582a31a838c4c6bae98a1747eba15aa",
    A = dgiot_utils:to_binary(AppId ++ App_data),
    B = dgiot_utils:to_binary(AppSecret),
    Api_sign = "POST" ++ dgiot_utils:to_list(Path_url) ++ "x-api-nonce:" ++ dgiot_utils:to_list(Timestamp) ++ "x-api-timestamp:" ++ dgiot_utils:to_list(Timestamp),
    AA = dgiot_utils:to_binary(Api_sign),
    BB = dgiot_utils:to_binary(Client_sec),
    Headers = [
        {"X-Api-ClientID", "229973"},
        {"X-Api-Auth-Version", "2.0"},
        {"X-Api-Timestamp", Timestamp},
        {"X-Api-Nonce", Timestamp},
        {"X-Api-SignHeaders", "x-api-timestamp,x-api-nonce"},
        {"X-Api-Signature", hmac(AA, BB)},
        {"X-KD-AppKey", "229973_Xedp77lt6Jm+waUtS61C3cVv5Mx94BlL"},
        {"X-KD-AppData", dgiot_utils:to_list(base64:encode(dgiot_utils:to_binary(App_data)))},
        {"X-KD-Signature", hmac(A, B)}, %%MDdmMzJiZDIxZWMxYjdkMDgwYjE3MzZlYmQ3MTJhOGNkYjQ4NzZhZjA4NTFiYzM0MWU1NzJjNTNlNzQ3NDczZA==
        {"Accept-Charset", "utf-8"},
        {"User-Agent", "Kingdee/Python WebApi SDK 7.3 (compatible; MSIE 6.0; Windows NT 5.1;SV1)"}
    ],
    ContentType = "application/json",
    Request = {Url, Headers, ContentType, Body},
            {ok,Row}=dgiot_http_client:request(post, Request),
            Row;
        _ ->
            error

 end.


get_header() ->

    Url = "http://king.jeenor.com:8008/k3cloud/Kingdee.BOS.ServiceFacade.ServicesStub.DynamicForm.DynamicFormService.Call.common.kdsvc",
    Body = get_body("newbody"),
    Header = [],
    ContentType = "application/json",
    Request = {Url, Header, ContentType, Body},
    {ok, {_Result, Headers, _Bodys}} = httpc:request(post, Request, [{timeout, 60000}, {connect_timeout, 60000}], [{body_format, binary}]),
    lists:foldl(
        fun
            ({"set-cookie", Row}, Acc) ->
                io:format("~s ~p Row = ~p ", [?FILE, ?LINE, Row]),
                case re:run(Row, "(.*?)=", [{capture, first, list}]) of
                    {match, ["kdservice-sessionid="]} -> Acc#{<<"KDService_SessionId">> => Row};
                    {match, ["ASP.NET_SessionId="]} -> Acc#{<<"ASP.NET_SessionId">> => Row};
                    _ -> Acc
                end;
            (_, Acc) ->
                Acc
        end, #{}, Headers).





hmac(Data, Key) ->
%%    Data=<<"229973_Xedp77lt6Jm+waUtS61C3cVv5Mx94BlL62a939f4df3245,Administrator,2052,0">>
    Big = dgiot_utils:binary_to_hex(crypto:mac(hmac, sha256, Key, Data)),
    dgiot_utils:to_list(base64:encode(string:to_lower(dgiot_utils:to_list(Big)))).



updata(Payload, DeviceId) ->
    FDocumentStatus=#{
        <<"创建"/utf8>> => <<"A">>,
        <<"审核中"/utf8>> => <<"B">>,
        <<"已审核"/utf8>> => <<"C">>,
        <<"重新审核"/utf8>> => <<"D">>,
        <<"暂存"/utf8>> => <<"Z">>
    },
    FWorkshipId= #{
        <<"生产部"/utf8>> => <<"BM000013">>
    },

    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"content">> := #{<<"baseInfo">> := BaseInfo}}} ->
            Json = jsx:decode(get_body("save")),
            Data = maps:get(<<"data">>, Json),
            Model = maps:get(<<"Model">>, Data),
            Modelvalue = maps:fold(fun(K, V, Acc) ->
                case K of
                    <<"FBillNo">> -> Acc#{<<"FBillNo">> => <<"">>};
                    <<"FBillType">> -> Acc#{<<"FBillType">> => #{<<"FName">> => maps:get(<<"Type_of_document">>, BaseInfo)}};
                    <<"FDocumentStatus">> -> Acc#{<<"FDocumentStatus">> =>  maps:get(maps:get(<<"Documents_state">>, BaseInfo), FDocumentStatus)};
                    <<"FDate">> -> Acc#{<<"FDate">> => maps:get(<<"Document_date">>, BaseInfo)};
                    <<"FPrdOrgId">> -> Acc#{<<"FPrdOrgId">> => #{<<"FNumber">> => <<"101">>}};
                    <<"FWorkshipIdH">> -> Acc#{<<"FWorkshipIdH">> => #{<<"FNumber">> => maps:get(maps:get(<<"Production_workshop">>, BaseInfo),FWorkshipId) }};
                    <<"FEntity">> ->
                        FEntity = maps:fold(fun(K1, V1, Acc1) ->
                            case K1 of
                                <<"FIsNew">> -> Acc1#{K1 => V1};
                                <<"FSrcEntryId">> -> Acc1#{<<"FSrcEntryId">> => maps:get(<<"FMoEntryId">>, BaseInfo)};
                                <<"FMaterialId">> -> Acc1#{<<"FMaterialId">> => #{<<"FNumber">> => maps:get(<<"Material_code">>, BaseInfo)}};
                                <<"FUnitID">> -> Acc1#{<<"FUnitID">> => #{<<"FNumber">> => maps:get(<<"Unit">>, BaseInfo)}};
                                <<"FMoEntrySeq">> -> Acc1#{<<"FMoEntrySeq">> => <<"1">>};
                                <<"FMoId">> -> Acc1#{<<"FMoId">> => maps:get(<<"FId">>, BaseInfo)};
                                <<"FMoEntryId">> -> Acc1#{<<"FMoEntryId">> => maps:get(<<"FMoEntryId">>, BaseInfo)};
                                <<"FWorkshipId">> -> Acc1#{<<"FWorkshipId">> => #{<<"FNumber">> =>  maps:get(maps:get(<<"Production_workshop">>, BaseInfo),FWorkshipId)}};
                                <<"FReworkQty">> -> Acc1#{<<"FReworkQty">> => 0};
                                <<"FScrapQty">> -> Acc1#{<<"FScrapQty">> => 0};
                                <<"FReMadeQty">> -> Acc1#{<<"FReMadeQty">> => 0};
                                <<"FQuaQty">> -> Acc1#{<<"FQuaQty">> => maps:get(<<"product_pnumber">>, Payload)};
                                <<"FFailQty">> -> Acc1#{<<"FFailQty">> => maps:get(<<"product_rejects">>, Payload)};
                                <<"FFinishQty">> ->
                                    case maps:find(<<"product_rejects">>, Payload) of
                                        {ok,FFailQty} ->
                                            case maps:find(<<"product_pnumber">>, Payload) of
                                                {ok,FQuaQty} ->
                                                    Acc1#{<<"FFinishQty">> =>FFailQty+FQuaQty };
                                                _ ->
                                                    Acc1
                                            end;
                                        _ ->
                                            Acc1
                                    end;
                                <<"FReportType">> -> Acc1#{<<"FReportType">> => #{<<"FNumber">> => <<"CTG002">>}};
                                <<"FBomId">> -> Acc1#{<<"FBomId">> => #{<<"FNumber">> => maps:get(<<"FBomId">>, BaseInfo)}};
                                <<"FCostRate">> -> Acc1#{K1 => V1};
                                <<"FCheckProduct">> -> Acc1#{K1 => V1};
                                <<"FIsEntrust">> -> Acc1#{K1 => V1};
                                <<"FStartTime">> -> Acc1#{<<"FStartTime">> => maps:get(<<"FStartTime">>, BaseInfo)};
                                <<"FEndTime">> -> Acc1#{<<"FEndTime">> => maps:get(<<"FStartTime">>, BaseInfo)};
                                <<"FTimeUnitId">> -> Acc1#{K1 => V1};
                                <<"FStandHourUnitId">> -> Acc1#{K1 => V1};
                                <<"FISBACKFLUSH">> -> Acc1#{K1 => V1};
                                <<"FIsFirstinspect">> -> Acc1#{K1 => V1}
                            end
                                            end
                            , #{}, lists:nth(1, V)),
                        Acc#{<<"FEntity">> => [FEntity]}
                end
                                   end, #{}, Model),
            NewData = maps:update(<<"Model">>, Modelvalue, Data),
            NewJson = maps:update(<<"data">>, NewData, Json),
%%            io:format("~s  ~p  NewJson= ~ts ~n", [?FILE, ?LINE, unicode:characters_to_list(jiffy:encode(NewJson))]),
            {ok, jiffy:encode(NewJson)};
        _ ->
            error
    end.
