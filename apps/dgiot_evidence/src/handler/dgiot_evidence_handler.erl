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

-module(dgiot_evidence_handler).
-author("johnliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_system/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
swagger_system() ->
    [
        dgiot_http_server:bind(<<"/swagger_evidence.json">>, ?MODULE, [], priv)
    ].

%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_framework:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            {200, Headers, #{}, Req};
        {ok, Res} ->
            {200, Headers, Res, Req};
        {Status, Res} ->
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================
do_request(post_evidence, Args, #{<<"sessionToken">> := SessionToken} = _Context, Req) ->
    ?LOG(info,"Args ~p ", [Args]),
    Host = dgiot_req:host(Req),
    ?LOG(info,"Host ~p ", [Host]),
    case dgiot_evidence:post(Args#{<<"ip">> => Host, <<"sessionToken">> => SessionToken}) of
        {ok, Result} ->
            {200, Result};
        {error, Reason} ->
            {error, Reason}
    end;

do_request(put_evidence, #{<<"status">> := Status} = Args, #{<<"sessionToken">> := SessionToken} = _Context, Req) ->
    ?LOG(info,"Status ~p ", [Status]),
    Host = dgiot_req:host(Req),
    dgiot_evidence:put(Args#{<<"ip">> => Host}, SessionToken);


do_request(get_evidence, #{<<"reportId">> := _ReportId} = Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_evidence:get(Args, SessionToken) of
        {ok, Result} ->
            {200, Result};
        {error, Reason} ->
            {error, Reason}
    end;

do_request(get_cert, _Args, _Context, _Req) ->
    case dgiot_evidence:readCert() of
        {ok, Result} ->
            {200, Result};
        {error, Reason} ->
            {error, Reason}
    end;

%% evidence 概要: 查询边缘网关及其子设备 描述:查询边缘网关及其子设备
%% OperationId:get_bed
%% 请求:POST /iotapi/bed
do_request(get_bed, #{<<"id">> := Id} = _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    get_bed(Id, SessionToken);

%% evidence 概要: 取证前处理 描述:取证前处理
%% OperationId:post_bed
%% 请求:POST /iotapi/bed
do_request(post_bed, #{<<"datatype">> := <<"liveMonitor">>} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"Body ~p", [Body]),
    case dgiot_auth:get_session(SessionToken) of
        #{<<"roles">> := Roles} ->
            [#{<<"alias">> := _AppId, <<"name">> := _AppName} | _] = maps:values(Roles);
        _ -> pass
    end,
    {200, #{<<"result">> => #{<<"status">> => 0}}};

do_request(post_bed, _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"roles">> := Roles} ->
            [#{<<"alias">> := _AppId, <<"name">> := _AppName} | _] = maps:values(Roles);
        _ -> pass
    end,
    {200, #{<<"result">> => #{<<"status">> => 0}}};

%% evidence 概要: 增加取证报告模版 描述:新增取证报告模版
%% OperationId:post_reporttemp
%% 请求:put /iotapi/reporttemp
do_request(put_reporttemp, #{<<"nodeType">> := _NodeType, <<"devType">> := _DevType} = Body,
        #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"Body ~p ", [Body]),
    R = dgiot_product:create_product(Body, SessionToken),
    ?LOG(info,"R ~p ", [R]),
    R;

%% DB 概要: 导入质检报告模版 描述:word/pdf质检报告
%% OperationId:reporttemp
%% 请求:POST /iotapi/reporttemp
do_request(post_reporttemp, #{<<"name">> := Name, <<"devType">> := DevType, <<"config">> := Config, <<"file">> := FileInfo},
        #{<<"sessionToken">> := SessionToken} = _Context, Req) ->
    Neconfig = jsx:decode(Config, [{labels, binary}, return_maps]),
    DataResult =
        case maps:get(<<"contentType">>, FileInfo, <<"unknow">>) of
            ContentType when
                ContentType =:= <<"application/msword">> orelse
                    ContentType =:= <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">> orelse
                    ContentType =:= <<"application/pdf">> ->
                FullPath = maps:get(<<"fullpath">>, FileInfo),
                Uri = "http://" ++ dgiot_utils:to_list(dgiot_req:host(Req)) ++ ":" ++ dgiot_utils:to_list(dgiot_req:port(Req)),
                {ok, #{<<"result">> => do_report(Neconfig, DevType, Name, SessionToken, FullPath,Uri)}};
            ContentType ->
                {error, <<"contentType error, contentType:", ContentType/binary>>}
        end,
    case DataResult of
        {ok, Data} ->
            {ok, Data};
        Error -> Error
    end;

%% evidence 概要: 增加取证报告 描述:新增取证报告
%% OperationId:get_report
%% 请求:GET /iotapi/report
do_request(get_report, #{<<"id">> := Id} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"Body ~p ", [Body]),
    get_report(Id, SessionToken);

%% evidence 概要: 增加取证报告 描述:新增取证报告
%% OperationId:post_report
%% 请求:POST /iotapi/report
do_request(post_report, #{<<"name">> := _Name, <<"devType">> := _DevType,
    <<"basedata">> := _Basedata} = Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"Body ~p ", [Body]),
    post_report(Body, SessionToken);


%% evidence 概要: 增加取证报告 描述:新增取证报告
%% OperationId:post_report
%% 请求:PUT /iotapi/report
do_request(put_report, #{<<"path">> := Path} = _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    put_report(Path, SessionToken);

%% Evidence 概要: 删除取证报告 描述:删除取证报告
%% OperationId:DELETE_REPORT_REPORTID
%% 请求:GET /iotapi/report
do_request(delete_report_reportid, #{<<"reportId">> := ReportId} = _Args,
        #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    delete_report(ReportId, SessionToken);


%% evidence 概要: 查询采样点 描述:查询采样点
%% OperationId:get_point
%% 请求:GET /iotapi/point
do_request(get_point, _Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    {200, #{<<"result">> => get_point(_Args, SessionToken)}};

%% evidence 概要: 标识采样点 描述:标识采样点
%% OperationId:post_point
%% 请求:POST /iotapi/point
do_request(post_point, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    post_point(Body, SessionToken),
    {200, #{<<"result">> => ok}};

do_request(get_capture, Args, _Context, _Req) ->
    dgiot_evidence:get_capture(Args);

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

do_report(Config, DevType, Name, SessionToken, FullPath, Uri) ->
    case dgiot_httpc:upload(Uri ++ "/dgiotproxy/dgiot_report/fileUpload", dgiot_utils:to_list(FullPath)) of
        #{<<"content">> := Content, <<"success">> := true} ->
            Url = cow_uri:urlencode(base64:encode(Content)),
            WordPreview = Uri ++ "/dgiotproxy/dgiot_report/onlinePreview?url=" ++ dgiot_utils:to_list(Url) ++ "&officePreviewType=image",
            List = dgiot_html:find(WordPreview, {<<"img">>, {<<"class">>, <<"my-photo">>}}, <<"data-src">>),
            WordUrl = Uri ++ "/dgiotproxy/dgiot_report/wordServer/"  ++ dgiot_utils:to_list(filename:basename(FullPath)),
            lists:foldl(fun(ImageUrl, Acc) ->
                case binary:split(filename:basename(ImageUrl), <<$.>>, [global, trim]) of
                    [<<"0">>, _] ->
                        Acc ++ [dgiot_evidence:create_report(Config, DevType, Name, <<"0">>, ImageUrl, WordUrl, SessionToken)];
                    [Index, _] ->
                        Acc ++ [dgiot_evidence:create_report(Config, DevType, Name, Index, ImageUrl, WordUrl, SessionToken)]
                end
                        end, [], List);
        _ ->
            []
    end.

post_point(#{
    <<"reportid">> := ReportId,
    <<"index">> := Index,
    <<"begin">> := Begin,
    <<"end">> := End
}, SessionToken) ->
    Query = #{<<"keys">> => [<<"count(*)">>, <<"original">>, <<"timestamp">>],
        <<"where">> => #{<<"$and">> => [#{
            <<"reportId">> => ReportId,
            <<"original.index">> => #{<<"$regex">> => Index}}]
        }
    },
    ?LOG(info,"Query ~p", [Query]),
    case dgiot_parse:query_object(<<"Evidence">>, Query,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Result}} when Count > 0 ->
            lists:map(fun(#{<<"objectId">> := ObjectId, <<"original">> := Original}) ->
                dgiot_parse:update_object(<<"Evidence">>, ObjectId,
                    #{<<"original">> => maps:without([<<"index">>], Original)},
                    [{"X-Parse-Session-Token", SessionToken}], [{from, rest}])
                      end, Result);
        _R ->
            pass
    end,
    Query1 = #{<<"keys">> => [<<"count(*)">>, <<"original">>, <<"timestamp">>],
        <<"where">> => #{<<"$and">> => [#{
            <<"reportId">> => ReportId,
            <<"timestamp">> => #{<<"$gte">> => Begin, <<"$lte">> => End}}]
        }
    },
    case dgiot_parse:query_object(<<"Evidence">>, Query1,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count1, <<"results">> := Result1}} when Count1 > 0 ->
            ?LOG(info,"Result1 ~p", [Result1]),
            lists:map(fun(#{<<"objectId">> := ObjectId, <<"original">> := Original}) ->
                dgiot_parse:update_object(<<"Evidence">>, ObjectId,
                    #{<<"original">> => Original#{<<"index">> => Index}},
                    [{"X-Parse-Session-Token", SessionToken}], [{from, rest}])
                      end, Result1);
        _R2 ->
            #{}
    end.

get_point(#{
    <<"reportid">> := ReportId,
    <<"index">> := 0,
    <<"begin">> := Begin,
    <<"end">> := End
}, SessionToken) ->
    Query = #{<<"keys">> => [<<"count(*)">>, <<"original">>, <<"timestamp">>],
        <<"where">> => #{<<"$and">> => [#{
            <<"original.datatype">> => #{<<"$regex">> => <<"performanceCurve">>},
            <<"reportId">> => ReportId,
            <<"timestamp">> => #{<<"$gte">> => Begin, <<"$lte">> => End}}]
        }
    },
    case dgiot_parse:query_object(<<"Evidence">>, Query,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Result}} when Count > 0 ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"original">> := #{<<"data">> := Data}, <<"timestamp">> := Time} ->
                        case maps:find(0, Acc) of
                            {ok, OldData} ->
                                #{<<"startAt">> := StartAt, <<"endAt">> := EndAt} = OldData,
                                NewData = lists:foldl(fun(Key, Acc1) ->
                                    case Key of
                                        <<"startAt">> -> Acc1;
                                        <<"endAt">> -> Acc1;
                                        _ ->
                                            Value = (maps:get(Key, Data, 0) + maps:get(Key, OldData, 0)) / 2,
                                            Acc1#{Key => Value}
                                    end
                                                      end, OldData, maps:keys(Data)),
                                Acc#{0 => NewData#{
                                    <<"startAt">> => min(StartAt, Time),
                                    <<"endAt">> => max(EndAt, Time)}
                                };
                            _ ->
                                NewData = Data#{<<"startAt">> => Time, <<"endAt">> => Time},
                                Acc#{0 => NewData}
                        end;
                    _ -> Acc
                end
                        end, #{}, Result);
        _R2 ->
            #{}
    end;

get_point(#{
    <<"reportid">> := ReportId,
    <<"index">> := 65535
}, SessionToken) ->
    Query = #{<<"keys">> => [<<"count(*)">>, <<"original">>, <<"timestamp">>],
        <<"where">> => #{<<"$and">> => [#{
            <<"original.datatype">> => #{<<"$regex">> => <<"performanceCurve">>},
            <<"reportId">> => ReportId,
            <<"original.index">> => #{<<"$regex">> => <<".+">>}}]
        }
    },
    ?LOG(info,"Query ~p", [Query]),
    case dgiot_parse:query_object(<<"Evidence">>, Query,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Result}} when Count > 0 ->
            get_point_average(Result);
        _R2 ->
            #{}
    end;

get_point(#{
    <<"reportid">> := ReportId,
    <<"index">> := Index
}, SessionToken) ->
    Query = #{<<"keys">> => [<<"count(*)">>, <<"original">>, <<"timestamp">>],
        <<"where">> => #{<<"$and">> => [#{
            <<"datatype">> => <<"performanceCurve">>,
            <<"reportId">> => ReportId,
            <<"original.index">> => Index}]
        }
    },
    ?LOG(info,"Query ~p", [Query]),
    case dgiot_parse:query_object(<<"Evidence">>, Query,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Result}} when Count > 0 ->
            get_point_average(Result);
        _R2 ->
            #{}
    end.

get_point_average(Result) ->
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"original">> := #{<<"index">> := Index, <<"data">> := Data}, <<"timestamp">> := Time} ->
                case maps:find(Index, Acc) of
                    {ok, OldData} ->
                        #{<<"startAt">> := StartAt, <<"endAt">> := EndAt} = OldData,
                        NewData = lists:foldl(fun(Key, Acc1) ->
                            case Key of
                                <<"startAt">> -> Acc1;
                                <<"endAt">> -> Acc1;
                                _ ->
                                    Value = (maps:get(Key, Data, 0) + maps:get(Key, OldData, 0)) / 2,
                                    Acc1#{Key => Value}
                            end
                                              end, OldData, maps:keys(Data)),
                        Acc#{Index => NewData#{
                            <<"startAt">> => min(StartAt, Time),
                            <<"endAt">> => max(EndAt, Time)}
                        };
                    _ ->
                        NewData = Data#{<<"startAt">> => Time, <<"endAt">> => Time},
                        Acc#{Index => NewData}
                end;
            _ -> Acc
        end
                end, #{}, Result).

get_bed(Id, SessionToken) ->
    case dgiot_parse:get_object(<<"Device">>, Id,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"parentId">> := #{<<"objectId">> := ParnetId}}} ->
            case dgiot_parse:get_object(<<"Device">>, ParnetId,
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                {ok, #{<<"basedata">> := #{<<"bedaddr">> := DtuAddr}}} ->
                    Query = #{<<"keys">> => [<<"name">>, <<"objectId">>],
                        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
                        <<"order">> => <<"devaddr">>, <<"limit">> => 256,
                        <<"include">> => <<"product">>},
                    ?LOG(info,"Query ~p ", [Query]),
                    dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
                R1 ->
                    R1
            end;
        R2 ->
            R2
    end.


delete_report(ReportId, SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"parentId">> => ReportId}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Devices}} when length(Devices) > 0 ->
            lists:map(fun(#{<<"objectId">> := ObjectId}) ->
                dgiot_parse:del_object(<<"Device">>, ObjectId,
                    [{"X-Parse-Session-Token", SessionToken}], [{from, rest}])
                      end, Devices),
            dgiot_parse:del_object(<<"Device">>, ReportId,
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        _ ->
            dgiot_parse:del_object(<<"Device">>, ReportId,
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}])
    end.


put_report(Path, SessionToken) ->
    [_, _, FileApp, FileName] = re:split(Path, <<"/">>),
    case dgiot_parse:query_object(<<"_Role">>, #{<<"where">> => #{<<"name">> => FileApp},
        <<"limit">> => 1}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"name">> := AppName, <<"config">> := Config} | _]}} ->
            Dir = maps:get(<<"home">>, Config),
            case filelib:is_dir(Dir) of
                true ->
                    Root = dgiot_evidence:get_filehome(unicode:characters_to_list(Dir)),
                    case AppName of
                        FileApp ->
                            NewPath = unicode:characters_to_list(FileApp) ++ "/" ++ unicode:characters_to_list(FileName),
                            file:delete(Root ++ "/Product.json"),
                            file:delete(Root ++ "/Device.json"),
                            file:delete(Root ++ "/Evidence.json"),
                            case dgiot_evidence:post_file(Root, NewPath) of
                                {ok, _} ->
                                    {_, R1} = dgiot_evidence:post_data(<<"Product">>, Root ++ "/Product.json"),
                                    {_, R2} = dgiot_evidence:post_data(<<"Device">>, Root ++ "/Device.json"),
                                    {_, R3} = dgiot_evidence:post_data(<<"Evidence">>, Root ++ "/Evidence.json"),
                                    {ok, #{
                                        <<"Product">> => #{<<"result">> => R1},
                                        <<"Device">> => #{<<"result">> => R2},
                                        <<"Evidence">> => #{<<"result">> => R3},
                                        <<"root">> => unicode:characters_to_binary(Root ++ "/" ++ NewPath),
                                        <<"file">> => unicode:characters_to_binary(Root ++ "/" ++ NewPath),
                                        <<"url">> => unicode:characters_to_binary(NewPath)
                                    }};
                                Error -> Error
                            end;
                        false -> {error, #{
                            <<"dir">> => unicode:characters_to_binary(unicode:characters_to_list(Dir)),
                            <<"appname">> => unicode:characters_to_binary(unicode:characters_to_list(AppName)),
                            <<"filename">> => unicode:characters_to_binary(unicode:characters_to_list(FileApp))
                        }
                        }
                    end;
                _ ->
                    {error, #{
                        <<"dir">> => unicode:characters_to_binary(unicode:characters_to_list(Dir)),
                        <<"appname">> => unicode:characters_to_binary(unicode:characters_to_list(AppName)),
                        <<"filename">> => unicode:characters_to_binary(unicode:characters_to_list(FileApp))
                    }}
            end;
        _ -> {error, <<"not find">>}
    end.


get_report(Id, SessionToken) ->
    ?LOG(info,"Id ~p SessionToken ~p", [Id, SessionToken]),
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{<<"$or">> => [#{<<"objectId">> => Id}, #{<<"parentId">> => Id}]}}) of
        {ok, #{<<"results">> := Devices}} ->
            {DAcc0, PAcc0, FAcc4} =
                lists:foldl(fun(Device, {DAcc, PAcc, FAcc}) ->
                    #{<<"objectId">> := ProductId} = maps:get(<<"product">>, Device),
                    {ok, Product} = dgiot_parse:get_object(<<"Product">>, ProductId),
                    FAcc1 =
                        case maps:get(<<"icon">>, Product, <<"">>) of
                            <<"">> -> FAcc;
                            Ico -> FAcc ++ [Ico]
                        end,
                    FAcc2 =
                        case maps:get(<<"config">>, Product, <<"">>) of
                            <<"">> -> FAcc1;
                            #{<<"layer">> := Layer} ->
                                case maps:get(<<"backgroundImage">>, Layer, <<"">>) of
                                    <<"">> -> FAcc1;
                                    ProductImage -> FAcc1 ++ [ProductImage]
                                end;
                            _ -> FAcc1
                        end,
                    FAcc3 =
                        case maps:get(<<"basedata">>, Device, <<"">>) of
                            <<"">> -> FAcc2;
                            #{<<"layer">> := Layer1} ->
                                case maps:get(<<"backgroundImage">>, Layer1, <<"">>) of
                                    <<"">> -> FAcc2;
                                    DeviceImage -> FAcc2 ++ [DeviceImage]
                                end;
                            _ -> FAcc1
                        end,
                    {DAcc ++ [maps:without([<<"createdAt">>, <<"updatedAt">>], Device)],
                            PAcc ++ [maps:without([<<"createdAt">>, <<"updatedAt">>], Product)], FAcc3}
                            end, {[], [], []}, Devices),
            case dgiot_parse:query_object(<<"Evidence">>, #{
                <<"where">> => #{<<"reportId">> => Id}}) of
                {ok, #{<<"results">> := Evidences}} ->
                    {EAcc0, FAcc0} =
                        lists:foldl(fun(Evidence, {EAcc, FAcc5}) ->
                            FAcc6 =
                                case maps:get(<<"original">>, Evidence, <<"">>) of
                                    <<"">> -> FAcc5;
                                    #{<<"data">> := Data, <<"datatype">> := DataType}
                                        when <<"liveMonitor">> =/= DataType ->
                                        case maps:get(<<"src">>, Data, <<"">>) of
                                            <<"">> -> FAcc5;
                                            Src -> FAcc5 ++ [Src]
                                        end
                                end,
                            {EAcc ++ [maps:without([<<"createdAt">>, <<"updatedAt">>], Evidence)], FAcc6}
                                    end, {[], FAcc4}, Evidences),
                    dgiot_evidence:get_report_package(Id, DAcc0, PAcc0, EAcc0, FAcc0, SessionToken);
                Error1 -> Error1
            end;
        Error -> Error
    end.

post_report(#{<<"name">> := Name, <<"devType">> := DevType,
    <<"basedata">> := Basedata}, SessionToken) ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"order">> => <<"desc">>, <<"limit">> => 1,
        <<"where">> => #{<<"devType">> => DevType, <<"nodeType">> => 1, <<"desc">> => <<"0">>}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"ACL">> := Acl,
            <<"config">> := Config, <<"objectId">> := ProductId} | _]}} ->
            case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"name">> => Name, <<"product">> => ProductId}},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                {ok, #{<<"results">> := Results}} when length(Results) == 0 ->
                    <<DtuAddr:12/binary, _/binary>> = license_loader:random(),
                    {ok, #{<<"objectId">> := ParentId}} =
                        dgiot_shadow:create_device(#{
                            <<"devaddr">> => DtuAddr,
                            <<"name">> => Name,
                            <<"ip">> => <<"">>,
                            <<"product">> => ProductId,
                            <<"ACL">> => Acl,
                            <<"status">> => <<"ONLINE">>,
                            <<"brand">> => <<"数蛙桌面采集网关"/utf8>>,
                            <<"devModel">> => <<"SW_WIN_CAPTURE">>,
                            <<"basedata">> => maps:merge(Config, Basedata)
                        }),
                    case dgiot_parse:query_object(<<"Product">>, #{<<"order">> => <<"desc">>, <<"where">> => #{
                        <<"devType">> => DevType, <<"nodeType">> => 0}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                        {ok, #{<<"results">> := Products}} ->
                            lists:foldl(fun(X, Sum) ->
                                #{
                                    <<"config">> := Config1,
                                    <<"desc">> := Desc,
                                    <<"ACL">> := Acl1,
                                    <<"objectId">> := ObjectId
                                } = X,
                                <<Addr:12/binary, _/binary>> = dgiot_license:to_md5(license_loader:random()),
                                R0 = dgiot_shadow:create_device(#{
                                    <<"devaddr">> => Addr,
                                    <<"name">> => <<Name/binary, "_", Desc/binary, ""/utf8>>,
                                    <<"ip">> => <<"">>,
                                    <<"product">> => ObjectId,
                                    <<"ACL">> => Acl1,
                                    <<"basedata">> => Config1#{<<"index">> => Sum},
                                    <<"route">> => #{DtuAddr => Addr},
                                    <<"status">> => <<"ONLINE">>,
                                    <<"brand">> => <<"数蛙桌面采集网关"/utf8>>,
                                    <<"devModel">> => <<"SW_WIN_CAPTURE">>,
                                    <<"parentId">> => #{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"Device">>,
                                        <<"objectId">> => ParentId}}),
                                ?LOG(info,"R0 ~p", [R0]),
                                Sum + 1
                                        end, 1, Products),
                            {ok, #{<<"result">> => <<"success">>}};
                        _R1 ->
                            ?LOG(info,"R1 ~p", [_R1]),
                            {error, <<"report exist">>}
                    end;
                _R2 ->
                    ?LOG(info,"R2 ~p", [_R2]),
                    {error, <<"report exist">>}
            end;
        _R3 ->
            ?LOG(info,"R3 ~p", [_R3]),
            {error, <<"report exist">>}
    end.


