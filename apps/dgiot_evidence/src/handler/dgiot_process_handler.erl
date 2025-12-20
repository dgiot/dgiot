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

-module(dgiot_process_handler).
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
    [dgiot_http_server:bind(<<"/swagger_process.json">>, ?MODULE, [], priv)].


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
                      false -> dgiot_utils:format("~p", [Reason])
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


%% process 概要: 获取流程 描述:获取流程
%% OperationId:get_process
%% 请求:GET /iotapi/process
do_request(get_process, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    % io:format("~s ~p Body ~p ~n", [?FILE, ?LINE, Body]),
    Skip =
        case maps:find(<<"skip">>, Body) of
            {ok, Value1} when Value1 =/= undefined ->
                Value1;
            _ ->
                0
        end,
    Limit =
        case maps:find(<<"limit">>, Body) of
            {ok, Value2} when Value2 =/= undefined ->
                Value2;
            _ ->
                100
        end,
    Type =
        case maps:find(<<"type">>, Body) of
            {ok, Value3} when Value3 =/= undefined ->
                Value3;
            _ ->
                undefined
        end,
    Where =
        case maps:find(<<"where">>, Body) of
            {ok, Value4} when Value4 =/= undefined ->
                io:format("~s ~p Where ~p ~n", [?FILE, ?LINE, Value4]),
                jsx:decode(Value4, [return_maps]);
            _ ->
                #{}
        end,
    Order =
        case maps:find(<<"order">>, Body) of
            {ok, Value5} when Value5 =/= undefined ->
                io:format("~s ~p Order ~p ~n", [?FILE, ?LINE, Value5]),
                Value5;
            _ ->
                <<"createdAt">>
        end,
    From = maps:get(<<"from">>, Body, <<"">>),

    Result = get_process(Skip, Limit, Type, Where, Order, From, SessionToken),
    % io:format("~s ~p Result ~p ~n", [?FILE, ?LINE, Result]),
    {ok, Result};

do_request(get_process_id, _Body, #{<<"sessionToken">> := SessionToken} = _Context, Req) ->
    % io:format("~s ~p Body ~p ~n", [?FILE, ?LINE, _Body]),
    Id = dgiot_req:binding(<<"id">>, Req),
    % io:format("~s ~p Body ~p Id: ~p ~n", [?FILE, ?LINE, _Body, Id]),
    get_process(Id, SessionToken);

%% process 概要: 发起流程 描述:发起流程
%% OperationId:post_process
%% 请求:POST /iotapi/process
do_request(post_process, Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "Body ~p ", [Body]),
    post_process(Body, SessionToken);

%% process 概要: 审批流程 描述:审批流程
%% OperationId:put_process_id
%% 请求:PUT /iotapi/process
do_request(put_process_id, #{<<"paramList">> := ParamList} = _Body, #{<<"sessionToken">> := SessionToken} = _Context, Req) ->
    Id = dgiot_req:binding(<<"id">>, Req),
    % TerminalType = maps:get(<<"terminalType">>, Body, <<"pc">>),
    put_process(Id, ParamList, SessionToken);

%% process 概要: 撤销流程 描述:撤销流程
%% OperationId:delete_process_id
%% 请求:GET /iotapi/process
do_request(delete_process_id,
           #{<<"id">> := ProcessId} = _Args,
           #{<<"sessionToken">> := SessionToken} = _Context,
           _Req) ->
    delete_process(ProcessId, SessionToken);

do_request(get_process_info, _Args, _Context, _Req) ->
    io:format("~s ~p ~p~n", [?FILE, ?LINE, _Args]),
    case dgiot_parse:query_object(<<"Product">>, #{<<"count">> => <<"objectId">>, <<"limit">> => 100, <<"where">> => #{<<"devType">> => <<"COSL">>}}) of
        {ok, #{<<"results">> := ProductList}} when length(ProductList) > 0 ->
            Results = lists:foldl(fun(Product, Acc) ->
                                          Item = maps:with([<<"objectId">>, <<"name">>], Product),
                                          [Item | Acc]
                                  end,
                                  [],
                                  ProductList),

            AddrList = dgiot_evidence:get_areas(),
            LevelList = dgiot_evidence:get_levels(),

            Result = #{<<"products">> => Results, <<"areas">> => AddrList, <<"levels">> => LevelList},

            {ok, #{code => 0, <<"result">> => Result}};
        Error ->
            {error, #{code => 1, <<"error">> => Error}}
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    io:format("~s ~p ~p~n", [?FILE, ?LINE, _OperationId]),
    {error, <<"Not Allowed.">>}.


%% 发起流程
post_process(#{<<"product">> := ProductId, <<"dept">> := Department, <<"level">> := LevelName, <<"work_area">> := WorkArea, <<"date">> := Date} = _Args, SessionToken) ->

    % io:format("~s ~p ~p~n", [?FILE, ?LINE, Args]),
    % <<DtuAddr:12/binary, _/binary>> = dgiot_utils:random(),
    % DevAddr = maps:get(<<"devaddr">>, Args, DtuAddr),

    dgiot_process:post_process(ProductId, Department, LevelName, WorkArea, Date, SessionToken);

post_process(_Args, _SessionToken) ->
    io:format("~s ~p ~p~n", [?FILE, ?LINE, _Args]),
    {error, <<"Args Error">>}.


%% 撤销流程
delete_process(ProcessId, SessionToken) ->
    io:format("~s ~p ProcessId ~p SessionToken ~p~n", [?FILE, ?LINE, ProcessId, SessionToken]),
    Result = dgiot_parse:del_object(<<"Device">>, ProcessId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
    io:format("~s ~p Result ~p~n", [?FILE, ?LINE, Result]),
    case Result of
        ok ->
            {ok, #{<<"result">> => <<"success">>}};
        _ ->
            {error, #{<<"result">> => <<"failed">>, <<"msg">> => <<"撤销流程失败">>}}
    end.


%% 审批流程
put_process(Id, ParamList, SessionToken) ->
    io:format("~s ~p Id ~p SessionToken ~p ~n", [?FILE, ?LINE, Id, SessionToken]),
    % dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"ACL">> => Acl, <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => dgiot_utils:to_float(Longitude), <<"latitude">> => dgiot_utils:to_float(Latitude)}}),
    dgiot_process:put_process(Id, ParamList, SessionToken).


%% 获取流程
get_process(Id, SessionToken) ->
    io:format("~s ~p Id ~p SessionToken ~p~n", [?FILE, ?LINE, Id, SessionToken]),
    dgiot_process:get_process(Id, SessionToken).


get_process(Skip, Limit, Type, Where, Order, From, SessionToken) ->
    io:format("~s ~p ~p~n", [?FILE, ?LINE, Skip]),
    % 根据分类id查产品列表，写死流程管理的分类id
    dgiot_process:get_process(Skip, Limit, Type, Where, Order, From, SessionToken).
