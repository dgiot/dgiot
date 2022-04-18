%%%-------------------------------------------------------------------
%%% @author dgiot
%%% @copyright (C) 2019, dgiot
%%% @doc
%%% API 处理模块 产生时间: Mon, 27 Apr 2020 12:54:08 +0800
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_mysql_handler).
-author("dgiot").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include("dgiot_mysql.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_tdengine/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/tdengine">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_tdengine.json">>, ?MODULE, [], priv)
swagger_tdengine() ->
    [
        dgiot_http_server:bind(<<"/swagger_mysql.json">>, ?MODULE, [], priv)
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
            %?LOG(info,"do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> list_to_binary(io_lib:format("~p", [Reason]))
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            %?LOG(debug,"do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            %?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            %?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            %?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            %?LOG(debug,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% get_test_info_no
do_request(get_mysql_table, #{<<"table">> := Table} = Args,
        #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    Query = maps:without([<<"table">>], Args),
    Where = case maps:get(<<"where">>, Args, undefined) of
                undefined -> #{};
                <<>> -> #{};
                Bin -> jsx:decode(Bin, [{labels, binary}, return_maps])
            end,
    Filter = #{<<"keys">> => [<<"name">>, <<"objectId">>],
        <<"where">> => #{<<"cType">> => ?TYPE}},
    case dgiot_parse:query_object(<<"Channel">>, Filter,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Result}} when length(Result) == 1 ->
            [#{<<"objectId">> := Channel} | _] = Result,
            dgiot_mysql:query_object(Channel, Table, Query#{<<"where">> => Where});
        {ok, #{<<"results">> := Result}} when length(Result) > 1 ->
            {error, <<"too many channel,pls use get_classes_cid_table api">>};
        _ ->
            {error, <<"not find channel">>}
    end;

do_request(get_show_tables, #{<<"filter">> := Filter}, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info,"Filter ~p", [Filter]),
    Sql = <<"show tables like \'", Filter/binary, "\'">>,
    {ok, _, R} = dgiot_mysql:query(Sql, SessionToken, from_rest),
    R1 = lists:foldl(fun(X, Acc) ->
        Acc ++ X
                     end, [], R),
    {ok, #{<<"result">> => R1}};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.