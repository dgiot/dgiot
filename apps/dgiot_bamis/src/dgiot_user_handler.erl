%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_user_handler).
-author("h7ml").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_user/0]).
-export([handle/4]).
%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/amis">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_view.json">>, ?MODULE, [], priv)
swagger_user() ->
    [
        dgiot_http_server:bind(<<"/swagger_user.json">>, ?MODULE, [], priv)  %需要于priv/swagger/目录下放置swagger.json文件名保持一致
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
                      false -> dgiot_utils:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            ?LOG(debug, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================


%%%===================================================================
%%% 所有配置在 amis 组件中的接口，都要符合下面的返回格式
%% 适配amis接口参数
%% {
%%   "status": 0,
%%   "msg": "",
%%   "data": {
%%     ...其他字段
%%   }
%% }
%% status: 返回 0，表示当前接口正确返回，否则按错误请求处理；
%% msg: 返回接口处理信息，主要用于表单提交或请求失败时的 toast 显示；
%% data: 必须返回一个具有 key-value 结构的对象。
%% https://aisuda.bce.baidu.com/amis/zh-CN/docs/types/api#%E6%8E%A5%E5%8F%A3%E8%BF%94%E5%9B%9E%E6%A0%BC%E5%BC%8F-%E9%87%8D%E8%A6%81-
%%%===================================================================

%% https://www.showdoc.com.cn/1942734591393597/8834826658323785
%% iot_hub 概要: 通过当前登录人员token（例如登录用户为生产报工部门员工） 查询获取当前用户所有同级部门及其子部门所有的用户的人员树
%% OperationId:post_dashboard
%% 请求:POST /iotapi/post_dashboard
do_request(get_usertree,_Args, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    User =
        case dgiot_data:get(get_roletree) of
            not_find ->
                P = get_roletree(SessionToken),
                dgiot_data:insert(get_roletree, P);
            P ->
                P
        end,
    {200, User};


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    ?LOG(info, "_OperationId:~p~n", [_OperationId]),
    {error, <<"Not Allowed.">>}.


%% todo 后面优化懒加载
get_roletree(SessionToken) ->
    case dgiot_parse:query_object(<<"_Role">>, #{}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Roles}} when length(Roles) > 0 ->
            case dgiot_auth:get_session(SessionToken) of
                #{<<"roles">> := UserRoles} ->
                    Keys = maps:keys(UserRoles),
                    KeyRoles = lists:filter(fun(#{<<"objectId">> := RoleId}) ->
                        lists:member(RoleId, Keys)
                                            end, Roles),
                    [ParentRoleId, _] =
                        lists:foldl(fun(#{<<"objectId">> := RoleId, <<"level">> := Level}, Acc) ->
                            case Acc of
                                [] -> [RoleId, Level];
                                [OldRoleId, OldLevel] ->
                                    case Level < OldLevel of
                                        true -> [RoleId, Level];
                                        false -> [OldRoleId, OldLevel]
                                    end
                            end
                                    end, [], KeyRoles),
                    NewRoles =
                        lists:foldl(fun(#{<<"objectId">> := RoleId} = Role, Acc) ->
                            case RoleId of
                                ParentRoleId ->
                                    Acc ++ [Role#{<<"parent">> => <<"0">>}];
                                _ -> Acc ++ [Role]
                            end
                                    end, [], Roles),
                    RoleTree = dgiot_parse_utils:create_tree(NewRoles, <<"parent">>),
                    Len = length(RoleTree),
                    Num = 1000,
                    case Len =< Num of
                        true ->
                            {200, #{<<"results">> => RoleTree}};
                        false ->
                            ShowMenus = lists:sublist(RoleTree, 1, Num),
                            MoreMenus = lists:sublist(RoleTree, Num + 1, length(RoleTree) - Num),
                            {200, #{<<"results">> => ShowMenus ++ [#{
                                <<"children">> => MoreMenus,
                                <<"name">> => <<"...">>,
                                <<"icon">> => <<"More">>,
                                <<"parent">> => <<"0">>,
                                <<"url">> => <<"/more">>
                            }]}}
                    end;
                _ -> {error, <<"1 not find">>}
            end;
        _ -> {error, <<"2 not find">>}
    end.
