%% @doc 认证钩子（刀 3）：把"谁可以连"变成一个显式可插拔决策。
%%
%% 配置（application env）：
%%   {auth, #{mode => allow_all}}                      %% 默认：实验室放行（但记账）
%%   {auth, #{mode => callback, module => my_auth}}    %% 回调：Mod:authenticate/3
%%
%% 铁律：任何异常都变成显式拒绝（{error, {auth_callback_failed, _}}），
%% 绝不"回调挂了就当放行"——这正是要替换掉的那类静默缺陷。
%% 每次决策都记日志（可观测），拒绝带原因。
-module(dgiot_broker_auth).

-export([authenticate/3, policy/0]).

-spec authenticate(binary() | undefined, binary() | undefined,
                   binary() | undefined) -> ok | {error, term()}.
authenticate(ClientId, Username, Password) ->
    case validate_clientid(ClientId) of
        {error, Reason} ->
            deny(ClientId, Username, Reason);
        ok ->
            case policy() of
                #{mode := allow_all} ->
                    allow(ClientId, Username, allow_all);
                #{mode := callback, module := Mod} ->
                    run_callback(Mod, ClientId, Username, Password);
                Other ->
                    deny(ClientId, Username, {bad_auth_policy, Other})
            end
    end.

policy() ->
    case application:get_env(dgiot_broker, auth) of
        {ok, P} when is_map(P) -> P;
        _ -> #{mode => allow_all}
    end.

%% MQTT 3.1.1：空 ClientId 仅允许 clean_session=1（此处只做非空校验，
%% clean 语义由连接进程结合 CONNECT 标志判定）
validate_clientid(<<>>) -> {error, empty_client_id};
validate_clientid(undefined) -> {error, no_client_id};
validate_clientid(ClientId) when byte_size(ClientId) > 65535 ->
    {error, client_id_too_long};
validate_clientid(_) -> ok.

run_callback(Mod, ClientId, Username, Password) ->
    try Mod:authenticate(ClientId, Username, Password) of
        ok -> allow(ClientId, Username, {callback, Mod});
        {error, Reason} -> deny(ClientId, Username, Reason);
        Other -> deny(ClientId, Username, {bad_callback_return, Other})
    catch
        Class:Reason:Stack ->
            deny(ClientId, Username,
                 {auth_callback_failed, Class, Reason, hd(Stack)})
    end.

allow(ClientId, Username, How) ->
    logger:notice("[broker-auth] allow client=~s user=~p via ~p",
                  [ClientId, Username, How]),
    ok.

deny(ClientId, Username, Reason) ->
    logger:warning("[broker-auth] DENY client=~p user=~p reason=~p",
                   [ClientId, Username, Reason]),
    {error, Reason}.
