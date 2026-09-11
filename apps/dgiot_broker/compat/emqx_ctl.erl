%% @doc 同名承接：emqx_ctl（刀 6 第一批）。实测 227 次调用 / 9 个组合，
%% 是插件注册 CLI 命令与打印输出的入口——机械但要齐全。
%%
%% 语义对齐 EMQX：register_command/3 注册 {Name, {Mod, Fun}, Usage}，
%% print/usage 输出到 stdout（EMQX 的 CLI 会把输出送回调用方）。
%% 未注册命令的调用显式报错（不静默）。
-module(emqx_ctl).

-export([init/0, register_command/1, register_command/3,
         unregister_command/1, commands/0, lookup/1,
         print/1, print/2, print/3, print/6, print/7,
         usage/1, usage/2, run/2]).

-define(TAB, dgiot_broker_cli_commands).

init() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, set, public,
                                   {read_concurrency, true}]);
        _ -> ?TAB
    end,
    ok.

%% 注册：{Name, {Mod, Fun}, Usage}
register_command({Name, MFA, Usage}) -> register_command(Name, MFA, Usage);
register_command(Other) -> {error, {bad_command, Other}}.

register_command(Name, {Mod, Fun} = MFA, Usage) when is_atom(Mod), is_atom(Fun) ->
    init(),
    ets:insert(?TAB, {Name, MFA, Usage}),
    ok;
register_command(Name, Fun, Usage) when is_function(Fun) ->
    init(),
    ets:insert(?TAB, {Name, Fun, Usage}),
    ok;
register_command(Name, Other, _Usage) ->
    {error, {bad_command_handler, Name, Other}}.

unregister_command(Name) ->
    init(),
    ets:delete(?TAB, Name),
    ok.

commands() -> init(), [N || {N, _H, _U} <- ets:tab2list(?TAB)].

lookup(Name) ->
    init(),
    case ets:lookup(?TAB, Name) of
        [{Name, H, U}] -> {ok, H, U};
        [] -> {error, not_found}
    end.

%% 执行已注册命令
run(Name, Args) ->
    case lookup(Name) of
        {ok, {Mod, Fun}, _U} -> Mod:Fun(Args);
        {ok, Fun, _U} when is_function(Fun) -> Fun(Args);
        {error, Reason} ->
            print("Command not found: ~s", [Name]),
            {error, Reason}
    end.

%% ---- 输出（EMQX 的 CLI 语义：写到标准输出） ----
print(Msg) -> print("~s", [Msg]).

print(Format, Args) ->
    io:format(Format ++ "~n", Args),
    ok.

print(Prefix, Format, Args) ->
    io:format("~s " ++ Format ++ "~n", [Prefix | Args]),
    ok.

%% EMQX 里 print/6、print/7 是带颜色/标签的变体；这里降级为统一输出并
%% **显式标注降级**（不假装支持颜色）
print(P1, P2, P3, P4, P5, P6) ->
    io:format("~s ~s ~s ~s ~s ~s~n", [P1, P2, P3, P4, P5, P6]),
    ok.

print(P1, P2, P3, P4, P5, P6, P7) ->
    io:format("~s ~s ~s ~s ~s ~s ~s~n", [P1, P2, P3, P4, P5, P6, P7]),
    ok.

usage(Command) -> usage(Command, []).

usage(Command, Args) ->
    case lookup(Command) of
        {ok, _H, Usage} -> print("~s", [Usage]);
        {error, _} -> print("Unknown command: ~s", [Command])
    end,
    case Args of
        [] -> ok;
        _ -> print("  (args: ~p)", [Args])
    end.
