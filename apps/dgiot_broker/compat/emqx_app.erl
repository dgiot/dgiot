%% @doc 同名承接：emqx_app（版本信息）。
%% 管理 CLI 用 get_release/0 打印节点版本串；返回本 broker 的发布标识。
-module(emqx_app).

-export([get_release/0]).

get_release() ->
    case application:get_key(emqx, vsn) of
        {ok, Vsn} when is_list(Vsn) -> Vsn;
        _ -> "4.9.3-dgiot"
    end.
