%% @doc 同名承接：emqx_calendar（时间工具）→ 自包含实现。
-module(emqx_calendar).

-export([offset_second/1, parse/3, now_to_second/0]).

%% 时区偏移秒（EMQX 语义）：返回本地相对 UTC 的秒差（简单实现）
offset_second(Zone) ->
    try
        %% 仅支持 "UTC" / "+HH:MM" / "-HH:MM" 形态；其余显式报错
        parse_zone(Zone)
    catch
        _:_ -> {error, {bad_zone, Zone}}
    end.

parse_zone(utc) -> 0;
parse_zone(<<"UTC">>) -> 0;
parse_zone(<<"+", H1, H2, ":", M1, M2>>) ->
    ((H1 - $0) * 10 + (H2 - $0)) * 3600 + ((M1 - $0) * 10 + (M2 - $0)) * 60;
parse_zone(<<"-", H1, H2, ":", M1, M2>>) ->
    -(((H1 - $0) * 10 + (H2 - $0)) * 3600 + ((M1 - $0) * 10 + (M2 - $0)) * 60);
parse_zone(Other) -> erlang:error({bad_zone, Other}).

%% parse/3：占位——EMQX 里是 cron/窗口解析，本项目未用；显式声明
parse(_Unit, _Field, _Value) ->
    {error, {not_implemented, cut7, calendar_parse}}.

now_to_second() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
