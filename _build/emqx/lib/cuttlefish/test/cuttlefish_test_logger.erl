%% A logger mock for testing
-module(cuttlefish_test_logger).

-compile({no_auto_import, [error/1]}).
-compile({no_auto_import, [error/2]}).

-export([bounce/0, bounce/1, get_logs/0]).
-export([debug/1, debug/2, error/1, error/2, info/1, info/2, warning/1, warning/2]).

bounce() -> bounce(error).

bounce(Level) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(
      fun() ->
              try
                  erlang:register(?MODULE, self())
              catch
                  _ : _->
                      Parent ! {init_ack, Ref, whereis(?MODULE)},
                      exit(normal)
              end,
              Parent ! {init_ack, Ref, whereis(?MODULE)},
              loop()
      end),
    Pid = receive {init_ack, Ref, P} -> P
          after 1000 -> error(timeout) end,
    true = is_pid(Pid),
    Pid ! {bounce, Level},
    ok.

get_logs() ->
    Ref = make_ref(),
    ?MODULE ! {get_logs, Ref, self()},
    receive {Ref, Logs} -> Logs
    after 5000 -> error(timeout)
    end.

debug(Msg) -> debug(Msg, []).
error(Msg) -> error(Msg, []).
info(Msg) -> info(Msg, []).
warning(Msg) -> warning(Msg, []).

debug(Fmt, Args) -> ?MODULE ! {log, debug, Fmt, Args}, ok.
error(Fmt, Args) -> ?MODULE ! {log, error, Fmt, Args}, ok.
info(Fmt, Args) -> ?MODULE ! {log, info, Fmt, Args}, ok.
warning(Fmt, Args) -> ?MODULE ! {log, warning, Fmt, Args}, ok.

%% ===================== internals ================

loop() ->
    receive
        {bounce, Level} ->
            loop(Level, [])
    end.

loop(Level, Logs) ->
    receive
        {bounce, NewLevel} ->
            loop(NewLevel, []);
        {get_logs, Ref, Pid} ->
            Pid ! {Ref, lists:reverse(Logs)},
            loop(Level, Logs);
        {log, LogLevel, Fmt, Args} ->
            case Level =:= LogLevel of
                true ->
                    Line = depp(iolist_to_binary(io_lib:fwrite(Fmt, Args))),
                    loop(Level, [Line | Logs]);
                false ->
                    loop(Level, Logs)
            end
    end.

depp(X) ->
    Lines = binary:split(X, <<"\n">>, [global]),
    F = fun(Line) -> string:strip(binary_to_list(Line)) end,
    iolist_to_binary(join(lists:map(F, Lines))).

join([]) -> [];
join([X]) -> X;
join([A | B]) -> [A | [[" ", I] || I <- B]].

