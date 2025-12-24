%%%-------------------------------------------------------------------
%%% @author stoneliu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2022 21:53
%%%-------------------------------------------------------------------
-module(dgiot_serial_port).
-author("stoneliu").

%% API
-export([test/2]).
-export([init/1]).

%%  dgiot_serial_port:test(<<"/dev/ttyUSB0">>, <<"9600">>).
%%  FD = srly:open(<<"/dev/ttyS0">>)
%%  srly:write(Fd, <<1,3,7,208,0,2,196,134>>);
test(SerialPort, BaudRate) ->
    B = dgiot_utils:to_atom(<<"b", BaudRate/binary>>),
    srly:open(SerialPort, [{speed, B}]),
    receive
        A ->
            io:format("~s ~p A = ~p.~n", [?FILE, ?LINE, A])
%%        {serial_open, FD} ->
%%            io:format("~s ~p FD = ~p.~n", [?FILE, ?LINE, FD]),
%%            srly:write(FD, <<"12223">>);
%%        {serial_data, _, {Pid, FD, Data}} ->
%%            io:format("~s ~p FD = ~p.~n", [?FILE, ?LINE, FD]),
%%            io:format("~s ~p Data = ~p.~n", [?FILE, ?LINE, Data]),
%%            srly:write(FD, <<"12223">>),
%%            srly:close(FD)
    after 10000 ->
        io:format("~s ~p after ~n", [?FILE, ?LINE]),
        exit(self(), kill),
        {error, eintr}
    end.

init(Values) ->
    SerialPort = maps:get(<<"serialport">>, Values, <<"usb_north_top">>),
    dgiot_serial_client:open(SerialPort,
        Values#{
            <<"hupcl">> => true,
            <<"ignbrk">> => true,
            <<"ignpar">> => false
        }).




