%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2019 14:56
%%%-------------------------------------------------------------------
-module(license_trigger).
-author("kenneth").

%% API
-export([do/1]).


do(Req0) ->
    {ok, Body, Req} = dgiot_req:read_body(Req0),
    Data = jsx:decode(Body, [{labels, binary}, return_maps]),
    io:format(" ~p~n", [Data]),
    Req1 = dgiot_req:reply(500, #{
        <<"content-type">> => <<"application/json; charset=utf-8">>
    }, jsx:encode(#{error => <<>>}), Req),
    {ok, Req1}.
