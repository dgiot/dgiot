-module(gun_http2_test).
-include_lib("eunit/include/eunit.hrl").

handle_go_away_test() ->
	State = gun_http2:init(self(), socket, fake_transport, #{}),
	Data = <<0,0,22,7,0,0,0,0,0,0,0,0,0,0,0,0,11,116,111,111,95,109,97,110,121,95,112,105,110,103,115>>,
	Result = gun_http2:handle(Data, State),
	?assertEqual(close, Result).
