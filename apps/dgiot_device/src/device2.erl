%%%-------------------------------------------------------------------
%%% @author h7ml
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 7月 2021 10:23
%%%-------------------------------------------------------------------
-module(device2).
-author("h7ml").

%% API
-export([updata_data/0]).
-include_lib("dgiot/include/logger.hrl").


updata_data() ->
    Success = fun(Page) -> io:format("~p ~n", [Page])
%%    ?LOG(error, "~p", [Page])
%%                            dgiot_parse:update_object(<<"Device">>, DeviceId, #{})
              end,
    Success = fun(Page) -> io:format("~p ~n", [Page]) end,
    Query = #{<<"where">> => #{}},
    dgiot_parse_loader:start(<<"Device">>, Query, 0, 100, 20000, Success),
    ok.
