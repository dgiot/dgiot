%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_parse_cache).
-author("johnliu").
-include_lib("eunit/include/eunit.hrl").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([save_cache/1, save_to_cache/1, save_to_cache/2, save_test/1]).

-export([add_hook/2, do_hook/3]).

add_hook(Class, Method) ->
    dgiot_hook:add({Class, Method}, {?MODULE, do_hook}).

do_hook(_TriggerName, _Path, Body) ->
    case jsx:is_json(Body) of
        true ->
            Data = jsx:decode(Body, [return_maps]),
            ?LOG(info,"_TriggerName ~p Data ~p", [_TriggerName,Data]);
        false ->
            pass
    end,
    {ok, Body}.

%% 先缓存定时存库
save_to_cache(Requests) ->
    save_to_cache(?DEFAULT, Requests).
save_to_cache(Channel, Requests) when is_list(Requests) ->
    ETS = ?CACHE(Channel),
    timer:sleep(2),
    Key = erlang:system_time(millisecond),
    NewRequest =
        case dgiot_dcache:lookup(ETS, Key) of
            {ok, Acc} ->
                lists:foldl(fun(Request, Acc1) -> [Request | Acc1] end, Acc, Requests);
            {error, not_find} ->
                Requests
        end,
    dgiot_dcache:insert(?CACHE(Channel), {Key, NewRequest}),
    case check_cache(Channel) of
        true ->
            dgiot_channelx:do_event(?TYPE, Channel, full, self()),
            ok;
        false ->
            ok
    end;
save_to_cache(Channel, Request) when is_map(Request) ->
    save_to_cache(Channel, [Request]).


check_cache(Channel) ->
    Info = dgiot_dcache:info(?CACHE(Channel)),
    {size, Size} = lists:keyfind(size, 1, Info),
    case Size >= 200 of
        true ->
            true;
        false ->
            {memory, Memory} = lists:keyfind(memory, 1, Info),
            MaxSize = application:get_env(dgiot_parse, cache_max_size, 50000),
            MaxMemory = application:get_env(dgiot_parse, cache_max_memory, 102400),
            Size >= MaxSize orelse Memory >= MaxMemory
    end.

save_cache(Channel) ->
    Fun =
        fun({Idx, Requests}, Acc) ->
            save_cache(Idx, Channel, Requests, Acc)
        end,
    save_to_parse(Channel, dgiot_dcache:search(?CACHE(Channel), Fun)).

save_cache(Idx, Channel, Requests, Acc) ->
    true = dgiot_dcache:delete(?CACHE(Channel), Idx),
    NewRequests = lists:foldl(fun(Request, Acc1) -> [Request | Acc1] end, Acc, Requests),
    case length(NewRequests) < 1000 of
        true ->
            {true, NewRequests};
        false ->
            ok = save_to_parse(Channel, NewRequests),
            {true, []}
    end.

save_to_parse(_, []) -> ok;
save_to_parse(Channel, Requests) ->
    case dgiot_parse:batch(Requests) of
        {ok, Results} ->
            dgiot_metrics:inc(dgiot_parse, <<"parse_save_cache">>, length(Requests)),
            do_result(Requests, Results);
        {error, Reason} ->
            ?LOG(error,"save cache,~p,~p~n", [Requests, Reason]),
            save_to_cache(Channel, Requests),
            ok
    end.

do_result([], []) ->
    ok;
do_result([Request | Requests], [Result | Results]) ->
    case maps:get(pid, Request, no) of
        no ->
            log(Request, Result);
        Pid ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                false ->
                    log(Request, Result);
                true ->
                    Pid ! {batch, maps:remove(pid, Request), Result}
            end
    end,
    do_result(Requests, Results).

log(_Request, #{<<"success">> := _}) -> ok;
log(Request, #{<<"error">> := Error}) ->
    ?LOG(error,"save ~p, cache,~p~n", [Request, Error]).


save_test(Count) ->
    [save_to_cache(#{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Test">>,
        <<"body">> => #{
            <<"i">> => I
        }
    }) || I <- lists:seq(1, Count)], ok.

