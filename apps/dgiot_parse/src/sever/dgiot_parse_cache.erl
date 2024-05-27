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
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-dgiot_data("ets").
-export([test/0, init_ets/0, start_cache/1, cache_classes/0]).
-export([do_save/1, save_to_cache/1, save_to_cache/2, save_test/1]).

init_ets() ->
    dgiot_data:init(?DGIOT_PARSE_ETS),
    dgiot_data:init(?ROLE_ETS),
    dgiot_data:init(?USER_ETS),
    dgiot_data:init(?ROLE_USER_ETS),
    dgiot_data:init(?USER_ROLE_ETS),
    dgiot_data:init(?ROLE_VIEWS_ETS),
    dgiot_data:init(?ROLE_MENUVIEWS_ETS),
    dgiot_data:init(?ROLE_PARENT_ETS),
    dgiot_data:init(?NAME_ROLE_ETS),
    dgiot_data:init(?ROLE_NAME_ETS),
    dgiot_data:init(?CLASS_COUNT_ETS),
    dgiot_data:init(?PARENT_ROLE_ETS, [public, named_table, bag, {write_concurrency, true}, {read_concurrency, true}]).

start_cache(_Pid) ->
    dgiot_role:load_roles(),
    dgiot_role:load_user(),
    dgiot_parse_auth:load_roleuser().
%%    cache_classes().

%% todo
cache_classes() ->
    case dgiot_parse:get_schemas() of
        {ok, #{<<"results">> := Results}} ->
            lists:map(fun(#{<<"className">> := CLasseName}) ->
                case catch dgiot_hook:run_hook(dgiot_utils:to_atom("parse_cache_" ++ dgiot_utils:to_list(CLasseName)), CLasseName) of
                    {ok, _} ->
                        pass;
                    _ ->
                        pass
                end
                      end, Results);
        _ ->
            pass
    end.

%% 先缓存定时存库
save_to_cache(Requests) ->
    save_to_cache(?DEFAULT, Requests).
save_to_cache(Channel, Requests) when is_list(Requests) ->
    dgiot_dcache:insert(?CACHE(Channel), Requests),
    case check_cache(Channel) of
        true ->
            dgiot_dcache:save(?CACHE(Channel)),
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

do_save(Channel) ->
    Fun =
        fun({Idx, Requests}, Acc) ->
            do_save(Idx, Channel, Requests, Acc)
        end,
    save_to_parse(Channel, dgiot_dcache:search(?CACHE(Channel), Fun)).

do_save(Idx, Channel, Requests, Acc) ->
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
    case dgiot_parse:batch(Channel, Requests) of
        {ok, Results} ->
            dgiot_metrics:inc(dgiot_parse, <<"parse_save_success">>, length(Requests)),
            do_result(Requests, Results);
        %%  错误报文应该丢弃，不是所有报文都应该重新缓存
        #{<<"code">> := 100, <<"error">> := _Error} ->
            dgiot_metrics:inc(dgiot_parse, <<"parse_save_fail">>, length(Requests));
        Result ->
            log(Requests, Result),
            dgiot_metrics:inc(dgiot_parse, <<"parse_save_fail">>, length(Requests)),
            save_to_cache(Channel, Requests)
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
    io:format("save ~p, cache,~p~n", [Request, Error]).


test() ->
    Fun3 = fun
               ({_, _, ['Device' | _]} = X) ->
                   io:format("~p~n", [X]);
               ({_, _, [_ClassesName | _]}) ->
                   pass
           end,
    dgiot_mnesia:search(Fun3, #{}).

save_test(Count) ->
    [save_to_cache(#{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Test">>,
        <<"body">> => #{
            <<"i">> => I
        }
    }) || I <- lists:seq(1, Count)], ok.