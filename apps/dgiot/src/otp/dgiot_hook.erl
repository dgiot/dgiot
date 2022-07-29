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
-module(dgiot_hook).

-author("johnliu").
-export([add/2, add/3, remove/1, run_hook/2]).
-include_lib("dgiot/include/logger.hrl").

%% HOOK方式
%% one_for_one : 一个key对应一个回调，后面新加的hook会覆盖前端的
%% one_for_more: 一个key对应多个回调

add(Key, Fun) ->
    add(one_for_more, Key, Fun).

add(HookType, Key, Fun) ->
    Hooks = get_hooks(),
    New = case HookType of
    	one_for_more ->
    	    Old = maps:get(Key, Hooks, []),
    	    [Fun|Old];
    	one_for_one ->
    	    [Fun]
    end,
    dgiot_data:insert(dgiot_hook, Hooks#{ Key => New }).

remove(Key) ->
    Hooks = get_hooks(),
	NewHooks = maps:remove(Key, Hooks),
	dgiot_data:insert(dgiot_hook, NewHooks).

run_hook(Key, Args) ->
	Funs = get_hooks(Key),
	case Funs of
		not_find ->
			{error, not_find};
		_ ->
			{_NewFuns, Rtns} = lists:foldl(
				fun(Fun, {Acc, R}) ->
					case catch (do_hook(Fun, Args)) of
						{'EXIT', Reason} ->
							?LOG(error,"do hook error, Args:~p -> ~p~n", [Args, Reason]),
							{Acc, R};
						Result ->
							{[Fun | Acc], [Result | R]}
					end
				end, {[], []}, Funs),
			{ok, Rtns}
	end.

do_hook({M, F}, Args) when is_list(Args) ->
    apply(M, F, Args);

do_hook({M, F}, Arg) ->
    do_hook({M, F}, [Arg]);

do_hook(Fun, Args) ->
    Fun(Args).

get_hooks(Key) ->
	Hooks = get_hooks(),
	maps:get(Key, Hooks, not_find).

get_hooks() ->
	case dgiot_data:lookup(dgiot_hook) of
		{ok, Hooks} -> Hooks;
		{error, not_find} -> #{}
	end.
