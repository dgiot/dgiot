%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_retainer).

-behaviour(gen_server).

-include("emqx_retainer.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/1]).

-export([ load/1
        , unload/0
        ]).

-export([ on_session_subscribed/3
        , on_message_publish/2
        ]).

-export([clean/1]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {stats_fun, stats_timer, expiry_timer}).

%%--------------------------------------------------------------------
%% Load/Unload
%%--------------------------------------------------------------------

load(Env) ->
    emqx:hook('session.subscribed', fun ?MODULE:on_session_subscribed/3, []),
    emqx:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]).

unload() ->
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    emqx:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/3).

on_session_subscribed(_, _, #{share := ShareName}) when ShareName =/= undefined ->
    ok;
on_session_subscribed(_, Topic, #{rh := Rh, is_new := IsNew}) ->
    if
        Rh =:= 0 orelse (Rh =:= 1 andalso IsNew) ->
            gen_server:cast(?MODULE, {dispatch, self(), Topic});
        true -> ok
    end.

%% RETAIN flag set to 1 and payload containing zero bytes
on_message_publish(Msg = #message{flags   = #{retain := true},
                                  topic   = Topic,
                                  payload = <<>>}, _Env) ->
    mnesia:dirty_delete(?TAB, Topic),
    {ok, Msg};

on_message_publish(Msg = #message{flags = #{retain := true}}, Env) ->
    Msg1 = emqx_message:set_header(retained, true, Msg),
    store_retained(Msg1, Env),
    {ok, Msg};
on_message_publish(Msg, _Env) ->
    {ok, Msg}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Start the retainer
-spec(start_link(Env :: list()) -> emqx_types:startlink_ret()).
start_link(Env) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Env], []).

clean(Topic) when is_binary(Topic) ->
    case emqx_topic:wildcard(Topic) of
        true -> match_delete_messages(Topic);
        false ->
            Fun = fun() ->
                      case mnesia:read({?TAB, Topic}) of
                          [] -> 0;
                          [_M] -> mnesia:delete({?TAB, Topic}), 1
                      end
                  end,
            {atomic, N} = mnesia:transaction(Fun), N
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Env]) ->
    Copies = case proplists:get_value(storage_type, Env, disc) of
                 ram       -> ram_copies;
                 disc      -> disc_copies;
                 disc_only -> disc_only_copies
             end,
    StoreProps = [{ets, [compressed,
                         {read_concurrency, true},
                         {write_concurrency, true}]},
                  {dets, [{auto_save, 1000}]}],
    ok = ekka_mnesia:create_table(?TAB, [
                {type, set},
                {Copies, [node()]},
                {record_name, retained},
                {attributes, record_info(fields, retained)},
                {storage_properties, StoreProps}]),
    ok = ekka_mnesia:copy_table(?TAB),
    case mnesia:table_info(?TAB, storage_type) of
        Copies -> ok;
        _Other ->
            {atomic, ok} = mnesia:change_table_copy_type(?TAB, node(), Copies)
    end,
    StatsFun = emqx_stats:statsfun('retained.count', 'retained.max'),
    {ok, StatsTimer} = timer:send_interval(timer:seconds(1), stats),
    State = #state{stats_fun = StatsFun, stats_timer = StatsTimer},
    {ok, start_expire_timer(proplists:get_value(expiry_interval, Env, 0), State)}.

start_expire_timer(0, State) ->
    State;
start_expire_timer(undefined, State) ->
    State;
start_expire_timer(Ms, State) ->
    {ok, Timer} = timer:send_interval(Ms, expire),
    State#state{expiry_timer = Timer}.

handle_call(Req, _From, State) ->
    ?LOG(error, "[Retainer] Unexpected call: ~p", [Req]),
    {reply, ignored, State}.

handle_cast({dispatch, Pid, Topic}, State) ->
    Msgs = case emqx_topic:wildcard(Topic) of
                       false -> read_messages(Topic);
                       true  -> match_messages(Topic)
                   end,
            [Pid ! {deliver, Topic, Msg} || Msg  <- sort_retained(Msgs)],
    {noreply, State};

handle_cast(Msg, State) ->
    ?LOG(error, "[Retainer] Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(stats, State = #state{stats_fun = StatsFun}) ->
    StatsFun(retained_count()),
    {noreply, State, hibernate};

handle_info(expire, State) ->
    expire_messages(),
    {noreply, State, hibernate};

handle_info(Info, State) ->
    ?LOG(error, "[Retainer] Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State = #state{stats_timer = TRef1, expiry_timer = TRef2}) ->
    timer:cancel(TRef1), timer:cancel(TRef2).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

sort_retained([]) -> [];
sort_retained([Msg]) -> [Msg];
sort_retained(Msgs)  ->
    lists:sort(fun(#message{timestamp = Ts1}, #message{timestamp = Ts2}) ->
                   Ts1 =< Ts2
               end, Msgs).

store_retained(Msg = #message{topic = Topic, payload = Payload}, Env) ->
    case {is_table_full(Env), is_too_big(size(Payload), Env)} of
        {false, false} ->
            ok = emqx_metrics:set('messages.retained', retained_count()),
            mnesia:dirty_write(?TAB, #retained{topic = Topic,
                                               msg = Msg,
                                               expiry_time = get_expiry_time(Msg, Env)});
        {true, false} ->
            case mnesia:dirty_read(?TAB, Topic) of
                [_] ->
                    mnesia:dirty_write(?TAB, #retained{topic = Topic,
                                                       msg = Msg,
                                                       expiry_time = get_expiry_time(Msg, Env)});
                [] ->
                    ?LOG(error, "[Retainer] Cannot retain message(topic=~s) for table is full!", [Topic])
            end;
        {true, _} ->
            ?LOG(error, "[Retainer] Cannot retain message(topic=~s) for table is full!", [Topic]);
        {_, true} ->
            ?LOG(error, "[Retainer] Cannot retain message(topic=~s, payload_size=~p) "
                              "for payload is too big!", [Topic, iolist_size(Payload)])
    end.

is_table_full(Env) ->
    Limit = proplists:get_value(max_retained_messages, Env, 0),
    Limit > 0 andalso (retained_count() > Limit).

is_too_big(Size, Env) ->
    Limit = proplists:get_value(max_payload_size, Env, 0),
    Limit > 0 andalso (Size > Limit).

get_expiry_time(#message{headers = #{properties := #{'Message-Expiry-Interval' := 0}}}, _Env) ->
    0;
get_expiry_time(#message{headers = #{properties := #{'Message-Expiry-Interval' := Interval}}, timestamp = Ts}, _Env) ->
    Ts + Interval * 1000;
get_expiry_time(#message{timestamp = Ts}, Env) ->
    case proplists:get_value(expiry_interval, Env, 0) of
        0 -> 0;
        Interval -> Ts + Interval
    end.

-spec(read_messages(binary()) -> [emqx_types:message()]).
read_messages(Topic) ->
    case mnesia:dirty_read(?TAB, Topic) of
        [#retained{msg = Msg, expiry_time = 0}] ->
            [Msg];
        [#retained{topic = Topic, msg = Msg, expiry_time = ExpiryTime}] ->
            case erlang:system_time(millisecond) >= ExpiryTime of
                true ->
                    mnesia:transaction(fun() -> mnesia:delete({?TAB, Topic}) end),
                    [];
                false ->
                    [Msg]
            end;
        [] -> []
    end.

-spec(match_messages(binary()) -> [emqx_types:message()]).
match_messages(Filter) ->
    %% TODO: optimize later...
    Fun = fun
            (#retained{topic = Name, msg = Msg, expiry_time = ExpiryTime}, {Unexpired, Expired}) ->
                case emqx_topic:match(Name, Filter) of
                    true ->
                        case ExpiryTime =/= 0 andalso erlang:system_time(millisecond) >= ExpiryTime of
                            true -> {Unexpired, [Msg | Expired]};
                            false ->
                                {[Msg | Unexpired], Expired}
                        end;
                    false -> {Unexpired, Expired}
                end
            end,
    {Unexpired, Expired} = mnesia:async_dirty(fun mnesia:foldl/3, [Fun, {[], []}, ?TAB]),
    mnesia:transaction(
        fun() ->
            lists:foreach(fun(Msg) -> mnesia:delete({?TAB, Msg#message.topic}) end, Expired)
        end),
    Unexpired.

-spec(match_delete_messages(binary()) -> integer()).
match_delete_messages(Filter) ->
    %% TODO: optimize later...
    Fun = fun(#retained{topic = Name}, Topics) ->
              case emqx_topic:match(Name, Filter) of
                true -> mnesia:delete({?TAB, Name}), [Name | Topics];
                false -> Topics
              end
          end,
    Topics = mnesia:async_dirty(fun mnesia:foldl/3, [Fun, [], ?TAB]),
    mnesia:transaction(
        fun() ->
            lists:foreach(fun(Topic) -> mnesia:delete({?TAB, Topic}) end, Topics)
        end),
    length(Topics).

-spec(expire_messages() -> any()).
expire_messages() ->
    NowMs = erlang:system_time(millisecond),
    mnesia:transaction(
        fun() ->
            Match = ets:fun2ms(
                        fun(#retained{topic = Topic, expiry_time = ExpiryTime})
                            when ExpiryTime =/= 0 andalso NowMs > ExpiryTime -> Topic
                        end),
            Topics = mnesia:select(?TAB, Match, write),
            lists:foreach(fun(Topic) -> mnesia:delete({?TAB, Topic})
                           end, Topics)
        end).

-spec(retained_count() -> non_neg_integer()).
retained_count() -> mnesia:table_info(?TAB, size).

