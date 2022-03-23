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
-module(dgiot_httpc_worker).
-author("johnliu").
-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_bridge.hrl").
%% API
-export([
    childSpec/1,
    start/1,
    start_link/1
]).

-export([
    set_url/2,
    set_method/2,
    set_contenttype/2,
    set_header/2,
    set_body/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-record(state, {tid, pid, did, token, sleep = 12}).
%%%===================================================================
%%% API
%%%===================================================================
childSpec(ChannelId) ->
    [?CHILD(dgiot_httpc_sup, supervisor, [?DGIOT_SUP(ChannelId)])].

start(#{<<"channelid">> := ChannelId} = Args) ->
    supervisor:start_child(?DGIOT_SUP(ChannelId), [Args]).

start_link(#{
    <<"channelid">> := ChannelId,
    <<"productid">> := ProductId,
    <<"devaddr">> := DevAddr
}) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case dgiot_data:lookup({ChannelId, DeviceId, httpc}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    Server = list_to_atom(lists:concat([httpc, dgiot_utils:to_list(ChannelId), dgiot_utils:to_list(DeviceId)])),
    gen_server:start_link({local, Server}, ?MODULE,
        [#{<<"channelid">> => ChannelId, <<"productid">> => ProductId, <<"devaddr">> => DevAddr}], []).

init([#{
    <<"channelid">> := ChannelId,
    <<"productid">> := ProductId,
    <<"devaddr">> := DevAddr
}]) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    dgiot_data:insert({ChannelId, DeviceId, httpc}, self()),
    erlang:send_after(10000, self(), retry),
    erlang:send_after(10 * 1000, self(), retry),
    {ok, #state{tid = ChannelId, pid = ProductId, did = DeviceId}};

init(Args) ->
    io:format("dgiot_httpc_worker:init:~p~n", [Args]).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(retry, #state{tid = Tid, sleep = Sleep} = State) ->
    Url = get_url(Tid),
    Headers = get_header(Tid),
    ContentHeader = get_contenttype(Tid),
    Body = dgiot_json:encode(get_body(Tid)),
    case dgiot_http_client:request(get_method(Tid), {Url, Headers, ContentHeader, Body}) of
        {ok, R} ->
            case jsx:is_json(dgiot_utils:to_binary(R)) of
                true ->
                    Bin = dgiot_utils:to_binary(R),
                    ?LOG(info, "R1 ~p ", [maps:get(<<"username">>, jsx:decode(Bin, [{labels, binary}, return_maps]), <<"">>)]);
                _ ->
                    io:format("~s ~p R2 ~s ", [?FILE, ?LINE, R])
            end;
        {error, Reason} ->
            ?LOG(info, "Reason ~p ", [Reason])
    end,
    erlang:send_after(Sleep * 1000, self(), retry),
    {noreply, State#state{tid = Tid}};

handle_info(refreshtoken, #state{pid = ProductId} = State) ->
    case dgiot_hook:run_hook({httpc, refreshtoken, ProductId}, State) of
        {ok, NewState} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{tid = Tid, did = Id} = _State) ->
    dgiot_data:delete({Tid, Id, httpc}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

set_method(ChannelId, Args) ->
    Method = dgiot_utils:to_list(maps:get(<<"method">>, Args)),
    dgiot_data:insert({ChannelId, ?MODULE, method}, dgiot_utils:to_atom(string:to_lower(Method))).

get_method(ChannelId) ->
    dgiot_data:get({ChannelId, ?MODULE, method}).

set_url(ChannelId, Args) ->
    dgiot_data:insert({ChannelId, ?MODULE, url}, dgiot_utils:to_list(maps:get(<<"url">>, Args))).

get_url(ChannelId) ->
    dgiot_data:get({ChannelId, ?MODULE, url}).

set_contenttype(ChannelId, Args) ->
    dgiot_data:insert({ChannelId, ?MODULE, contenttype}, dgiot_utils:to_list(maps:get(<<"contenttype">>, Args))).

get_contenttype(ChannelId) ->
    dgiot_data:get({ChannelId, ?MODULE, contenttype}).

set_header(ChannelId, Args) ->
    Header = lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"key">> := Key, <<"value">> := Value} ->
                Acc ++ [{dgiot_utils:to_list(Key), dgiot_utils:to_list(Value)}];
            _ ->
                Acc
        end
                         end, [], maps:get(<<"header">>, Args)),
    dgiot_data:insert({ChannelId, ?MODULE, header}, Header).

get_header(ChannelId) ->
    dgiot_data:get({ChannelId, ?MODULE, header}).

set_body(ChannelId, Args) ->
    Body = lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"key">> := Key, <<"value">> := Value} ->
                Acc#{Key => Value};
            _ ->
                Acc
        end
                       end, #{}, maps:get(<<"body">>, Args)),
    io:format("Body ~p", [Body]),
    dgiot_data:insert({ChannelId, ?MODULE, body}, Body).

get_body(ChannelId) ->
    dgiot_data:get({ChannelId, ?MODULE, body}).
