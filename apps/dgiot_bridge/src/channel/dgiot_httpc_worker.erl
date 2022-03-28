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
-define(CRLF, "\r\n").
-export([
    test1/1,
    test/1,
    set_host/2,
    get_host/1,
    set_path/2,
    get_path/1,
    set_method/2,
    get_method/1,
    set_contenttype/2,
    get_contenttype/1,
    set_header/2,
    get_header/1,
    set_body/2,
    get_body/1
]).

%% gen_server callbacks
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tid, pid, did, token, freq = 12}).
%%%===================================================================
%%% API
%%%===================================================================

start_link(#{
    <<"channelid">> := ChannelId,
    <<"productid">> := ProductId,
    <<"devaddr">> := DevAddr
} = Args) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case dgiot_data:lookup({ChannelId, DeviceId, httpc}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    Server = list_to_atom(lists:concat([httpc, dgiot_utils:to_list(ChannelId), dgiot_utils:to_list(DeviceId)])),
    gen_server:start_link({local, Server}, ?MODULE, [Args], []).

init([#{
    <<"channelid">> := ChannelId,
    <<"productid">> := ProductId,
    <<"devaddr">> := DevAddr,
    <<"freq">> := Freq
}]) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    dgiot_data:insert({ChannelId, DeviceId, httpc}, self()),
    erlang:send_after(Freq * 1000, self(), toke),
    {ok, #state{tid = ChannelId, pid = ProductId, did = DeviceId, freq = Freq}};

init(Args) ->
    io:format("dgiot_httpc_worker:init:~p~n", [Args]).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(token, #state{pid = ProductId, freq = Freq} = State) ->
    erlang:send_after(Freq * 10 * 1000, self(), reshtoken),
    erlang:send_after(Freq * 1000, self(), capture),
    case dgiot_hook:run_hook({httpc, token, ProductId}, State) of
        {ok, NewState} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(capture, #state{tid = Tid, pid = ProductId, freq = Freq} = State) ->
    {Method, Request} =
        case dgiot_hook:run_hook({httpc, do_before, ProductId}, State) of
            {ok, {NewMethod, NewRequest}} ->
                {NewMethod, NewRequest};
            _ ->
                Url = get_host(Tid) ++ get_path(Tid),
                Headers = get_header(Tid),
                ContentHeader = get_contenttype(Tid),
                Body = dgiot_json:encode(get_body(Tid)),
                get_method(Tid), {Url, Headers, ContentHeader, Body}
        end,
    case dgiot_http_client:request(Method, Request) of
        {ok, Result} ->
            ?LOG(info, "Result ~p ", [Result]),
            dgiot_hook:run_hook({httpc, do_after, ProductId}, {Result, State});
        {error, Reason} ->
            ?LOG(info, "Reason ~p ", [Reason])
    end,
    erlang:send_after(Freq * 1000, self(), capture),
    {noreply, State};

handle_info(reshtoken, #state{pid = ProductId} = State) ->
    case dgiot_hook:run_hook({httpc, reshtoken, ProductId}, State) of
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

set_host(ChannelId, Args) ->
    dgiot_data:insert({ChannelId, ?MODULE, url}, dgiot_utils:to_list(maps:get(<<"host">>, Args))).

get_host(ChannelId) ->
    dgiot_data:get({ChannelId, ?MODULE, url}).

set_path(ChannelId, Args) ->
    dgiot_data:insert({ChannelId, ?MODULE, url}, dgiot_utils:to_list(maps:get(<<"path">>, Args))).

get_path(ChannelId) ->
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
    dgiot_data:insert({ChannelId, ?MODULE, body}, Body).

get_body(ChannelId) ->
    dgiot_data:get({ChannelId, ?MODULE, body}).

test1(SessionId) ->
    Host = "127.0.0.1:8090",
    Url = "http://" ++ Host ++ "/project/accesspointgroup/GetAllDevices",
%%    SessionId = get_SessionId(),
    Body = <<"----------------------------323091708878914445963306", ?CRLF,
        "Content-Disposition: form-data; name=\"page\"", ?CRLF, ?CRLF,
        "1", ?CRLF,
        "----------------------------323091708878914445963306", ?CRLF,
        "Content-Disposition: form-data; name=\"rows\"", ?CRLF, ?CRLF,
        "200", ?CRLF,
        "----------------------------323091708878914445963306--",?CRLF>>,
    Size = byte_size(Body),
    io:format("Size ~p ~n", [Size]),
    Headers = [
        {"cookie", "ASP.NET_SessionId=" ++ SessionId},
        {"Accept", "application/json, text/javascript, */*; q=0.01"},
        {"User-Agent", "Content-Length"},
        {"Postman-Token", "78ff7879-35e9-4e0a-be65-6651a2783b89"},
        {"Accept-Encoding", "gzip, deflate"},
        {"Connection", "keep-alive"},
        {"Content-Length", Size}
    ],
    ContentType = "multipart/form-data; boundary=--------------------------323091708878914445963306",
    Request = {Url, Headers, ContentType, Body},
    io:format(" ~p ~n", [Request]),
    case dgiot_http_client:request(post, Request) of
        {ok, {{"HTTP/1.1", 200, "OK"}, _, Data}} ->
            BinData = dgiot_utils:to_binary(Data),
            case jsx:is_json(BinData) of
                true ->
                    ?LOG(info, "Data ~p ", [jsx:decode(BinData, [{labels, binary}, return_maps])]);
                _ ->
                    io:format("33  ~p ~n", [BinData])
            end;
        Other ->
            io:format("~p ~n", [Other])
    end.


test(SessionId) ->
    Host = "127.0.0.1:8090",
    Url = "http://" ++ Host ++ "/project/accesspointgroup/GetAllDevices",
%%    SessionId = get_SessionId(),
    Body = "page=1&rows=10",
    Size = length(Body),
    io:format("Size ~p ~n", [Size]),
    Headers = [
        {"Content-Length", Size},
        {"Accept", "application/json, text/javascript, */*; q=0.01"},
        {"X-Requested-With", "XMLHttpRequest"},
        {"User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.82 Safari/537.36"},
        {"Accept-Encoding", "gzip, deflate"},
        {"Accept-Language", "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"},
        {"cookie", "ASP.NET_SessionId=" ++ SessionId},
        {"Connection", "keep-alive"}
    ],
    ContentType = "application/x-www-form-urlencoded; charset=UTF-8",
    Request = {Url, Headers, ContentType, Body},
    io:format(" ~p ~n", [Request]),
    case dgiot_http_client:request(post, Request) of
        {ok, {{"HTTP/1.1", 200, "OK"}, _, Data}} ->
            BinData = dgiot_utils:to_binary(Data),
            case jsx:is_json(BinData) of
                true ->
                    ?LOG(info, "Data ~p ", [jsx:decode(BinData, [{labels, binary}, return_maps])]);
                _ ->
                    io:format("33  ~p ~n", [BinData])
            end;
        Other ->
            io:format("~p ~n", [Other])
    end.