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

-module(dgiot_bridge_loader).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).


%% API
-export([start/3, load_channel/2, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {success}).


start(Name, Filter, Fun) ->
    case whereis(Name) of
        undefined ->
            ChildSpec = {
                Name,
                {?MODULE, start_link, [Name, Filter, Fun]},
                permanent, 5000, worker, [?MODULE]
            },
            supervisor:start_child(dgiot_bridge_sup, ChildSpec);
        Pid ->
            case gen_server:call(Pid, {load, Filter, Fun}, 5000) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    {error, Reason}
            end
    end.


start_link(Name, Where, Success) ->
    gen_server:start_link({local, Name}, ?MODULE, [Where, Success], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Filter, Success]) ->
    start_load_channel(self(), 100, 100, Filter),
    {ok, #state{success = Success}}.

handle_call({load, Filter, Success}, _From, State) ->
    start_load_channel(self(), 100, 100, Filter),
    {reply, ok, State#state{success = Success}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({load, Time, Module, Channels}, State) when Time > 5 ->
    ?LOG(error,"Load Err ~p, Mod:~p", [Module, Channels]),
    {stop, normal, State};
handle_info({load, Time, Module, Channels}, #state{success = Success} = State) ->
    case load_channel(Channels, fun(Channel) -> Success(Module, Channel) end) of
        {ok, []} ->
            {noreply, State};
        {ok, ErrChannels} ->
            erlang:send_after(5000 * (Time + 1), self(), {load, Time + 1, Module, ErrChannels}),
            {noreply, State};
        {error, Reason} ->
            ?LOG(error,"~p load Err, ~p", [Channels, Reason]),
            erlang:send_after(5000 * (Time + 1), self(), {load, Time + 1, Module, Channels}),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_channel(Channels, Fun) ->
    Request =
        [#{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"/classes/Product">>,
            <<"body">> => #{
                <<"keys">>=>[<<"objectId">>, <<"productSecret">>,<<"decoder">>],
                <<"include">> => [<<"Dict">>],
                <<"where">> => #{
                    <<"$relatedTo">> =>#{
                        <<"key">> => <<"product">>,
                        <<"object">> => #{
                            <<"__type">> => <<"Pointer">>,
                            <<"className">> => <<"Channel">>,
                            <<"objectId">> => ChannelId
                        }
                    }
                }
            }
        } || #{<<"objectId">> := ChannelId} <- Channels],
    case dgiot_parse:batch(Request) of
        {ok, Results} ->
            format_channel(Channels, Results, Fun, []);
        {error, Reason} ->
            {error, Reason}
    end.

format_channel([], [], _, Err) -> {ok, Err};
format_channel([Info | Channels], [#{<<"success">> := #{<<"results">> := Products}} | Results], Fun, Err) ->
    Keys = [<<"decoder">>,<<"ACL">>, <<"dynamicReg">>, <<"nodeType">>, <<"productSecret">>, <<"config">>,<<"thing">>, <<"topics">>],
    NewProducts = [{ProductId, maps:with(Keys, Product)} || #{<<"objectId">> := ProductId} = Product <- Products],
    Channel = Info#{<<"product">> => NewProducts},
    Fun(Channel),
    format_channel(Channels, Results, Fun, Err);
format_channel([Channel | Channels], [#{<<"error">> := Reason} | Results], Fun, Err) ->
    ?LOG(error,"~p load error, ~p", [Channel, Reason]),
    format_channel(Channels, Results, Fun, [Channel | Err]).

start_load_channel(_, _, _, []) -> ok;
start_load_channel(Pid, PageSize, MaxTotal, [Filter | Filters]) ->
    start_load_channel(Pid, PageSize, MaxTotal, Filter),
    start_load_channel(Pid, PageSize, MaxTotal, Filters);


start_load_channel(Pid, PageSize, MaxTotal, #{<<"mod">> := Module, <<"where">> := Where}) ->
    Keys = [<<"type">>, <<"cType">>, <<"config">>],
    Query = #{
        <<"keys">> => Keys,
        <<"where">> => Where
    },
    dgiot_parse_loader:start(<<"Channel">>, Query, PageSize, MaxTotal,
        fun(Channels) ->
            Pid ! {load, 0, Module, Channels}
        end);
start_load_channel(Pid, PageSize, MaxTotal, #{<<"where">> := Where}) ->
    start_load_channel(Pid, PageSize, MaxTotal, #{
        <<"mod">> => <<"dgiot_bridge_frame">>,
        <<"where">> => Where
    }).




%%load_protocol() ->
%%    {file, Here} = code:is_loaded(?MODULE),
%%    Dir = filename:dirname(filename:dirname(Here)),
%%    Path = lists:concat([Dir, "/priv/protocol/"]),
%%    case file:list_dir(Path) of
%%        {error, Reason} ->
%%            {error, Reason};
%%        {ok, FileNams} ->
%%            load_protocol(FileNams, Path)
%%    end.
%%
%%load_protocol(FileName, Dir) ->
%%    Path = lists:concat([Dir, "/", FileName]),
%%    case file:read_file(Path) of
%%        {ok, Data} ->
%%            io:
%%    end.
