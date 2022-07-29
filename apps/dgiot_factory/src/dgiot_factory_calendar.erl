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

-module(dgiot_factory_calendar).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-define(DEFAULT,#{<<"default">> => #{<<"type">> => <<"day">>,
    <<"work_shift">> => #{<<"morning_shift">> => #{<<"begin">> => <<"8:00">>, <<"end">> => <<"12:00">>},
        <<"afternonn_shift">> => #{<<"begin">> => <<"13:00">>, <<"end">> => <<"19:00">>},
        <<"evening_shift">> => #{<<"begin">> => <<"19:00">>, <<"end">> => <<"6:30">>
        }}},<<"other">>=>#{}}).
-define(TYPE_LIST,[<<"week">>,<<"month">>]).
-define(FACTORY_CALENDAR,<<"factory_calendar">>).
-define(SCOPE,#{<<"week">>=>[1,7],<<"month">>=[1,31]}).


-export([get_calendar/0,post_calendar/1,check_default/1,check_type/1,check_work_shift/1,check_laying_off/2]).
-export([get_new_other/2]).





%%dgiot_datetime:format(dgiot_datetime:nowstamp(), <<"YY-MM-DD">>)
%%dgiot_datetime:format("HH:NN:SS")

post_calendar(Newcalendar)->
    io:format("~s ~p Newcalendar =~p ~n",[?FILE,?LINE,Newcalendar]),
        case dgiot_factory_calendar:get_calendar() of
            {ok,ObjectId,OldCalendar} ->
                Merged_Calendar = maps:fold(
                     fun(K,V,Acc)->
                        case  K of
                            <<"default">> ->
                                Acc#{<<"default">> => V};
                            <<"other">> ->
                                Other = get_new_other(OldCalendar,V),
                                Acc#{<<"other">> =>Other};
                            _ ->
                                Acc
                        end
                    end,OldCalendar,Newcalendar),
                dgiot_parse:update_object(<<"Dict">>,ObjectId,#{<<"data">> => Merged_Calendar});
            _ ->
                pass
        end
.

get_new_other(OldCalendar, NewOther) ->
    case maps:find(<<"other">>, OldCalendar) of
        {ok, Old} ->
             maps:fold(
                fun(K, V, Acc) ->
                    io:format("~s ~p Acc = ~p ~n",[?FILE,?LINE,Acc]),
                    case maps:size(V) of
                        0 ->
                            io:format("~s ~p herep ~n",[?FILE,?LINE]),
                            maps:remove(K, Acc);
                        _ ->
                            io:format("~s ~p herep ~n",[?FILE,?LINE]),
                            Acc#{K => V}
                    end
                end, Old, NewOther);
        error ->
            NewOther
    end.


get_calendar()->
    case dgiot_parse:query_object(<<"Dict">>,#{<<"where">> => #{<<"type">> => ?FACTORY_CALENDAR}}) of
        {ok,#{<<"results">> := Results}} ->
            case length(Results) of
                1 ->

                    #{<<"objectId">> := ObjectId,<<"data">> :=Calendar} = lists:nth(1,Results),
                    {ok,ObjectId,Calendar};
                0 ->
                    case dgiot_parse:create_object(<<"Dict">>, #{<<"type">> => ?FACTORY_CALENDAR, <<"data">> =>?DEFAULT }) of
                        {ok,#{<<"objectId">> := ObjectId}} ->
                            {ok,ObjectId,?DEFAULT};
                        {error,Error} ->
                            {create_calendar_failed,Error}
                    end;
                Num ->
                    {get_calendar_more_than_one,Num}
            end;
        _ ->
            pass
    end.

check_default(Default)->
    case check_type(Default) of
        {ok}->
            case check_work_shift(Default) of
                {ok} ->
                    {ok};
                {error}->
                    pass
            end;
        {error,Res} ->
            {error,Res}
    end.

check_type(Default)->
    case maps:find(<<"type">>,Default) of
        {ok,<<"day">>}->
            {ok};
        {ok,Type} ->
            case lists:member(Type,?TYPE_LIST) of
                true ->
                    case check_laying_off(Type,Default) of
                        {ok}->
                            {ok};
                        {error,Res} ->
                            {error,Res}
                    end;
                false ->
                    {error,wrong_type}
            end;
        error->
            {error,not_find_type}
    end.

check_laying_off(_Type,Default)->
    case maps:find(<<"laying_off">>,Default) of
        {ok,_Laying_off} ->
            {ok};
        error ->
            {error,not_find_laying_of}
    end.

check_work_shift(_Default)->
    {ok}.
