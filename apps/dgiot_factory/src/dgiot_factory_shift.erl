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

-module(dgiot_factory_shift).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").
-export([get_one_shift/1, save_one_shift/1, updata_id/0, get_shift/3, post_shift/1, get_all_shift/4, get_workshop/2]).
-export([post_one_shift/1, get_shift_time/1]).
-define(DAY, 86400).
-define(SHIFT, [<<"白班"/utf8>>, <<"晚班"/utf8>>]).
-define(WORKERCALENDAR, <<"WorkerCanlendar">>).
-define(INTERVAL, 604800).
-define(ONEDAY, 86400).

get_all_shift(Department, undefined, undefined, SessionToken) ->
    StartDate = dgiot_datetime:localtime_to_unixtime(calendar:local_time()),
    EndDate = StartDate + ?INTERVAL,
    WorkshopList = get_workshop(SessionToken, Department),
    Res = get_department_shift(Department, StartDate, EndDate, WorkshopList, []),
    {ok, Res};
get_all_shift(Department, Data, undefined, SessionToken) ->
    WorkshopList = get_workshop(SessionToken, Department),
    Res = lists:foldl(
        fun(X, Acc) ->
            case get_shift(Department, Data, X) of
                {ok, Res} ->
                    Acc ++ Res;
                _ ->
                    Acc
            end
        end, [], WorkshopList),
    {ok,  Res};


get_all_shift(Department, undefined, Workshop, _) ->
    StartDate = dgiot_datetime:localtime_to_unixtime(calendar:local_time()),
    EndDate = StartDate + ?INTERVAL,
    Res = get_department_shift(Department, StartDate, EndDate, [Workshop], []),
    {ok, Res};

get_all_shift(Department, Data, Workshop, _) ->
    {ok, Res} = dgiot_factory_shift:get_shift(Department, Data, Workshop),
    {ok, Res}.


%%get_all_shift(Department, Data, Workshop, SessionToken) ->
%%    case Data of
%%        undefined ->
%%            StartDate = dgiot_datetime:localtime_to_unixtime(calendar:local_time()),
%%            EndDate = StartDate + ?INTERVAL,
%%            WorkshopList = case Workshop of
%%                               undefined ->
%%                                   get_workshop(SessionToken, Department);
%%                               _ ->
%%                                   [Workshop]
%%                           end,
%%            Res = get_department_shift(StartDate, EndDate, WorkshopList, []),
%%            {ok, Res};
%%        _ ->
%%            WorkshopList = get_workshop(SessionToken, Department),
%%            Res = lists:foldl(
%%                fun(X, Acc) ->
%%                    case get_shift(Data, X) of
%%                        {ok, Res} ->
%%                            Acc ++ Res;
%%                        _ ->
%%                            Acc
%%                    end
%%                end, [], WorkshopList),
%%            {ok, #{Data => Res}}
%%
%%    end.

get_workshop(SessionToken, Department) ->
    case dgiot_parse_utils:get_classtree(<<"_Role">>, <<"parent">>, #{}, SessionToken) of
        {200, Res} ->
            Tree = maps:get(<<"results">>, Res, <<"">>),
            get_workshop(Tree, [], Department);
        _ ->
            {error, not_find_workshop}
    end.

get_workshop(Tree, Acc, Department) ->
    case length((Tree)) of
        0 ->
            Acc;
        _ ->
            lists:foldl(
                fun(X, ACC) ->
                    case maps:get(<<"depname">>, X, <<"">>) of
                        Department ->
                            Child = maps:get(<<"children">>, X, []),
                            lists:foldl(
                                fun(Workshop, Workshops) ->
                                    case maps:get(<<"name">>, Workshop, <<"">>) of
                                        <<"">> ->
                                            Workshops;
                                        Other ->
                                            Workshops ++ [Other]
                                    end
                                end, [], Child);

                        _ ->
                            Child = maps:get(<<"children">>, X, []),
                            get_workshop(Child, ACC, Department)
                    end
                end, Acc, Tree)
    end.

get_department_shift(Department, StartDate, EndDate, WorkshopList, Acc) ->
    case (EndDate - StartDate) < 0 of
        true ->
            Acc;
        _ ->
            DateStr = dgiot_datetime:format(StartDate, <<"YY-MM-DD">>),
            Shifts = lists:foldl(
                fun(X, ACC) ->
                    case get_shift(Department, DateStr, X) of
                        {ok, Res} ->
                            ACC ++ Res;
                        _ ->
                            ACC
                    end
                end, [], WorkshopList),
            get_department_shift(Department, StartDate + ?ONEDAY, EndDate, WorkshopList, Acc ++ Shifts)
    end.



get_shift(Department, Date, Workshop) ->
    case get_shift_time(Department) of
        {ok, Shifts, _} ->
            Res = lists:foldl(
                fun(X, Acc) ->
                    case get_one_shift(#{<<"date">> => Date, <<"device">> => Workshop, <<"shift">> => X}) of
                        {ok, #{<<"worker">> := Worker}} ->
                            Acc ++ [#{<<"date">> => Date, <<"device">> => Workshop, <<"shift">> => X, <<"worker">> => Worker}];
                        _ ->
                            Acc
                    end
                end, [], Shifts),
            {ok, Res};
        {error, Msg} ->
            {error, Msg}
    end.


post_shift(#{<<"device">> := Dev, <<"date">> := Date, <<"shift">> := Shift, <<"worker">> := Worker}) ->
    post_one_shift(#{<<"device">> => Dev, <<"date">> => Date, <<"shift">> => Shift, <<"worker">> => Worker});

post_shift(Shifts) when is_list(Shifts) ->
    lists:foldl(
        fun(X, _Acc) ->
            case X of
                #{<<"device">> := Dev, <<"date">> := Date, <<"shift">> := Shift, <<"worker">> := Worker} ->
                    post_one_shift(#{<<"device">> => Dev, <<"date">> => Date, <<"shift">> => Shift, <<"worker">> => Worker});
                _ ->
                    pass
            end
        end, [], Shifts);
post_shift(_) ->
    error.


post_one_shift(Args) ->
    case dgiot_parse_id:get_objectid(<<"shift">>, Args) of
        #{<<"objectId">> := ObjectId} ->
            io:format("~s ~p ObjectId =~p ~n",[?FILE,?LINE,ObjectId]),
            case dgiot_parse:get_object(?WORKERCALENDAR, ObjectId) of
                {ok, _} ->
                    dgiot_parse:update_object(?WORKERCALENDAR, ObjectId, Args);
                {error, _} ->
                    NewArgs = Args#{<<"objectId">> => ObjectId},
                    dgiot_parse:create_object(?WORKERCALENDAR, NewArgs)
            end;
        _ ->
            pass
    end.



get_one_shift(Args) ->
    case dgiot_parse_id:get_objectid(<<"shift">>, Args) of
        #{<<"objectId">> := ObjectId} ->
            case dgiot_parse:get_object(?WORKERCALENDAR, ObjectId) of
                {ok, Res} ->
                    {ok, Res};
                {error, Res} ->
                    {error, Res}
            end;
        _ ->
            pass
    end.


save_one_shift(Shift) ->
    case dgiot_parse_id:get_objectid(<<"shift">>, Shift) of
        #{<<"objectId">> := ObjectId} ->
            case dgiot_parse:get_object(?WORKERCALENDAR, ObjectId) of
                {ok, _Res} ->
                    io:format("~s ~p here~n", [?FILE, ?LINE]),
                    dgiot_parse:update_object(?WORKERCALENDAR, ObjectId, Shift);
                A ->
                    io:format("~s ~p A = ~p ~n", [?FILE, ?LINE, A]),
                    NewShift = Shift#{<<"objectId">> => ObjectId},
                    dgiot_parse:create_object(?WORKERCALENDAR, NewShift)
            end;
        _ ->

            pass
    end.


%%dgiot_datetime:format(dgiot_datetime:nowstamp(), <<"YY-MM-DD">>)
%%dgiot_datetime:format("HH:NN:SS")


updata_id() ->
    {ok, #{<<"results">> := Res}} = dgiot_parse:query_object(?WORKERCALENDAR, #{}),
    lists:foldl(
        fun(X, _Acc) ->
            Old = maps:get(<<"objectId">>, X),
            #{<<"objectId">> := ObjectId} = dgiot_parse_id:get_objectid(<<"shift">>, X),
            io:format("Old = ~p ,Id =~p ~n", [Old, ObjectId]),
            dgiot_parse:update_object(?WORKERCALENDAR, Old, #{<<"objectId">> => ObjectId})
        end, [], Res).


get_shift_time(Department) ->
    case dgiot_factory_calendar:get_calendar(Department) of
        {ok, _, Calendar} ->
            Default = maps:get(<<"default">>, Calendar),
            Shifts = maps:get(<<"work_shift">>, Default),
            ShiftList = maps:keys(Shifts),
            {ok, ShiftList, Shifts};
        _ ->
            {error, not_find_shift}
    end.


%%post_shift(Shifts) ->
%%    io:format("~s ~p here~n", [?FILE, ?LINE]),
%%    maps:fold(
%%        fun(Date, DateCon, _ACc) ->
%%            maps:fold(
%%                fun(Dev, DevCon, _Acc) ->
%%                    maps:fold(
%%                        fun(Shift, Worker, _ACC) ->
%%%%                            DateStr = dgiot_datetime:format(Date, <<"YY-MM-DD">>),
%%                            Args = #{<<"device">> => Dev, <<"date">> => Date, <<"shift">> => Shift, <<"worker">> => Worker},
%%                            post_one_shift(Args)
%%                        end, #{}, DevCon)
%%                end, #{}, DateCon)
%%        end, #{}, Shifts).
