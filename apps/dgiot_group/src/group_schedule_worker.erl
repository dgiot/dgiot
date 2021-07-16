-module(group_schedule_worker).

-include("dgiot_group.hrl").
-include_lib("dgiot/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, receive_from_mqtt/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(#{<<"vcaddr">> := VcAddr, <<"di">> := Di, <<"tid">> := Tid} = State) ->
    case dgiot_data:lookup(?dgiot_GROUP_TASK, {VcAddr, Tid, Di}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    gen_server:start_link(?MODULE, [State#{<<"vcaddr">> => VcAddr}], []).

init([#{<<"pns">> := <<"all">>, <<"vcaddr">> := VcAddr} = State]) ->
    start_task(State#{<<"pns">> => dgiot_data:get(?dgiot_GROUP, VcAddr)});

init([#{<<"pns">> := _Pns} = State]) ->
    start_task(State);

init([#{<<"vcaddr">> := VcAddr} = State]) ->
    start_task(State#{<<"pns">> => dgiot_data:get(?dgiot_GROUP, VcAddr)}).

start_task(#{
    <<"vcaddr">> := VcAddr,
    <<"di">> := Di,
    <<"tid">> := Tid,
    <<"pns">> := Pns,
    <<"fdate">> := Fdate}) ->
    Chs = dgiot_data:get(?dgiot_GROUP_TASK, {<<"Chs">>,Tid}),
    ChsLen = length(Chs),
    [Cid,Retry] = lists:nth(1,Chs),
    Pn = dgiot_group_utils:get_next_pn_bits(0,Pns),
    QueLen = dgiot_utils:binary_bits_sum(Pns),
    dgiot_data:insert(?dgiot_GROUP_TASK, {VcAddr, Tid, Di}, self()),
    Ref = erlang:send_after(Retry + dgiot_data:get_consumer({<<"taskdelay">>,Tid,Di},1) * 5, self(), timeout),
    dgiot_metrics:counter(?GROUP_METRICS, ?dgiot_GROUP_METRICS , [Di,Tid, 1, Cid,"total"],QueLen),
    HexVcAddr = dgiot_utils:binary_to_hex(VcAddr),
    Topic = <<"task/",HexVcAddr/binary,"/",Di/binary>>,
    dgiot_utils:subscribe(Topic),
    ?LOG(info,"start Topic ~p Deylay ~p ~p:~p QueLen ~p Pns ~p ",[Topic, Retry,dgiot_utils:binary_to_hex(VcAddr),Pn,QueLen,dgiot_utils:binary_to_hex(Pns)]),
    {ok,#schedule_worker{ tid = Tid, chslen = ChsLen - 1, cid = Cid, rid = 1, ref = Ref,
        vcaddr = VcAddr, pns = Pns, quelen = QueLen,  pn = Pn, di = Di, fdate = Fdate, status = ?UN_SEND}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};

handle_info(stop, State) ->
    {stop, normal, State};

% 抄表结束
handle_info(timeout, #schedule_worker{tid = _Tid, vcaddr = _VcAddr, chslen = 0, quelen = 0} = State) ->
    ?LOG(info,"stop ~p:  ",[dgiot_utils:binary_to_hex(_VcAddr)]),
    erlang:send_after(100, self(), stop),
    {noreply, State};

%%新一轮补抄
handle_info(timeout, #schedule_worker{tid = Tid, chslen = ChsLen, rid = Rid,
            pns = FailPns, quelen = 0,  di =  Di}  = State) ->
    QueLen = dgiot_group_utils:binary_bits_sum(FailPns),
    Chs  = dgiot_data:get(?dgiot_GROUP_TASK, {<<"Chs">>,Tid}),
    {NewChsLen,NewRetry,NewCid} = case QueLen of
        0 -> {0,10,1};
        _ ->
            [TmpCid,Retry] = lists:nth(Rid+1, Chs),
            dgiot_metrics:counter(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di,Tid, Rid+1, TmpCid,"total"], QueLen),
            {ChsLen - Rid, Retry, TmpCid}
    end,
    Pn = dgiot_group_utils:get_next_pn_bits(0,FailPns),
    erlang:send_after(NewRetry, self(), timeout),
    {noreply, State#schedule_worker{chslen = NewChsLen, cid = NewCid, rid = Rid + 1, quelen = QueLen, pn = Pn, status = ?UN_SEND}};

%%-define(SEND_SUCC, 0).   %%成功
%%-define(UN_SEND, 1).     %%未发送
%%-define(PASS_SEND, 2).   %%已发送
%%-define(SEND_TIMEOUT,N). %%超时次数 N - 2
% 超时触发抄表
handle_info(timeout, #schedule_worker{vcaddr = VcAddr, pn = Pn, cid = Cid, fdate = Fdate, di = Di,
    tid = Tid, rid = Rid, quelen = QueLen, status = ?UN_SEND } = State) ->
    send_to_channel(#{<<"ch">> => Cid, <<"tid">> => Tid, <<"rid">> => Rid,  <<"vcaddr">> => VcAddr,
        <<"pn">> => Pn, <<"di">> => Di, <<"fdate">> => Fdate}),
    Ref = erlang:send_after(?TIMEOUT_INTERVAL, self(), timeout),
    {noreply, State#schedule_worker{pn = Pn, quelen = QueLen, status = ?PASS_SEND, ref = Ref}};

handle_info(timeout, #schedule_worker{vcaddr = VcAddr, pn = Pn, cid = Cid, fdate = Fdate, di = Di, pns = Pns,
    tid = Tid, rid = Rid, quelen = QueLen, status = ?SEND_SUCC } = State) ->
    NewPn = dgiot_group_utils:get_next_pn_bits(Pn, Pns),
    send_to_channel(#{<<"ch">> => Cid, <<"tid">> => Tid, <<"rid">> => Rid, <<"vcaddr">> => VcAddr,
        <<"pn">> => NewPn, <<"di">> => Di, <<"fdate">> => Fdate}),
    Ref = erlang:send_after(?TIMEOUT_INTERVAL, self(), timeout),
    {noreply, State#schedule_worker{pn = NewPn, quelen = QueLen, status = ?PASS_SEND, ref = Ref}};

handle_info(timeout, #schedule_worker{vcaddr = VcAddr, pn = Pn, cid = Cid, fdate = Fdate, di = Di,
    tid = Tid, rid = Rid , quelen = QueLen, pns = Pns, status = ?PASS_SEND} = State) ->
    NewPn = dgiot_group_utils:get_next_pn_bits(Pn,Pns),
    send_to_channel(#{<<"ch">> => Cid, <<"tid">> => Tid, <<"rid">> => Rid,  <<"vcaddr">> => VcAddr,
        <<"pn">> => NewPn, <<"di">> => Di, <<"fdate">> => Fdate}),
    Ref = erlang:send_after(?TIMEOUT_INTERVAL, self(), timeout),
    {noreply, State#schedule_worker{pn = NewPn, quelen = QueLen - 1, ref = Ref}};

handle_info({meter_ack, Payload}, #schedule_worker{ref = Ref, pn = Pn, vcaddr  = VcAddr, di = Di, quelen = QueLen,
        pns = Pns, tid = Tid, cid = Cid, rid = Rid, fdate = Fdate, ack = Ack} =  State) ->
    NewPns =  dgiot_group_utils:set_binary_bits_n(Pns, Pn, 0),
    dgiot_metrics:counter(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di,Tid, Rid, Cid,"to_vcon"],1),
    erlang:cancel_timer(Ref),
    erlang:send_after(Ack * 1000, self(), timeout),
    send_to_resource(Payload#{
        <<"vctime">> => dgiot_datetime:now_secs(), <<"tid">> => Tid,  <<"rid">> => Rid, <<"cid">> => Cid,
        <<"tags">> => VcAddr, <<"di">> => Di, <<"fdate">> => Fdate, <<"status">> => ?SEND_SUCC, <<"pn">> => Pn}),
    ?LOG(info,"~p:~p QueLen ~p Di ~p Pns ~p  ",[dgiot_utils:binary_to_hex(VcAddr),Pn,QueLen,Di,dgiot_utils:binary_to_hex(NewPns)]),
    {noreply, State#schedule_worker{quelen = QueLen - 1, status = ?SEND_SUCC, pn = Pn, pns = NewPns}};

handle_info({deliver, _Topic, Msg}, #schedule_worker{ref = Ref, pn = Pn, vcaddr  = VcAddr, di = Di, quelen = QueLen,
    pns = Pns, tid = Tid, cid = Cid, rid = Rid, fdate = Fdate, ack = Ack} =  State) ->
    Payload = binary_to_term(dgiot_mqtt:get_payload(Msg)),
    NewPns =  dgiot_group_utils:set_binary_bits_n(Pns, Pn, 0),
    dgiot_metrics:counter(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di,Tid, Rid, Cid,"to_vcon"],1),
    erlang:cancel_timer(Ref),
    erlang:send_after(Ack * 1000, self(), timeout),
    save_to_td(Payload#{
        <<"vctime">> => dgiot_datetime:now_secs(),
        <<"tid">> => Tid,
        <<"rid">> => Rid,
        <<"cid">> => Cid,
        <<"tags">> => VcAddr,
        <<"di">> => Di,
        <<"fdate">> => Fdate,
        <<"pn">> => Pn,
        <<"status">> => ?SEND_SUCC}),
    ?LOG(info,"~p:~p QueLen ~p Di ~p Pns ~p  ",[dgiot_utils:binary_to_hex(VcAddr),Pn,QueLen,Di,dgiot_utils:binary_to_hex(NewPns)]),
    {noreply, State#schedule_worker{quelen = QueLen - 1, status = ?SEND_SUCC, pn = Pn, pns = NewPns}};

handle_info({td_gc,Pid}, State) ->
%%    ?LOG(info,"td_gc ~p ",[Pid]),
    case is_process_alive(Pid) of
        true ->
            erlang:garbage_collect(Pid),
            erlang:exit(Pid, brutal_kill);
        _ -> ok
    end,
{noreply, State};

handle_info(_Info, State) ->
    ?LOG(info,"handle other Info:~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, #schedule_worker{ vcaddr  = VcAddr, di = Di, tid = Tid} =  _State) ->
    dgiot_data:delete(?dgiot_GROUP_TASK, {VcAddr, Tid, Di}),
%%    ?LOG(info,"stop _Reason ~p:  ",[_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_to_channel(#{<<"ch">> := Ch,<<"tid">> := Tid, <<"rid">> := Rid, <<"vcaddr">> := VcAddr,
    <<"pn">> := Pn, <<"di">> := Di,  <<"fdate">> := Fdate} = Frame) ->
    case dgiot_data:get(?dgiot_GROUP_METER,{VcAddr, Pn}) of
           #meter{ addr = BinAddr, channel = Chs }->
                case maps:get(Ch, Chs, no) of
                    no ->
                        HexVcAddr = dgiot_utils:binary_to_hex(VcAddr),
                        Topic = <<"MSRELAY/",HexVcAddr/binary>>,
                        Payload = jsx:encode(Frame#{
                            <<"vcaddr">> => HexVcAddr,
                            <<"pn">> => Pn,
                            <<"di">> => Di,
                            <<"fdate">> => Fdate,
                            <<"frame_type">> => <<"task">>}),
                        dgiot_utils:publish(?MODULE, Topic, Payload),
                        dgiot_metrics:counter(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, 18, "to_meter"],1);
                    Name ->
                        Payload = [#{<<"di">> => Di,
                            <<"vctime">> => Fdate,
                            <<"frame_type">> => <<"task">>,
                            <<"addr">> => BinAddr,
                            <<"ch">> => Name},#{}],
                        dgiot_metrics:counter(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Ch, "to_meter"],1),
                        dgiot_data:insert(?dgiot_GROUP_ROUTE, BinAddr, self()),
                        dgiot_hook:run_hook({channel,Ch},Payload)
                end;
            _ -> pass
    end.

save_to_td(#{<<"vctime">> := _Ts, <<"tid">> := Tid, <<"rid">> := Rid, <<"cid">> := Cid,<<"tags">> := VcAddr,
    <<"fdate">> := Fdate, <<"status">> := Status, <<"pn">> := Pn,<<"di">> := Di, <<"value">> := Values} = Payload ) ->
    case catch dgiot_tdengine_bridge_dml_ddl:insert_table({Di,dgiot_utils:binary_to_hex(VcAddr),dgiot_datetime:now_ms(),Fdate,Tid,Rid,Status,Pn}, Values) of
        {'EXIT', Reason} ->
            ?LOG(error,"~p ~p",[Reason,Payload]);
        _ ->
            dgiot_metrics:counter(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Cid, "to_td"],1)
    end.

send_to_resource( #{<<"vctime">> := _Ts, <<"tid">> := Tid, <<"rid">> := Rid} = Payload ) ->
    save_to_td(Payload),
    Res  = dgiot_data:get(?dgiot_GROUP_TASK, {<<"Res">>,Tid}),
    lists:map(fun(_Rid) ->
        dgiot_hook:run_hook({resource, Rid},[Payload])
    end,Res).

receive_from_mqtt(#{<<"payload">> := Payload}) ->
    case Payload of
        #{<<"addr">> := DevAddr} ->
            case dgiot_data:lookup(?dgiot_GROUP_ROUTE, DevAddr) of
            {ok, Pid} when is_pid(Pid) ->
            dgiot_utils:send_msg(Pid, {meter_ack,Payload});
            _Reason ->
                pass
            end,
            dgiot_data:delete(?dgiot_GROUP_ROUTE, DevAddr);
        _ -> pass
    end.
