%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd.
%%--------------------------------------------------------------------

-module(dgiot_message).
-author("dgiot").
-include("dgiot_mqtt.hrl").
-export([make/4]).

make(Client, Qos, Topic, Payload) ->
    #message{
        id = dgiot_guid:gen(),
        qos = Qos,
        from = Client,
        flags = #{},
        headers = #{},
        topic = Topic,
        payload = Payload,
        timestamp = erlang:system_time(millisecond)
    }.
