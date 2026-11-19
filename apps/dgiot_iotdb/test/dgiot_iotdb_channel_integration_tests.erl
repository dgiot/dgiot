-module(dgiot_iotdb_channel_integration_tests).
-include_lib("eunit/include/eunit.hrl").

channel_roundtrip_test_() ->
    case {os:getenv("IOTDB_REST_URL"), os:getenv("IOTDB_REST_USERNAME"), os:getenv("IOTDB_REST_PASSWORD")} of
        {Url, User, Password} when is_list(Url), is_list(User), is_list(Password) ->
            {timeout, 60, fun() -> roundtrip(list_to_binary(Url), list_to_binary(User), list_to_binary(Password)) end};
        _ -> []
    end.

roundtrip(Url, User, Password) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(mnesia),
    dgiot_data:init(),
    dgiot_data:init(dgiot_product),
    dgiot_data:init(dgiot_bridge),
    Channel = binary:encode_hex(crypto:strong_rand_bytes(8)),
    Product = <<"p", Channel/binary>>,
    Address = <<"meter-18">>,
    Config = #{<<"url">> => Url, <<"username">> => User, <<"password">> => Password,
        <<"root">> => <<"root.rn18_", Channel/binary>>, <<"db">> => <<"ProductId">>, <<"timeout">> => 10000},
    {ok, State, []} = dgiot_iotdb_channel:init(<<"IOTDB">>, Channel, Config),
    {ok, State} = dgiot_iotdb_channel:handle_message(init_products, State),
    Database = dgiot_iotdb:get_database(Channel, Product),
    Device = dgiot_iotdb:get_device_path(Channel, Product, dgiot_parse_id:get_deviceid(Product, Address)),
    Timestamp = erlang:system_time(millisecond),
    {ok, _} = dgiot_iotdb:create_database(Channel, Database),
    try
        {ok, State} = dgiot_iotdb_channel:handle_message({data, Product, Address,
            #{<<"createdat">> => Timestamp, <<"temperature">> => 18.5}, #{}}, State),
        {ok, #{<<"results">> := [Row]}} = dgiot_iotdb:query_object(Channel, Product, Device,
            #{<<"keys">> => <<"temperature">>, <<"limit">> => 1}),
        ?assertEqual(Timestamp, maps:get(<<"createdat">>, Row)),
        ?assertEqual(18.5, maps:get(<<"temperature">>, Row))
    after
        {ok, _} = dgiot_iotdb:batch_sql(Channel, <<"DROP DATABASE ", Database/binary>>)
    end.
