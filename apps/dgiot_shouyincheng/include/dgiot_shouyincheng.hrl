-define(DGIOT_SHOUYINCHENG_TCP_DTU, dgiot_shouyincheng_tcp_dtu).
-record(state, {
    id,
    devaddr = <<>>,
    heartcount = 0,
    regtype = <<>>,
    head = "xxxxxx0eee",
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    scale = 10,
    temperature = 0,
    env = <<>>,
    dtutype = <<>>
}).
