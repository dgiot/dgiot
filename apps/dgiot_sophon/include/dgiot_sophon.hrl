-define(sophon, <<"sophon">>).

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
    dtutype = <<>>,
    hb = 60,
    maxaddr = 0,
    que = []
}).
