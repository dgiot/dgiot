-module(dgiot_device_subdevice).
-export([get_sub_device/1, get_sub_device/2, get_subdevices/2,
         save_subdevice/2, save_subdevice/3, get_subdevice/2]).
get_sub_device(_) -> [].
get_sub_device(_, _) -> [].
get_subdevices(_, _) -> [].
save_subdevice(X, Y) -> dgiot_device_cache:save_subdevice(X, Y).
save_subdevice(_, _, _) -> ok.
get_subdevice(X, Y) -> dgiot_device_cache:get_subdevice(X, Y).
