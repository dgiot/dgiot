-module(dgiot_device_permission).
-export([get_acl/1, get_acl_by_role/1, get_appname/1,
         get_readonly_acl/1, get_roleids/1]).
get_acl(_) -> [].
get_acl_by_role(_) -> [].
get_appname(_) -> <<>>.
get_readonly_acl(_) -> [].
get_roleids(_) -> [].
