emqx 核心应用自带模块数: 89
（其余 emqx_* 模块属于 12 个插件应用，切换后仍在）

===== dgiot 侧：需要我们提供的核心面 =====
  组合 14 个模块 / impl=23 stub=10 missing=3 调用=65
  （同时引用插件自带模块 13 个：切换后无需我们实现）
    [GAP] emqx                     impl=  4 stub=  2 missing=  0 calls=14
    [GAP] emqx_metrics             impl=  3 stub=  0 missing=  2 calls=11
    [GAP] emqx_guid                impl=  2 stub=  0 missing=  1 calls=5
    [OK ] emqx_logger              impl=  2 stub=  0 missing=  0 calls=5
    [GAP] emqx_broker_helper       impl=  0 stub=  4 missing=  0 calls=5
    [OK ] emqx_types               impl=  3 stub=  0 missing=  0 calls=4
    [OK ] emqx_broker              impl=  3 stub=  0 missing=  0 calls=4
    [GAP] emqx_shared_sub          impl=  0 stub=  2 missing=  0 calls=4
    [OK ] emqx_topic               impl=  2 stub=  0 missing=  0 calls=3
    [OK ] emqx_router              impl=  2 stub=  0 missing=  0 calls=3
    [GAP] emqx_vm                  impl=  0 stub=  1 missing=  0 calls=2
    [OK ] emqx_message             impl=  1 stub=  0 missing=  0 calls=2
    [OK ] emqx_hooks               impl=  1 stub=  0 missing=  0 calls=2
    [GAP] emqx_alarm_handler       impl=  0 stub=  1 missing=  0 calls=1

===== plugins 侧：需要我们提供的核心面 =====
  组合 35 个模块 / impl=62 stub=1 missing=96 调用=579
  （同时引用插件自带模块 63 个：切换后无需我们实现）
    [OK ] emqx_ctl                 impl=  9 stub=  0 missing=  0 calls=227
    [GAP] emqx_channel             impl=  0 stub=  0 missing= 16 calls=41
    [OK ] emqx_topic               impl= 11 stub=  0 missing=  0 calls=36
    [OK ] emqx_message             impl=  9 stub=  0 missing=  0 calls=30
    [GAP] emqx_json                impl=  2 stub=  0 missing=  2 calls=28
    [GAP] emqx                     impl=  4 stub=  0 missing=  1 calls=23
    [OK ] emqx_guid                impl=  2 stub=  0 missing=  0 calls=21
    [OK ] emqx_types               impl=  5 stub=  0 missing=  0 calls=19
    [OK ] emqx_logger              impl= 11 stub=  0 missing=  0 calls=17
    [GAP] emqx_metrics             impl=  3 stub=  0 missing=  2 calls=13
    [GAP] emqx_cm                  impl=  0 stub=  0 missing=  8 calls=12
    [GAP] emqx_listeners           impl=  0 stub=  0 missing=  7 calls=11
    [GAP] emqx_misc                impl=  0 stub=  0 missing=  9 calls=11
    [GAP] emqx_plugins             impl=  0 stub=  0 missing=  5 calls=9
    [OK ] emqx_broker              impl=  4 stub=  0 missing=  0 calls=8
    [GAP] emqx_trace_handler       impl=  0 stub=  0 missing=  5 calls=8
    [GAP] emqx_sys                 impl=  0 stub=  0 missing=  4 calls=7
    [GAP] emqx_hooks               impl=  2 stub=  0 missing=  2 calls=7
    [GAP] emqx_alarm               impl=  0 stub=  0 missing=  4 calls=7
    [GAP] emqx_http_lib            impl=  0 stub=  0 missing=  2 calls=6
    [GAP] emqx_stats               impl=  0 stub=  0 missing=  2 calls=5
    [GAP] emqx_session             impl=  0 stub=  0 missing=  4 calls=5
    [GAP] emqx_vm                  impl=  0 stub=  1 missing=  2 calls=4
    [GAP] emqx_calendar            impl=  0 stub=  0 missing=  3 calls=4
    [GAP] emqx_passwd              impl=  0 stub=  0 missing=  2 calls=3
    [GAP] emqx_banned              impl=  0 stub=  0 missing=  2 calls=3
    [GAP] emqx_pd                  impl=  0 stub=  0 missing=  3 calls=3
    [GAP] emqx_tls_lib             impl=  0 stub=  0 missing=  2 calls=2
    [GAP] emqx_inflight            impl=  0 stub=  0 missing=  2 calls=2
    [GAP] emqx_packet              impl=  0 stub=  0 missing=  2 calls=2
    [GAP] emqx_acl_cache           impl=  0 stub=  0 missing=  1 calls=1
    [GAP] emqx_router              impl=  0 stub=  0 missing=  1 calls=1
    [GAP] emqx_app                 impl=  0 stub=  0 missing=  1 calls=1
    [GAP] emqx_base62              impl=  0 stub=  0 missing=  1 calls=1
    [GAP] emqx_pool                impl=  0 stub=  0 missing=  1 calls=1

===== 真实缺口（核心模块里我们尚未实现的组合）=====
  - emqx:get_env/1
  - emqx_acl_cache:drain_cache/0
  - emqx_alarm:activate/2
  - emqx_alarm:deactivate/1
  - emqx_alarm:delete_all_deactivated_alarms/0
  - emqx_alarm:get_alarms/1
  - emqx_app:get_release/0
  - emqx_banned:create/1
  - emqx_banned:delete/1
  - emqx_base62:encode/1
  - emqx_calendar:format/4
  - emqx_calendar:offset_second/1
  - emqx_calendar:parse/3
  - emqx_channel:channel/0
  - emqx_channel:clear_keepalive/1
  - emqx_channel:do_deliver/2
  - emqx_channel:ensure_keepalive/2
  - emqx_channel:get_session/1
  - emqx_channel:handle_call/2
  - emqx_channel:handle_deliver/2
  - emqx_channel:handle_in/2
  - emqx_channel:handle_info/2
  - emqx_channel:handle_timeout/3
  - emqx_channel:info/1
  - emqx_channel:info/2
  - emqx_channel:init/2
  - emqx_channel:set_session/2
  - emqx_channel:stats/1
  - emqx_channel:terminate/2
  - emqx_cm:connection_closed/1
  - emqx_cm:get_chan_info/2
  - emqx_cm:get_chan_stats/2
  - emqx_cm:insert_channel_info/3
  - emqx_cm:kick_session/1
  - emqx_cm:lookup_channels/1
  - emqx_cm:set_chan_info/2
  - emqx_cm:set_chan_stats/2
  - emqx_guid:new/0
  - emqx_hooks:put/2
  - emqx_hooks:put/3
  - emqx_http_lib:uri_decode/1
  - emqx_http_lib:uri_parse/1
  - emqx_inflight:size/1
  - emqx_inflight:to_list/1
  - emqx_json:safe_decode/2
  - emqx_json:safe_encode/1
  - emqx_listeners:find_by_id/1
  - emqx_listeners:find_by_listen_on/1
  - emqx_listeners:find_id_by_listen_on/1
  - emqx_listeners:format_listen_on/1
  - emqx_listeners:identifier/1
  - emqx_listeners:restart_listener/1
  - emqx_listeners:stop_listener/1
  - emqx_metrics:inc_msg/1
  - emqx_metrics:inc_recv/1
  - emqx_metrics:set/2
  - emqx_misc:bin2hexstr_a_f_lower/1
  - emqx_misc:bin2hexstr_a_f_upper/1
  - emqx_misc:cancel_timer/1
  - emqx_misc:hexstr2bin/1
  - emqx_misc:ipv6_probe/0
  - emqx_misc:is_sane_id/1
  - emqx_misc:merge_opts/2
  - emqx_misc:proc_stats/0
  - emqx_misc:start_timer/2
  - emqx_packet:format/1
  - emqx_packet:to_message/2
  - emqx_passwd:hash/2
  - emqx_passwd:hash/3
  - emqx_pd:get_counter/1
  - emqx_pd:get_counters/1
  - emqx_pd:inc_counter/2
  - emqx_plugins:find_plugin/1
  - emqx_plugins:list/0
  - emqx_plugins:load/1
  - emqx_plugins:reload/1
  - emqx_plugins:unload/1
  - emqx_pool:async_submit/2
  - emqx_router:lookup_routes/1
  - emqx_session:dequeue/2
  - emqx_session:enqueue/3
  - emqx_session:info/2
  - emqx_session:replay/2
  - emqx_stats:getstats/0
  - emqx_stats:statsfun/2
  - emqx_sys:cluster_name/0
  - emqx_sys:info/0
  - emqx_sys:sysdescr/0
  - emqx_sys:version/0
  - emqx_tls_lib:integral_ciphers/2
  - emqx_tls_lib:integral_versions/1
  - emqx_trace_handler:install/3
  - emqx_trace_handler:install/4
  - emqx_trace_handler:running/0
  - emqx_trace_handler:uninstall/1
  - emqx_trace_handler:uninstall/2
  - emqx_vm:loads/0
  - emqx_vm:mem_info/0
  共 98 个组合

## emqx_channel  （41 次调用 / 16 个组合）
   info/2                       calls=  17 files=  1 [missing]
   set_session/2                calls=   5 files=  1 [missing]
   get_session/1                calls=   5 files=  1 [missing]
   do_deliver/2                 calls=   2 files=  1 [missing]
   terminate/2                  calls=   1 files=  1 [missing]
   stats/1                      calls=   1 files=  1 [missing]
   init/2                       calls=   1 files=  1 [missing]
   info/1                       calls=   1 files=  1 [missing]
   handle_timeout/3             calls=   1 files=  1 [missing]
   handle_info/2                calls=   1 files=  1 [missing]
   handle_in/2                  calls=   1 files=  1 [missing]
   handle_deliver/2             calls=   1 files=  1 [missing]
   handle_call/2                calls=   1 files=  1 [missing]
   ensure_keepalive/2           calls=   1 files=  1 [missing]
   clear_keepalive/1            calls=   1 files=  1 [missing]
   channel/0                    calls=   1 files=  1 [missing]

## emqx_cm  （12 次调用 / 8 个组合）
   set_chan_info/2              calls=   2 files=  1 [missing]
   lookup_channels/1            calls=   2 files=  1 [missing]
   kick_session/1               calls=   2 files=  2 [missing]
   get_chan_info/2              calls=   2 files=  2 [missing]
   set_chan_stats/2             calls=   1 files=  1 [missing]
   insert_channel_info/3        calls=   1 files=  1 [missing]
   get_chan_stats/2             calls=   1 files=  1 [missing]
   connection_closed/1          calls=   1 files=  1 [missing]

## emqx_listeners  （11 次调用 / 7 个组合）
   format_listen_on/1           calls=   3 files=  1 [missing]
   restart_listener/1           calls=   2 files=  2 [missing]
   find_id_by_listen_on/1       calls=   2 files=  2 [missing]
   stop_listener/1              calls=   1 files=  1 [missing]
   identifier/1                 calls=   1 files=  1 [missing]
   find_by_listen_on/1          calls=   1 files=  1 [missing]
   find_by_id/1                 calls=   1 files=  1 [missing]

## emqx_misc  （11 次调用 / 9 个组合）
   is_sane_id/1                 calls=   2 files=  1 [missing]
   cancel_timer/1               calls=   2 files=  1 [missing]
   start_timer/2                calls=   1 files=  1 [missing]
   proc_stats/0                 calls=   1 files=  1 [missing]
   merge_opts/2                 calls=   1 files=  1 [missing]
   ipv6_probe/0                 calls=   1 files=  1 [missing]
   hexstr2bin/1                 calls=   1 files=  1 [missing]
   bin2hexstr_a_f_upper/1       calls=   1 files=  1 [missing]
   bin2hexstr_a_f_lower/1       calls=   1 files=  1 [missing]

## emqx_plugins  （9 次调用 / 5 个组合）
   list/0                       calls=   3 files=  3 [missing]
   unload/1                     calls=   2 files=  2 [missing]
   load/1                       calls=   2 files=  2 [missing]
   reload/1                     calls=   1 files=  1 [missing]
   find_plugin/1                calls=   1 files=  1 [missing]

## emqx  （8 次调用 / 3 个组合）
   get_env/1                    calls=   6 files=  4 [missing]
   shutdown/1                   calls=   1 files=  1 [stub]
   reboot/0                     calls=   1 files=  1 [stub]

## emqx_trace_handler  （8 次调用 / 5 个组合）
   running/0                    calls=   3 files=  2 [missing]
   uninstall/2                  calls=   2 files=  2 [missing]
   uninstall/1                  calls=   1 files=  1 [missing]
   install/4                    calls=   1 files=  1 [missing]
   install/3                    calls=   1 files=  1 [missing]

## emqx_sys  （7 次调用 / 4 个组合）
   info/0                       calls=   3 files=  2 [missing]
   version/0                    calls=   2 files=  2 [missing]
   sysdescr/0                   calls=   1 files=  1 [missing]
   cluster_name/0               calls=   1 files=  1 [missing]

## emqx_alarm  （7 次调用 / 4 个组合）
   deactivate/1                 calls=   4 files=  2 [missing]
   get_alarms/1                 calls=   1 files=  1 [missing]
   delete_all_deactivated_alarms/0 calls=   1 files=  1 [missing]
   activate/2                   calls=   1 files=  1 [missing]

## emqx_metrics  （6 次调用 / 3 个组合）
   inc_msg/1                    calls=   3 files=  3 [missing]
   set/2                        calls=   2 files=  1 [missing]
   inc_recv/1                   calls=   1 files=  1 [missing]

## emqx_vm  （6 次调用 / 3 个组合）
   get_otp_version/0            calls=   3 files=  3 [stub]
   loads/0                      calls=   2 files=  2 [missing]
   mem_info/0                   calls=   1 files=  1 [missing]

## emqx_http_lib  （6 次调用 / 2 个组合）
   uri_decode/1                 calls=   5 files=  5 [missing]
   uri_parse/1                  calls=   1 files=  1 [missing]

## emqx_broker_helper  （5 次调用 / 4 个组合）
   lookup_subpid/1              calls=   2 files=  1 [stub]
   register_sub/2               calls=   1 files=  1 [stub]
   reclaim_seq/1                calls=   1 files=  1 [stub]
   get_sub_shard/2              calls=   1 files=  1 [stub]

## emqx_stats  （5 次调用 / 2 个组合）
   getstats/0                   calls=   4 files=  3 [missing]
   statsfun/2                   calls=   1 files=  1 [missing]

## emqx_session  （5 次调用 / 4 个组合）
   info/2                       calls=   2 files=  1 [missing]
   replay/2                     calls=   1 files=  1 [missing]
   enqueue/3                    calls=   1 files=  1 [missing]
   dequeue/2                    calls=   1 files=  1 [missing]

## emqx_shared_sub  （4 次调用 / 2 个组合）
   unsubscribe/3                calls=   2 files=  1 [stub]
   subscribe/3                  calls=   2 files=  1 [stub]

## emqx_calendar  （4 次调用 / 3 个组合）
   offset_second/1              calls=   2 files=  1 [missing]
   parse/3                      calls=   1 files=  1 [missing]
   format/4                     calls=   1 files=  1 [missing]

## emqx_passwd  （3 次调用 / 2 个组合）
   hash/3                       calls=   2 files=  2 [missing]
   hash/2                       calls=   1 files=  1 [missing]

## emqx_banned  （3 次调用 / 2 个组合）
   create/1                     calls=   2 files=  2 [missing]
   delete/1                     calls=   1 files=  1 [missing]

## emqx_pd  （3 次调用 / 3 个组合）
   inc_counter/2                calls=   1 files=  1 [missing]
   get_counters/1               calls=   1 files=  1 [missing]
   get_counter/1                calls=   1 files=  1 [missing]

## emqx_hooks  （2 次调用 / 2 个组合）
   put/3                        calls=   1 files=  1 [missing]
   put/2                        calls=   1 files=  1 [missing]

## emqx_json  （2 次调用 / 2 个组合）
   safe_encode/1                calls=   1 files=  1 [missing]
   safe_decode/2                calls=   1 files=  1 [missing]

## emqx_tls_lib  （2 次调用 / 2 个组合）
   integral_versions/1          calls=   1 files=  1 [missing]
   integral_ciphers/2           calls=   1 files=  1 [missing]

## emqx_inflight  （2 次调用 / 2 个组合）
   to_list/1                    calls=   1 files=  1 [missing]
   size/1                       calls=   1 files=  1 [missing]

## emqx_packet  （2 次调用 / 2 个组合）
   to_message/2                 calls=   1 files=  1 [missing]
   format/1                     calls=   1 files=  1 [missing]

## emqx_alarm_handler  （1 次调用 / 1 个组合）
   unload/0                     calls=   1 files=  1 [stub]

## emqx_guid  （1 次调用 / 1 个组合）
   new/0                        calls=   1 files=  1 [missing]

## emqx_acl_cache  （1 次调用 / 1 个组合）
   drain_cache/0                calls=   1 files=  1 [missing]

## emqx_router  （1 次调用 / 1 个组合）
   lookup_routes/1              calls=   1 files=  1 [missing]

## emqx_app  （1 次调用 / 1 个组合）
   get_release/0                calls=   1 files=  1 [missing]

## emqx_base62  （1 次调用 / 1 个组合）
   encode/1                     calls=   1 files=  1 [missing]

## emqx_pool  （1 次调用 / 1 个组合）
   async_submit/2               calls=   1 files=  1 [missing]
