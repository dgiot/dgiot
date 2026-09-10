# dgiot → EMQX 调用点映射表（自动生成，勿手改）

总计 **68** 个 `模块:函数/元数` 组合，**125** 次调用，涉及 **27** 个 emqx 模块。

| emqx 模块 | 函数/元数 | 次数 | 承接刀 | 承接方式 |
|---|---|---|---|---|
| `emqx` | `hook/3` | 5 | cut1 | compat/emqx.erl -> dgiot_broker port |
| `emqx` | `publish/1` | 1 | cut4 | compat/emqx.erl -> dgiot_broker port |
| `emqx` | `reboot/0` | 1 | cut7 | compat/emqx.erl -> dgiot_broker port |
| `emqx` | `shutdown/1` | 1 | cut7 | compat/emqx.erl -> dgiot_broker port |
| `emqx` | `subscribe/3` | 1 | cut4 | compat/emqx.erl -> dgiot_broker port |
| `emqx` | `unhook/2` | 5 | cut1 | compat/emqx.erl -> dgiot_broker port |
| `emqx_alarm_handler` | `unload/0` | 1 | cut7 | compat/emqx_alarm_handler.erl -> dgiot_broker port |
| `emqx_broker` | `safe_publish/1` | 1 | cut4 | compat/emqx_broker.erl -> dgiot_broker port |
| `emqx_broker` | `subscribe/3` | 1 | cut4 | compat/emqx_broker.erl -> dgiot_broker port |
| `emqx_broker` | `unsubscribe/1` | 2 | cut4 | compat/emqx_broker.erl -> dgiot_broker port |
| `emqx_broker_helper` | `get_sub_shard/2` | 1 | cut4 | compat/emqx_broker_helper.erl -> dgiot_broker port |
| `emqx_broker_helper` | `lookup_subpid/1` | 2 | cut4 | compat/emqx_broker_helper.erl -> dgiot_broker port |
| `emqx_broker_helper` | `reclaim_seq/1` | 1 | cut4 | compat/emqx_broker_helper.erl -> dgiot_broker port |
| `emqx_broker_helper` | `register_sub/2` | 1 | cut4 | compat/emqx_broker_helper.erl -> dgiot_broker port |
| `emqx_guid` | `gen/0` | 3 | cut1 | compat/emqx_guid.erl -> dgiot_broker port |
| `emqx_guid` | `new/0` | 1 | cut1 | compat/emqx_guid.erl -> dgiot_broker port |
| `emqx_guid` | `to_hexstr/1` | 1 | cut1 | compat/emqx_guid.erl -> dgiot_broker port |
| `emqx_hooks` | `add/2` | 2 | cut1 | compat/emqx_hooks.erl -> dgiot_broker port |
| `emqx_logger` | `debug/2` | 1 | cut1 | compat/emqx_logger.erl -> dgiot_broker port |
| `emqx_logger` | `get_primary_log_level/0` | 3 | cut1 | compat/emqx_logger.erl -> dgiot_broker port |
| `emqx_logger` | `set_log_level/1` | 2 | cut1 | compat/emqx_logger.erl -> dgiot_broker port |
| `emqx_message` | `make/4` | 2 | cut4 | compat/emqx_message.erl -> dgiot_broker port |
| `emqx_metrics` | `ensure/1` | 3 | cut4 | compat/emqx_metrics.erl -> dgiot_broker port |
| `emqx_metrics` | `inc/2` | 1 | cut4 | compat/emqx_metrics.erl -> dgiot_broker port |
| `emqx_metrics` | `inc_msg/1` | 1 | cut4 | compat/emqx_metrics.erl -> dgiot_broker port |
| `emqx_metrics` | `set/2` | 2 | cut4 | compat/emqx_metrics.erl -> dgiot_broker port |
| `emqx_metrics` | `val/1` | 4 | cut4 | compat/emqx_metrics.erl -> dgiot_broker port |
| `emqx_mgmt` | `do_unsubscribe/2` | 1 | cut7 | compat/emqx_mgmt.erl -> dgiot_broker port |
| `emqx_mgmt` | `list_nodes/0` | 1 | cut7 | compat/emqx_mgmt.erl -> dgiot_broker port |
| `emqx_mgmt` | `subscribe/2` | 1 | cut7 | compat/emqx_mgmt.erl -> dgiot_broker port |
| `emqx_mgmt_cli` | `cluster/1` | 1 | cut7 | compat/emqx_mgmt_cli.erl -> dgiot_broker port |
| `emqx_mgmt_util` | `kmg/1` | 2 | cut7 | compat/emqx_mgmt_util.erl -> dgiot_broker port |
| `emqx_mqtt_types` | `topic_filters/0` | 2 | cut2 | compat/emqx_mqtt_types.erl -> dgiot_broker port |
| `emqx_router` | `has_routes/1` | 2 | cut4 | compat/emqx_router.erl -> dgiot_broker port |
| `emqx_rule_engine` | `rule/0` | 9 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `create_resource/2` | 4 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `create_rule/2` | 4 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `delete_resource/2` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `delete_rule/2` | 2 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `list_actions/2` | 2 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `list_resources/2` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `list_rules/2` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `show_action/2` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `show_resource/2` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `show_rule/2` | 4 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_engine_api` | `update_rule/2` | 3 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_events` | `event_name/1` | 2 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_events` | `unload/1` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_id` | `gen/0` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_metrics` | `create_rule_metrics/1` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_metrics` | `inc_actions_success/1` | 2 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_registry` | `remove_resource/1` | 2 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_registry` | `remove_rule/1` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_utils` | `can_topic_match_oneof/2` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_utils` | `log_action/4` | 1 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_utils` | `preproc_tmpl/1` | 2 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_rule_utils` | `proc_tmpl/2` | 2 | cut8 | dgiot_task/dgiot_bridge |
| `emqx_schema_parser` | `decode/3` | 1 | cut7 | compat/emqx_schema_parser.erl -> dgiot_broker port |
| `emqx_schema_parser` | `encode/3` | 1 | cut7 | compat/emqx_schema_parser.erl -> dgiot_broker port |
| `emqx_shared_sub` | `subscribe/3` | 2 | cut7 | compat/emqx_shared_sub.erl -> dgiot_broker port |
| `emqx_shared_sub` | `unsubscribe/3` | 2 | cut7 | compat/emqx_shared_sub.erl -> dgiot_broker port |
| `emqx_topic` | `match/2` | 1 | cut4 | compat/emqx_topic.erl -> dgiot_broker port |
| `emqx_topic` | `topic/0` | 2 | cut4 | compat/emqx_topic.erl -> dgiot_broker port |
| `emqx_tracer` | `lookup_traces/0` | 1 | cut7 | compat/emqx_tracer.erl -> dgiot_broker port |
| `emqx_types` | `subid/0` | 1 | cut2 | compat/emqx_types.erl -> dgiot_broker port |
| `emqx_types` | `subopts/0` | 1 | cut2 | compat/emqx_types.erl -> dgiot_broker port |
| `emqx_types` | `topic/0` | 2 | cut2 | compat/emqx_types.erl -> dgiot_broker port |
| `emqx_vm` | `get_otp_version/0` | 2 | cut1 | compat/emqx_vm.erl -> dgiot_broker port |

## 名字冲突处理（关键）

同名门面**不能**与 EMQX 同时在位：Erlang 同名模块在代码路径中二义。
因此 `compat/` 不参与当前 build；刀 6 切换时：
1. `rebar.config.erl` 的 `src_dirs` 加入 `compat`；
2. `relx_plugin_apps_per_rel/1` 剔除 15 个 emqx 应用；
3. 用 `broker.backend=dgiot` 切换，异常即回滚。