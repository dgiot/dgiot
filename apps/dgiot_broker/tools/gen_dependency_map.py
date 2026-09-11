#!/usr/bin/env python3
"""Scan dgiot apps for emqx_* call sites (depth-aware, arity-aware) and
emit: (1) doc/DEPENDENCY-MAP.md, (2) doc/dependency-map.json,
(3) compat/<module>.erl facades (uncompiled until the switch cut).

Why arity matters: Erlang has no varargs, so a same-named facade must
declare the exact arities dgiot actually calls.
"""
import json
import os
import re
import sys
from collections import defaultdict

SRC_ROOT = "/opt/dgiot-4.4/apps"
DEST = "/mnt/d/ai/github/iotStudio/hub_apps/dgiot_broker"
CALL = re.compile(r"\b(emqx[a-z0-9_]*)\s*:\s*([a-z0-9_]+)\s*\(")
MODULE_CUT = {
    # broker core -> native implementation cuts
    "emqx": "cut4", "emqx_broker": "cut4", "emqx_router": "cut4",
    "emqx_topic": "cut4", "emqx_shared_sub": "cut7", "emqx_broker_helper": "cut4",
    "emqx_message": "cut4", "emqx_mqtt_types": "cut2", "emqx_types": "cut2",
    "emqx_frame": "cut2", "emqx_packet": "cut2",
    # observability / utility
    "emqx_metrics": "cut4", "emqx_logger": "cut1", "emqx_guid": "cut1",
    "emqx_vm": "cut1", "emqx_mgmt": "cut7", "emqx_mgmt_util": "cut7",
    "emqx_mgmt_cli": "cut7", "emqx_topic_trace": "cut7",
    # rule engine: 收回给 dgiot_task/dgiot_bridge（不写门面）
    "emqx_rule_engine": "cut8", "emqx_rule_engine_api": "cut8",
    "emqx_rule_registry": "cut8", "emqx_rule_metrics": "cut8",
    "emqx_rule_utils": "cut8", "emqx_rule_events": "cut8",
    # plugin framework
    "emqx_plugin": "cut6", "emqx_plugin_libs": "cut6",
    # hooks / observability leftovers
    "emqx_hooks": "cut1", "emqx_tracer": "cut7", "emqx_trace": "cut7",
    "emqx_trace_handler": "cut7", "emqx_trace_api": "cut7",
    "emqx_alarm_handler": "cut7", "emqx_schema_parser": "cut7",
    "emqx_rule_id": "cut8",
}
# 函数级覆盖：同一模块内不同函数归属不同刀（钩子面 vs 数据面）
FUNCTION_CUT = {
    ("emqx", "hook"): "cut1", ("emqx", "unhook"): "cut1",
    ("emqx", "publish"): "cut4", ("emqx", "subscribe"): "cut4",
    ("emqx", "reboot"): "cut7", ("emqx", "shutdown"): "cut7",
    ("emqx_broker", "safe_publish"): "cut4",
    ("emqx_broker", "publish"): "cut4",
}


def cut_of(mod, fun):
    return FUNCTION_CUT.get((mod, fun), MODULE_CUT.get(mod, "cut9_unmapped"))


def scan():
    calls = defaultdict(int)
    files = defaultdict(set)
    for root, _dirs, names in os.walk(SRC_ROOT):
        # only dgiot's own apps (vendored emqx* apps inside apps/ are EMQX's
        # own internals, not dgiot coupling)
        app = os.path.relpath(root, SRC_ROOT).split(os.sep)[0]
        if not app.startswith("dgiot"):
            continue
        for n in names:
            if not n.endswith(".erl"):
                continue
            path = os.path.join(root, n)
            try:
                text = open(path, encoding="utf-8", errors="replace").read()
            except OSError:
                continue
            for m in CALL.finditer(text):
                mod, fun = m.group(1), m.group(2)
                if not (mod.startswith("emqx") or mod == "emqx"):
                    continue
                # depth-aware argument scan
                i = m.end()
                depth, commas, seen = 1, 0, False
                while i < len(text) and depth > 0:
                    ch = text[i]
                    if ch in "([{":
                        depth += 1
                    elif ch in ")]}":
                        depth -= 1
                        if depth == 0:
                            break
                    elif ch == "," and depth == 1:
                        commas += 1
                    elif not ch.isspace():
                        seen = True
                    i += 1
                arity = 0 if not seen else commas + 1
                calls[(mod, fun, arity)] += 1
                files[(mod, fun, arity)].add(path)
    return calls, files


def facade_source(mod, funcs, cut_of_fn):
    lines = [
        f"%% @doc 同名门面：{mod} —— 仅在「无 EMQX 模式」下编译（刀 6 切换）。",
        "%%",
        "%% 为什么不在 src/：EMQX 在位时同名模块会与 emqx app 冲突（代码路径",
        "%% 二义），故本目录不参与当前 build；切换刀把 compat/ 加入 src_dirs 并",
        "%% 从 release 剔除 emqx 应用。",
        "%%",
        "%% 未实现的能力一律显式报错，绝不静默返回 ok。",
        f"-module({mod}).",
    ]
    exports = ", ".join(f"{f}/{a}" for (f, a) in sorted(funcs))
    lines.append(f"-export([{exports}]).")
    lines.append("")
    for (f, a) in sorted(funcs):
        args = ", ".join(f"_A{i}" for i in range(a)) if a else ""
        lines.append(f"{f}({args}) ->")
        lines.append(f"    {{error, {{not_implemented, {cut_of_fn(f)}, {mod}, {f}}}}}.")
        lines.append("")
    return "\n".join(lines)


def main():
    calls, files = scan()
    by_mod = defaultdict(set)
    for (mod, fun, arity) in calls:
        by_mod[mod].add((fun, arity))

    os.makedirs(f"{DEST}/doc", exist_ok=True)
    os.makedirs(f"{DEST}/compat", exist_ok=True)

    # machine-readable map
    data = [{"module": m, "function": f, "arity": a,
             "calls": calls[(m, f, a)],
             "files": sorted(p.replace(SRC_ROOT, "apps") for p in files[(m, f, a)]),
             "cut": cut_of(m, f),
             "via": ("dgiot_task/dgiot_bridge" if cut_of(m, f) == "cut8"
                     else f"compat/{m}.erl -> dgiot_broker port")}
            for (m, f, a) in sorted(calls)]
    open(f"{DEST}/doc/dependency-map.json", "w", encoding="utf-8").write(
        json.dumps(data, ensure_ascii=False, indent=2))

    # human map
    md = ["# dgiot → EMQX 调用点映射表（自动生成，勿手改）", "",
          f"总计 **{len(data)}** 个 `模块:函数/元数` 组合，"
          f"**{sum(d['calls'] for d in data)}** 次调用，涉及 "
          f"**{len(by_mod)}** 个 emqx 模块。", "",
          "| emqx 模块 | 函数/元数 | 次数 | 承接刀 | 承接方式 |", "|---|---|---|---|---|"]
    for d in sorted(data, key=lambda x: (x["module"], x["function"], x["arity"])):
        md.append(f"| `{d['module']}` | `{d['function']}/{d['arity']}` | {d['calls']} "
                  f"| {d['cut']} | {d['via']} |")
    md += ["", "## 名字冲突处理（关键）", "",
           "同名门面**不能**与 EMQX 同时在位：Erlang 同名模块在代码路径中二义。",
           "因此 `compat/` 不参与当前 build；刀 6 切换时：",
           "1. `rebar.config.erl` 的 `src_dirs` 加入 `compat`；",
           "2. `relx_plugin_apps_per_rel/1` 剔除 15 个 emqx 应用；",
           "3. 用 `broker.backend=dgiot` 切换，异常即回滚。"]
    open(f"{DEST}/doc/DEPENDENCY-MAP.md", "w", encoding="utf-8").write("\n".join(md))

    # facades：只补缺，不覆盖手写实现（影子内核模块为手写）
    written = 0
    kept = 0
    for mod, funcs in by_mod.items():
        cuts = {cut_of(mod, f) for (f, _a) in funcs}
        if cuts == {"cut8"}:
            continue  # rule engine: 收回给 dgiot_task/dgiot_bridge，不写门面
        path = f"{DEST}/compat/{mod}.erl"
        if os.path.exists(path):
            kept += 1
            continue
        open(path, "w", encoding="utf-8").write(
            facade_source(mod, funcs, lambda f: cut_of(mod, f)))
        written += 1
    print(f"pairs={len(data)} calls={sum(d['calls'] for d in data)} "
          f"modules={len(by_mod)} facades_written={written} facades_kept={kept}")
    unmapped = [m for m in by_mod if m not in MODULE_CUT]
    if unmapped:
        print("UNMAPPED:", unmapped)


if __name__ == "__main__":
    sys.exit(main())
