#!/usr/bin/env python3
"""切换就绪度（精确版）：先把 emqx 模块按**归属**分成两类——
  CORE   = 定义在 emqx 核心应用里（_build/.../emqx/src）→ 切换后消失，必须由我们提供
  PLUGIN = 定义在 12 个插件应用里 → 切换后仍在，不算缺口
再对照 compat 的实现情况（impl/stub/missing）给出真实缺口。"""
import os
import re
import sys
from collections import defaultdict

ROOT = "/opt/dgiot-4.4/apps"
CORE_SRC = "/opt/dgiot-4.4/_build/emqx/lib/emqx/src"
COMPAT = "/opt/dgiot-4.4/apps/dgiot_broker/compat"
CALL = re.compile(r"\b(emqx[a-z0-9_]*)\s*:\s*([a-z0-9_]+)\s*\(")
EXPORT = re.compile(r"-export\(\[(.*?)\]\)\.", re.S)


def core_modules():
    return {f[:-4] for f in os.listdir(CORE_SRC) if f.endswith(".erl")}


def scan_calls(prefixes):
    calls = defaultdict(int)
    for app in sorted(os.listdir(ROOT)):
        if not any(app.startswith(p) for p in prefixes):
            continue
        for root, _d, files in os.walk(os.path.join(ROOT, app, "src")):
            for f in files:
                if not f.endswith(".erl"):
                    continue
                text = open(os.path.join(root, f), encoding="utf-8",
                            errors="replace").read()
                for m in CALL.finditer(text):
                    mod, fun = m.group(1), m.group(2)
                    i, depth, commas, seen = m.end(), 1, 0, False
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
    return calls


def compat_state():
    state = defaultdict(dict)
    for f in sorted(os.listdir(COMPAT)):
        if not f.endswith(".erl"):
            continue
        mod = f[:-4]
        text = open(os.path.join(COMPAT, f), encoding="utf-8").read()
        m = EXPORT.search(text)
        if not m:
            continue
        for item in m.group(1).split(","):
            item = item.strip()
            if "/" not in item:
                continue
            fun, arity = item.split("/")
            body = re.search(rf"^{fun}\(([^)]*)\)\s*->\s*(.*?)\.",
                             text, re.S | re.M)
            st = "stub" if body and "not_implemented" in body.group(2) else "impl"
            state[mod][f"{fun}/{arity}"] = st
    return state


def main():
    core = core_modules()
    ours = scan_calls(["dgiot"])
    theirs = scan_calls(["emqx_"])
    state = compat_state()

    print(f"emqx 核心应用自带模块数: {len(core)}")
    print(f"（其余 emqx_* 模块属于 12 个插件应用，切换后仍在）\n")

    for label, calls in (("dgiot", ours), ("plugins", theirs)):
        need = defaultdict(lambda: {"impl": 0, "stub": 0, "missing": 0, "calls": 0})
        keep = set()
        for (mod, fun, arity), n in calls.items():
            if mod not in core:
                keep.add(mod)
                continue
            key = f"{fun}/{arity}"
            st = state.get(mod, {}).get(key, "missing")
            need[mod][st] += 1
            need[mod]["calls"] += n
        tot = {"impl": 0, "stub": 0, "missing": 0, "calls": 0}
        for a in need.values():
            for k in tot:
                tot[k] += a[k]
        print(f"===== {label} 侧：需要我们提供的核心面 =====")
        print(f"  组合 {sum(1 for m in need)} 个模块 / impl={tot['impl']} "
              f"stub={tot['stub']} missing={tot['missing']} 调用={tot['calls']}")
        print(f"  （同时引用插件自带模块 {len(keep)} 个：切换后无需我们实现）")
        for mod in sorted(need, key=lambda m: -need[m]["calls"]):
            a = need[mod]
            flag = "OK " if not a["missing"] and not a["stub"] else "GAP"
            print(f"    [{flag}] {mod:24} impl={a['impl']:3} stub={a['stub']:3} "
                  f"missing={a['missing']:3} calls={a['calls']}")
        print()

    print("===== 真实缺口（核心模块里我们尚未实现的组合）=====")
    gap = sorted({(m, f"{f}/{a}")
                  for (m, f, a) in list(ours) + list(theirs)
                  if m in core and state.get(m, {}).get(f"{f}/{a}", "missing") == "missing"})
    for m, k in gap:
        print(f"  - {m}:{k}")
    print(f"  共 {len(gap)} 个组合")
    return 0


if __name__ == "__main__":
    sys.exit(main())
