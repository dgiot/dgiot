#!/usr/bin/env python3
"""按调用量列出**缺失组合**的精确清单（模块:函数/元数 + 次数 + 调用方）。"""
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


def scan():
    out = defaultdict(int)
    where = defaultdict(set)
    for app in sorted(os.listdir(ROOT)):
        if not (app.startswith("dgiot") or app.startswith("emqx_")):
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
                    out[(mod, fun, arity)] += 1
                    where[(mod, fun, arity)].add(f)
    return out, where


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
            state[mod][f"{fun}/{arity}"] = (
                "stub" if body and "not_implemented" in body.group(2) else "impl")
    return state


def main():
    core = core_modules()
    calls, where = scan()
    state = compat_state()
    by_mod = defaultdict(list)
    for (mod, fun, arity), n in calls.items():
        if mod not in core:
            continue
        st = state.get(mod, {}).get(f"{fun}/{arity}")
        if st in ("impl",):
            continue
        by_mod[mod].append((n, f"{fun}/{arity}", st or "missing", len(where[(mod, fun, arity)])))
    for mod in sorted(by_mod, key=lambda m: -sum(x[0] for x in by_mod[m])):
        items = sorted(by_mod[mod], reverse=True)
        print(f"\n## {mod}  （{sum(x[0] for x in items)} 次调用 / {len(items)} 个组合）")
        for n, key, st, nf in items:
            print(f"   {key:28} calls={n:4} files={nf:3} [{st}]")
    return 0


if __name__ == "__main__":
    sys.exit(main())
