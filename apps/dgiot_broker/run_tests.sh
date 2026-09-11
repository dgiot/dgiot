#!/bin/bash
# dgiot_broker 测试运行器：主体单测 + 影子内核单测
#
# 说明：compat/ 是「无 EMQX 模式」的同名承接实现（emqx/emqx_broker/...），
# 与在位 EMQX 同名冲突，故**不参与常规 build**；本脚本把它们编译到临时目录，
# 在**不加载 EMQX 的干净节点**里跑影子内核测试（这正是切换后的运行形态）。
#
# 用法：bash apps/dgiot_broker/run_tests.sh [EBIN_DIR]
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
EBIN="${1:-$ROOT/_build/emqx/lib/dgiot_broker/ebin}"
SHADOW=/tmp/dgiot_broker_shadow
rm -rf "$SHADOW" && mkdir -p "$SHADOW"

echo "== 编译影子内核（同名承接）=="
for f in "$HERE"/compat/*.erl; do
  erlc -o "$SHADOW" -pa "$EBIN" "$f"
done

echo "== 编译测试 =="
for t in "$HERE"/test/*.erl; do
  erlc -o "$SHADOW" -pa "$EBIN" -pa "$SHADOW" "$t"
done

DEPS=""
for lib in dgiot jiffy jsx; do
  d="$ROOT/_build/emqx/lib/$lib/ebin"
  [ -d "$d" ] && DEPS="$DEPS -pa $d"
done

echo "== 运行（干净节点：不加载 EMQX）=="
erl -noshell -pa "$EBIN" $DEPS -pa "$SHADOW" -eval '
    Which = code:which(emqx),
    io:format("emqx resolves to: ~s~n", [Which]),
    R1 = eunit:test(dgiot_broker_frame_tests, []),
    R2 = eunit:test(dgiot_broker_core_tests, []),
    R3 = eunit:test(dgiot_broker_shadow_tests, []),
    case {R1, R2, R3} of
        {ok, ok, ok} -> io:format("ALL SUITES OK~n"), halt(0);
        _ -> io:format("FAILURES~n"), halt(1)
    end.' 2>&1 | tail -12
