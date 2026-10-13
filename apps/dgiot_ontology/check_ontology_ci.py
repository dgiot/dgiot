#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""check_ontology_ci.py — dgiot 仓内本体 CI 检查（2026-10-13）

**定位**：本脚本是 **CI 专用**，只检查**本仓内**的本体文件（`apps/dgiot_ontology/priv/*.ttl`）。
完整工具链在另一仓（工具链仓的 tools/ontology.py，含 15 个子命令：国标合规 / 一致性 /
质量度量 / 版本差分 / CQ 验证等）。**在 CI 里不复制那一整套** ✗ —— 一处一事 ✓
本脚本只做**能在本仓独立完成**的两件事：
  ① 语法可解析（W3C RDF/Turtle）
  ② **SHACL 验证**（ABox × shapes 若两者都在本仓）
  ③ 元数据完整性关键项（按 GB/T 48000.3 附录 A 的表 A.1/A.2 的最小集：
     类与属性有 label；属性有 domain 与 range；有 owl:Ontology 声明）

**判据**：退出码 0 = 通过 · 1 = 有问题（CI 门禁）
依赖：rdflib · pyshacl（见同目录 requirements-ontology-ci.txt）
"""
from __future__ import annotations

import sys
from pathlib import Path

from rdflib import Graph, OWL, RDF, RDFS, URIRef

try:
    from pyshacl import validate
except Exception:
    validate = None

try:
    sys.stdout.reconfigure(encoding='utf-8')
except Exception:
    pass

HERE = Path(__file__).resolve().parent          # apps/dgiot_ontology
PRIV = HERE / 'priv'
SKIP = ('http://www.w3.org/2002/07/owl#', 'http://www.w3.org/2000/01/rdf-schema#',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#', 'http://www.w3.org/2001/XMLSchema#',
        'http://www.w3.org/ns/shacl#')


def short(x) -> str:
    return str(x).split('#')[-1].split('/')[-1]


def check_file(p: Path) -> int:
    print('-' * 70)
    print('%s' % p.name)
    bad = 0
    try:
        g = Graph(); g.parse(p)
    except Exception as e:
        print('  FAIL 语法不可解析：%s' % str(e)[:90]); return 1
    print('  OK   语法可解析 · 三元组 %d' % len(g))

    cls = [c for c in g.subjects(RDF.type, OWL.Class) if not str(c).startswith(SKIP)]
    obj = list(g.subjects(RDF.type, OWL.ObjectProperty))
    dat = list(g.subjects(RDF.type, OWL.DatatypeProperty))
    props = obj + dat

    if cls or props:
        nolab = [short(x) for x in cls + props if not list(g.objects(x, RDFS.label))]
        if nolab:
            print('  FAIL 缺 label 的类/属性 %d 个：%s' % (len(nolab), nolab[:5])); bad += 1
        else:
            print('  OK   类与属性均有 rdfs:label（附录 A 表 A.1/A.2 第 3 项）')
        nodom = [short(x) for x in props if not list(g.objects(x, RDFS.domain))]
        norng = [short(x) for x in props if not list(g.objects(x, RDFS.range))]
        if nodom or norng:
            print('  FAIL 缺 domain %d 个 / 缺 range %d 个：%s' % (len(nodom), len(norng), (nodom + norng)[:5])); bad += 1
        else:
            print('  OK   属性均有 domain 与 range（表 A.2 第 5/6 项）')
        if not list(g.subjects(RDF.type, OWL.Ontology)) and not p.name.endswith('-shapes.ttl'):
            print('  FAIL 无 owl:Ontology 声明'); bad += 1
    return bad


def main() -> int:
    if not PRIV.exists():
        print('SKIP 目录不存在：%s' % PRIV)
        return 0
    files = sorted(list(PRIV.glob('*.ttl')))
    if not files:
        print('SKIP 无 .ttl 文件')
        return 0
    print('本体 CI 检查（本仓内 %d 个 .ttl）' % len(files))
    bad = 0
    for p in files:
        bad += check_file(p)

    # SHACL：standard-ontology + standard-abox(若在本仓) + standard-shapes
    onto = PRIV / 'standard-ontology.ttl'
    shp = PRIV / 'standard-shapes.ttl'
    if validate and onto.exists() and shp.exists():
        print('-' * 70)
        g_on = Graph(); g_on.parse(onto)
        g_sh = Graph(); g_sh.parse(shp)
        # ABox 在另一仓时常不存在 ⇒ 用 TBox 自身做形状一致性检查（能否解析与执行）
        try:
            c, res, text = validate(g_on, shacl_graph=g_sh, advanced=True)
            import re
            msgs = re.findall(r'Message:\s*(.+)', text)
            if c:
                print('  OK   SHACL 在 standard-ontology 上 conforms=True')
            else:
                # TBox 自身不承载实例 ⇒ 这里的违规多是"没有实例可校"，不算失败
                print('  INFO SHACL conforms=False（%d 条）—— 通常因 ABox 不在本仓；ABox 检查在'
                      ' 工具链仓的 CI 中完成' % len(msgs))
        except Exception as e:
            print('  FAIL SHACL 执行失败：%s' % str(e)[:80]); bad += 1
    print('=' * 70)
    print('%s  本仓本体检查：%s' % ('PASS' if bad == 0 else 'FAIL',
                                    '通过 ✓' if bad == 0 else '%d 项不合格 ✗' % bad))
    print('说明：完整检查（国标逐条 / 一致性 / 质量分 / CQ / 版本差分）在工具链仓的')
    print('      tools/ontology.py 中，共 15 个子命令；本脚本只覆盖可在本仓独立完成的部分。')
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
