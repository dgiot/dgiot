#!/usr/bin/env python3
"""Scene upgrade — AI analyzes ontology+history, suggests intelligent rules"""
import json, sys, math
from datetime import datetime, timedelta

def analyze_scene(thing_model_path, tdengine_url=None):
    """Read ontology + history, generate upgrade suggestions"""
    with open(thing_model_path, 'r', encoding='utf-8') as f:
        tm = json.load(f)

    suggestions = []

    # 1. Check missing alarms
    for p in tm.get('properties', []):
        alarm = p.get('alarm', {})
        rng = p.get('range', [])
        name = p['name']
        # Properties with range but no alarm -> suggest adding warning threshold
        if not alarm and rng and len(rng) >= 2:
            hi_80pct = round(rng[1] * 0.8, 1)
            suggestions.append({
                'type': 'ADD_ALARM',
                'property': name,
                'detail': f"No alarm defined, suggest alarm_hi={hi_80pct} (80% of range_max={rng[1]})",
                'action': f"Add alarm.high={hi_80pct} to {name}"
            })

    # 2. Detect correlated properties (simple heuristic: same protocol + adjacent address)
    props = tm.get('properties', [])
    for i, p1 in enumerate(props):
        for p2 in props[i+1:]:
            df1 = p1.get('dataForm', {})
            df2 = p2.get('dataForm', {})
            # Same protocol, adjacent address -> likely correlated
            try:
                a1 = int(df1.get('address', '0'))
                a2 = int(df2.get('address', '0'))
                if (df1.get('protocol') == df2.get('protocol') and
                    abs(a1 - a2) <= 2 and a1 != a2):
                    suggestions.append({
                        'type': 'CORRELATION',
                        'detail': f"{p1['name']}(addr={a1}) and {p2['name']}(addr={a2}) "
                                  f"are adjacent registers on {df1.get('protocol','?')}"
                                  f" — consider cross-alarm rule",
                        'action': f"Add rule: IF {p1['name']}_change AND {p2['name']}_change THEN "
                                  f"check co-variation"
                    })
            except: pass

    # 3. Speed/rate properties -> suggest trend detection
    for p in props:
        name = p['name']
        if any(kw in name for kw in ['频率', '电流', '速度', '转速', 'rate', 'speed', 'freq']):
            suggestions.append({
                'type': 'TREND_DETECT',
                'property': name,
                'detail': f"{name} is a rate-type property — suggest sudden_change detection",
                'action': f"Add EdgeStreamEngine rule: sudden_change({name}, threshold=20%)"
            })

    # 4. Status type properties -> suggest auto-response
    for p in props:
        dt = p.get('dataType', {}).get('type', '')
        name = p['name']
        if dt == 'int' and ('status' in name.lower() or '状态' in name):
            suggestions.append({
                'type': 'AUTO_RESPONSE',
                'property': name,
                'detail': f"{name} is a status flag — consider auto-command response",
                'action': f"Add gen_statem rule: IF {name}=0 THEN notify+pump_stop"
            })

    # 5. Dedup, group, rank
    seen = set()
    ranked = []
    for s in suggestions:
        key = f"{s['type']}:{s.get('property','')}:{s['detail'][:60]}"
        if key not in seen:
            seen.add(key)
            ranked.append(s)

    # 6. Group similar suggestions
    correlations = [s for s in ranked if s['type'] == 'CORRELATION']
    trends = [s for s in ranked if s['type'] == 'TREND_DETECT']
    alarms = [s for s in ranked if s['type'] == 'ADD_ALARM']
    others = [s for s in ranked if s['type'] not in ('CORRELATION', 'TREND_DETECT', 'ADD_ALARM')]

    # Group: CORRELATION by protocol
    corr_groups = {}
    for c in correlations:
        proto = 'modbus'
        key = f"corr_{proto}"
        if key not in corr_groups:
            corr_groups[key] = {'type': 'CORRELATION_GROUP', 'count': 0, 'detail': f'Adjacent register correlations on {proto}', 'action': f'Add cross-alarm rules for {proto} registers'}
        corr_groups[key]['count'] += 1

    # Group: TREND_DETECT by prefix
    trend_groups = {}
    for t in trends:
        name = t.get('property','')
        base = name.replace('A相','X').replace('B相','X').replace('C相','X').replace('0','X').replace('1','X').replace('2','X')
        x_name = base.replace('X','[ABC]')
        if base not in trend_groups:
            trend_groups[base] = {'type': 'TREND_DETECT_GROUP', 'count': 0,
                'detail': f'{name} type properties need sudden_change',
                'action': f'Add EdgeStreamEngine: sudden_change for {x_name}'}
        trend_groups[base]['count'] += 1

    # 7. Rank
    priority = {'ADD_ALARM': 5, 'CORRELATION_GROUP': 4, 'AUTO_RESPONSE': 3, 'TREND_DETECT_GROUP': 2, 'TREND_DETECT': 1}
    final = list(corr_groups.values()) + list(trend_groups.values()) + alarms + others
    final.sort(key=lambda s: (-priority.get(s['type'], 0), -s.get('count', 1)))

    return final[:25]  # Top 25 — CORRELATION grouped, others kept individual

if __name__ == '__main__':
    path = sys.argv[1] if len(sys.argv) > 1 else None
    if not path:
        print(json.dumps({'error': 'usage: scene_upgrade.py <thing_model.json>'}, ensure_ascii=False))
        sys.exit(1)

    suggestions = analyze_scene(path)
    print(json.dumps({'suggestions': suggestions, 'count': len(suggestions)}, indent=2, ensure_ascii=False))
