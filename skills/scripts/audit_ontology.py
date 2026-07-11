#!/usr/bin/env python3
"""Audit ontology — 7 automated checks before deployment"""
import json, sys, hashlib
from collections import defaultdict

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def audit_thing_model(tm):
    issues = []

    # 1. Register address conflicts (thing model is a template — only flag same address within a product)
    addrs = defaultdict(list)
    for p in tm.get('properties', []):
        df = p.get('dataForm', {})
        addr = df.get('address', '')
        if addr and addr != '0' and addr != '1' and addr != '2' and addr != '4':
            addrs[addr].append(p['name'])
    for addr, names in addrs.items():
        if len(names) > 1:
            issues.append({
                'severity': 'WARN',
                'type': 'ADDR_SHARED',
                'detail': f"Address {addr} shared by {len(names)} props (may be OK if different slaveid): {', '.join(names[:3])}..."
            })

    # 2. Data type mismatch
    for p in tm.get('properties', []):
        df = p.get('dataForm', {})
        dt = p.get('dataType', {})
        fmt = df.get('originaltype', '')
        dtype = dt.get('type', '')
        if 'float32' in fmt and dtype not in ('float', ''):
            issues.append({'severity': 'WARN', 'type': 'TYPE_MISMATCH',
                'detail': f"{p['name']}: register={fmt} but dataType={dtype}"})

    # 3. Alarm threshold OOB
    for p in tm.get('properties', []):
        alarm = p.get('alarm', {})
        rng = p.get('range', [])
        hi = alarm.get('high')
        if hi and len(rng) >= 2 and hi > rng[1]:
            issues.append({'severity': 'WARN', 'type': 'ALARM_OOB',
                'detail': f"{p['name']}: alarm_hi={hi} > range_max={rng[1]}"})

    return issues

def audit_devices(devices):
    issues = []
    devaddrs = []
    for d in devices:
        da = d.get('devaddr', d.get('id', ''))
        if da:
            devaddrs.append(da)
        pid = d.get('productid', d.get('product', ''))
        if not pid:
            issues.append({'severity': 'CRITICAL', 'type': 'ACL_GAP',
                'detail': f"Device {da}: no productid — ACL will deny all access"})

    # 4. Duplicate devaddr
    dupes = [da for da in devaddrs if devaddrs.count(da) > 1]
    for da in set(dupes):
        issues.append({'severity': 'CRITICAL', 'type': 'DEVADDR_DUPLICATE',
            'detail': f"devaddr {da} appears {devaddrs.count(da)} times"})

    return issues

def audit_topology(topo):
    issues = []
    ports = {}
    for p in topo.get('ports', []):
        key = (p.get('server_id'), p['port'])
        if key in ports:
            issues.append({'severity': 'WARN', 'type': 'PORT_CONFLICT',
                'detail': f"Server {key[0]} port {key[1]}: {ports[key]} vs {p.get('service','?')}"})
        ports[key] = p.get('service', '?')
    return issues

def audit_tdengine_names(devices):
    issues = []
    for d in devices:
        pid = d.get('productid', 'unknown')
        da = d.get('devaddr', d.get('id', 'unknown'))
        name = f"sub_{pid}_{da}"
        if len(name) > 192:
            issues.append({'severity': 'CRITICAL', 'type': 'TABLE_TOO_LONG',
                'detail': f"{name} ({len(name)} chars, TD limit 192)"})
    return issues

def audit_all(thing_model_path=None, devices_path=None, topology_path=None):
    report = {'status': 'PASS', 'issues': [], 'summary': {}}
    counts = {'CRITICAL': 0, 'WARN': 0, 'INFO': 0}

    if thing_model_path:
        report['issues'] += audit_thing_model(load_json(thing_model_path))
    if devices_path:
        report['issues'] += audit_devices(load_json(devices_path))
    if topology_path:
        report['issues'] += audit_topology(load_json(topology_path))
    if devices_path:
        report['issues'] += audit_tdengine_names(load_json(devices_path))

    for i in report['issues']:
        counts[i['severity']] = counts.get(i['severity'], 0) + 1

    if counts.get('CRITICAL', 0) > 0:
        report['status'] = 'FAIL'
    elif counts.get('WARN', 0) > 0:
        report['status'] = 'WARN'
    else:
        report['status'] = 'PASS'

    report['summary'] = counts
    return report

if __name__ == '__main__':
    tm = sys.argv[1] if len(sys.argv) > 1 else None
    dv = sys.argv[2] if len(sys.argv) > 2 else None
    tp = sys.argv[3] if len(sys.argv) > 3 else None

    report = audit_all(tm, dv, tp)
    print(json.dumps(report, indent=2, ensure_ascii=False))
    sys.exit(0 if report['status'] == 'PASS' else 1)
