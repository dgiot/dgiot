#!/usr/bin/env python3
"""Merge 4 ontology parts into unified output"""
import json, sys, hashlib, os
from datetime import datetime

def merge(thing_model_path=None, topology_path=None, deps=None):
    ontology = {
        'version': '2.0',
        'generated': datetime.now().isoformat(),
        'dgaiot': {
            'meta': {
                'ontology': 'DLAS: Data·Logic·Action·Security',
                'pipeline': 'FDE: Model->Ontology->Device->TS->Rules->Dashboard',
                'layers': ['Data', 'Logic', 'Action', 'Security']
            },
            'parts': {}
        }
    }

    # Part 1: Thing Model
    if thing_model_path and os.path.exists(thing_model_path):
        with open(thing_model_path, 'r', encoding='utf-8') as f:
            tm = json.load(f)
        props = tm.get('properties', [])
        events = tm.get('events', [])
        services = tm.get('services', [])
        ontology['dgaiot']['parts']['thing_model'] = {
            'source': thing_model_path,
            'properties': len(props),
            'events': len(events),
            'services': len(services),
            'data_types': list(set(p.get('dataType',{}).get('type','?') for p in props)),
            'protocols': list(set(p.get('dataForm',{}).get('protocol','?') for p in props)),
            'identifiers': [p.get('identifier','?') for p in props[:5]] + ['...']
        }

    # Part 2: Topology
    if topology_path and os.path.exists(topology_path):
        with open(topology_path, 'r', encoding='utf-8') as f:
            topo = json.load(f)
        ontology['dgaiot']['parts']['topology'] = {
            'source': topology_path,
            'sites': topo.get('sites', []),
            'gateways': topo.get('gateways', []),
            'devices': topo.get('devices', []),
            'relations': topo.get('relations', []),
            'counts': topo.get('counts', {})
        }

    # Part 3: Channel (from TDengine schema)
    ontology['dgaiot']['parts']['channel'] = {
        'tdengine': {
            'macro': {'prefix': '_', 'database': '_{ChannelId}', 'table': '_{ProductId}'},
            'tags': ['devaddr NCHAR(50) mandatory'],
            'ets_keys': [
                '{tdengine_db, ChannelId, ProductId} -> DB',
                '{ProductId, "TD"} -> ChannelId',
                '{td, ProductId, DeviceId} -> SubTable'
            ]
        }
    }

    # Part 4: ACL
    ontology['dgaiot']['parts']['acl'] = {
        'layers': [
            {'name': 'Device', 'clientId': '{ProductID}_{DevAddr}', 'auth': 'ProductSecret | DeviceSecret'},
            {'name': 'User', 'clientId': '{Token}{Type}', 'auth': 'SessionToken -> Role -> check_device_acl'},
            {'name': 'Superuser', 'clientId': 'dgiot', 'auth': '127.0.0.1 bypass'}
        ],
        'topic_prefix': '$dg/thing/', 'public_topic': 'ok() — passthrough'
    }

    # Summary
    tm_props = 0
    if thing_model_path and os.path.exists(thing_model_path):
        tm_props = len(json.load(open(thing_model_path))['properties'])
    topo_devices = 0
    if topology_path and os.path.exists(topology_path):
        topo_devices = json.load(open(topology_path)).get('counts', {}).get('devices', 0)
    ontology['summary'] = {
        'thing_model_props': tm_props,
        'topology_devices': topo_devices,
        'parts_count': len(ontology['dgaiot']['parts']),
        'ready': tm_props > 0 and ontology['dgaiot']['parts'].get('topology') is not None
    }

    return ontology

if __name__ == '__main__':
    tm = sys.argv[1] if len(sys.argv) > 1 else None
    tp = sys.argv[2] if len(sys.argv) > 2 else None
    result = merge(thing_model_path=tm, topology_path=tp)
    print(json.dumps(result['summary'], indent=2, ensure_ascii=False))
    # Full output to file
    out = sys.argv[3] if len(sys.argv) > 3 else None
    if out:
        with open(out, 'w', encoding='utf-8') as f:
            json.dump(result, f, indent=2, ensure_ascii=False)
        print(f'Full ontology written to {out}')
