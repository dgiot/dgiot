#!/usr/bin/env python3
"""Extract topology from io_ontology.json + network scans"""
import json, sys, socket

def extract_from_io_ontology(path):
    with open(path, 'r', encoding='utf-8') as f:
        data = json.load(f)

    sites = []
    gateways = []
    devices = []

    # Servers -> Gateways
    for srv in data.get('servers', []):
        gw = {
            'id': srv['id'].replace('io:', ''),
            'ip': srv.get('ip', ''),
            'hostname': srv.get('hostname', ''),
            'role': srv.get('role', ''),
            'site': 'oil_field_01',
            'notes': srv.get('notes', '')
        }
        gateways.append(gw)

    # Data sources -> Devices
    for ds in data.get('data_sources', []):
        dev = {
            'id': f"ds_{ds['id']}",
            'name': ds['name'],
            'type': ds.get('type', ''),
            'protocol': ds.get('protocol', ''),
            'devices_count': ds.get('devices', 0),
            'endpoint': ds.get('endpoint', ''),
            'status': ds.get('status', '')
        }
        devices.append(dev)

    # DCS endpoints -> Devices
    for dcs in data.get('dcs_endpoints', []):
        dev = {
            'id': f"dcs_{dcs['id']}",
            'name': f"DCS{dcs['id']} ({dcs.get('vendor','')})",
            'type': 'dcs',
            'protocol': 'OPC DA',
            'subnet': dcs.get('subnet', ''),
            'devices_count': dcs.get('devices', 0),
            'status': dcs.get('status', '')
        }
        devices.append(dev)

    # Wireless terminals -> Devices
    for term in data.get('wireless_terminals', []):
        devices.append({
            'id': term,
            'name': term,
            'type': 'wireless_terminal',
            'protocol': 'GPRS/DTU',
            'status': 'online'
        })

    # Relationships
    relations = data.get('relationships', [])

    # Ports
    ports = data.get('ports', [])

    return {
        'sites': sites,
        'gateways': gateways,
        'devices': devices,
        'relations': relations,
        'ports': ports,
        'counts': {
            'gateways': len(gateways),
            'devices': len(devices),
            'wireless': len(data.get('wireless_terminals', [])),
            'dcs': len(data.get('dcs_endpoints', []))
        }
    }

if __name__ == '__main__':
    path = sys.argv[1] if len(sys.argv) > 1 else None
    if not path:
        print(json.dumps({'error': 'usage: extract_topology.py <io_ontology.json>'}))
        sys.exit(1)
    result = extract_from_io_ontology(path)
    print(json.dumps(result, indent=2, ensure_ascii=False))
