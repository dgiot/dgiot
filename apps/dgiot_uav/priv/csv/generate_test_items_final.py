import pandas as pd
import json
import re
from collections import OrderedDict

# 工位号到站点名称的映射
STATION_NAME_MAP = {
    '0': '磁航向',
    '3': '总测1',
    '1': '总测2',
    '5': '桁架',
    '6': '拷机2',
    '7': '拷机1'
}

def safe_str(val):
    return '' if pd.isna(val) else str(val).strip()

def safe_int(val):
    try:
        return int(float(val)) if not pd.isna(val) else None
    except:
        return None

def infer_action_type(send_content, return_content, protocol, step_desc):
    send = bool(send_content and send_content.strip())
    recv = bool(return_content and return_content.strip())
    if send and recv:
        return 'request_response'
    elif send:
        if protocol in ['Modbus-RTU', 'Modbus-TCP']:
            return 'operate'
        else:
            return 'send'
    elif recv:
        return 'receive'
    else:
        desc = step_desc.lower()
        if '读取' in desc or '接收' in desc:
            return 'receive'
        elif '发送' in desc or '下发' in desc:
            return 'send'
        elif '判断' in desc:
            return 'judge'
        else:
            return 'unknown'

def parse_judge_from_notes(notes):
    if not notes:
        return None
    patterns = [
        (r'(\d+(?:\.\d+)?)\s*±\s*(\d+(?:\.\d+)?)', '±'),
        (r'≥\s*(\d+(?:\.\d+)?)', '>='),
        (r'≤\s*(\d+(?:\.\d+)?)', '<='),
        (r'(\d+(?:\.\d+)?)\s*~\s*(\d+(?:\.\d+)?)', 'range'),
        (r'(\d+(?:\.\d+)?)\s*-\s*(\d+(?:\.\d+)?)', 'range'),
        (r'等于\s*(\d+(?:\.\d+)?)', '=='),
        (r'大于\s*(\d+(?:\.\d+)?)', '>'),
        (r'小于\s*(\d+(?:\.\d+)?)', '<'),
    ]
    for pattern, op in patterns:
        m = re.search(pattern, notes)
        if m:
            if op == '±':
                return {'operator': 'range', 'min': float(m.group(1)) - float(m.group(2)), 'max': float(m.group(1)) + float(m.group(2))}
            elif op in ['>=', '<=', '>', '<', '==']:
                return {'operator': op, 'value': float(m.group(1))}
            elif op == 'range':
                return {'operator': 'range', 'min': float(m.group(1)), 'max': float(m.group(2))}
    return None

def main():
    df = pd.read_excel('测试卡验证V1.3.xlsx', sheet_name=0, header=None, dtype=str)
    # 手动指定列名
    columns = [
        'station_name_raw', 'station_no', 'ip', 'dtu_port', 'comm_port', 'protocol',
        'test_station_name', 'test_category', 'step_no', 'step_desc', 'related_object',
        'hardware_confirm', 'address_code', 'send_content', 'type',
        'chinese_name', 'mapping_name', 'col_r', 'col_s', 'return_content',
        'software_confirm', 'test_result', 'completion_time', 'notes'
    ]
    if len(df.columns) < len(columns):
        columns = columns[:len(df.columns)]
    df.columns = columns
    df = df.dropna(how='all')

    devices = OrderedDict()
    current_station_raw = None
    current_test_station = None
    current_category = None

    for idx, row in df.iterrows():
        if idx <= 1:  # 跳过表头行（假设第2行是表头）
            continue

        station_raw = safe_str(row.get('station_name_raw', ''))
        station_no = safe_str(row.get('station_no', ''))
        ip = safe_str(row.get('ip', ''))
        dtu_port = safe_int(row.get('dtu_port', ''))
        comm_port = safe_int(row.get('comm_port', ''))
        protocol = safe_str(row.get('protocol', ''))
        test_station = safe_str(row.get('test_station_name', ''))
        category = safe_str(row.get('test_category', ''))
        step_no = safe_int(row.get('step_no', ''))
        step_desc = safe_str(row.get('step_desc', ''))
        related_obj = safe_str(row.get('related_object', ''))
        hw_confirm = safe_str(row.get('hardware_confirm', ''))
        addr_code = safe_str(row.get('address_code', ''))
        send_content = safe_str(row.get('send_content', ''))
        type_ = safe_str(row.get('type', ''))
        chinese_name = safe_str(row.get('chinese_name', ''))
        mapping_name = safe_str(row.get('mapping_name', ''))
        return_content = safe_str(row.get('return_content', ''))
        sw_confirm = safe_str(row.get('software_confirm', ''))
        test_result = safe_str(row.get('test_result', ''))
        completion_time = safe_str(row.get('completion_time', ''))
        notes = safe_str(row.get('notes', ''))

        if not step_desc and not send_content and not return_content:
            continue

        if station_raw:
            current_station_raw = station_raw
        if test_station:
            current_test_station = test_station
        if category:
            current_category = category

        if current_category:
            device_key = f"{current_station_raw}_{current_category}"
            device_name = f"{current_category}测试项"
        elif current_test_station:
            device_key = f"{current_station_raw}_{current_test_station}"
            device_name = f"{current_test_station}测试项"
        else:
            device_key = current_station_raw
            device_name = f"{current_station_raw}测试项"

        if device_key not in devices:
            devices[device_key] = {
                'device_address': device_key,
                'device_name': device_name,
                'station_name': STATION_NAME_MAP.get(station_no, current_station_raw),
                'test_steps': []
            }

        # 构建通信参数
        comm = {}
        if ip:
            comm['ip'] = ip
        if dtu_port is not None:
            comm['dtu_port'] = dtu_port
        if comm_port is not None:
            comm['port'] = comm_port
        if protocol:
            comm['protocol'] = protocol

        # 构建 send 对象
        send_obj = None
        if send_content:
            send_obj = {'content': send_content}
            if addr_code:
                send_obj['address'] = addr_code
            if 'Modbus' in protocol:
                send_obj['format'] = 'modbus'
            elif protocol in ['遥控', '遥测']:
                send_obj['format'] = 'hex'
            else:
                send_obj['format'] = 'raw'

        # 构建 receive 对象
        receive_obj = None
        if return_content:
            receive_obj = {'content': return_content}
            if type_:
                receive_obj['type'] = type_
            if chinese_name:
                receive_obj['chinese_name'] = chinese_name
            if mapping_name:
                receive_obj['mapping_name'] = mapping_name

        # 推断动作类型
        action_type = infer_action_type(send_content, return_content, protocol, step_desc)

        # 尝试从 notes 提取判定规则
        judge_obj = parse_judge_from_notes(notes)
        if not judge_obj and ('判断' in step_desc or '合格' in step_desc):
            judge_obj = {'description': notes if notes else step_desc}

        step = {
            'step_number': step_no if step_no is not None else idx,
            'action_type': action_type,
            'description': step_desc,
            'target': related_obj,
            'communication': comm if comm else None,
            'send': send_obj,
            'receive': receive_obj,
            'judge': judge_obj,
            'notes': notes if notes else None
        }
        # 移除 None 字段
        step = {k: v for k, v in step.items() if v is not None}

        devices[device_key]['test_steps'].append(step)

    # 转换为列表
    test_items_full = list(devices.values())

    # 生成摘要
    test_items_summary = [
        {
            'device_address': dev['device_address'],
            'device_name': dev['device_name'],
            'station_name': dev['station_name'],
            'test_step_count': len(dev['test_steps'])
        }
        for dev in test_items_full
    ]

    # 写入文件
    with open('test_items_full.json', 'w', encoding='utf-8') as f:
        json.dump(test_items_full, f, ensure_ascii=False, indent=2)

    with open('test_items_summary.json', 'w', encoding='utf-8') as f:
        json.dump(test_items_summary, f, ensure_ascii=False, indent=2)

    print(f"生成完成：共 {len(test_items_full)} 个测试项，总计步骤数：{sum(len(dev['test_steps']) for dev in test_items_full)}")

if __name__ == '__main__':
    main()