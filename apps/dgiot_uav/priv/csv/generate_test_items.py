import pandas as pd
import json
from collections import OrderedDict

# 工位号到站点名称的映射（根据您的命名调整）
STATION_NAME_MAP = {
    '0': '磁航向',
    '3': '总测1',
    '1': '总测2',
    '2': '总测3',
    '4': '总测4',
    '5': '桁架',
    '6': '拷机2',
    '7': '拷机1'
}

def safe_str(val):
    """将NaN或None转为空字符串"""
    return '' if pd.isna(val) else str(val).strip()

def safe_int(val):
    """安全转换为整数，失败返回None"""
    try:
        return int(val) if not pd.isna(val) else None
    except:
        return None

def safe_float(val):
    """安全转换为浮点数，失败返回None"""
    try:
        return float(val) if not pd.isna(val) else None
    except:
        return None

def main():
    # 读取Excel文件，默认读取第一个sheet（Sheet1）
    df = pd.read_excel('测试卡验证V1.3.xlsx', sheet_name=0, header=None, dtype=str)

    # 找到表头行（通常在第2行，索引1）
    header_row = 1  # 如果您的文件表头在第2行（行号从0开始），根据实际情况调整
    # 手动指定列名（根据Excel列顺序）
    columns = [
        'station_name_raw', 'station_no', 'ip', 'dtu_port', 'comm_port', 'protocol',
        'test_station_name', 'test_category', 'step_no', 'step_desc', 'related_object',
        'hardware_confirm', 'address_code', 'send_content', 'type',
        'chinese_name', 'mapping_name', 'col_r', 'col_s', 'return_content',
        'software_confirm', 'test_result', 'completion_time', 'notes'
    ]
    # 如果列数不匹配，取前24列
    if len(df.columns) < len(columns):
        columns = columns[:len(df.columns)]
    df.columns = columns

    # 删除全为空的行
    df = df.dropna(how='all')

    # 初始化数据结构
    devices = OrderedDict()  # 使用有序字典保持插入顺序
    current_station_raw = None
    current_test_station = None
    current_category = None

    for idx, row in df.iterrows():
        # 跳过表头行
        if idx <= header_row:
            continue

        # 获取字段
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

        # 如果整行关键字段为空，跳过
        if not step_desc and not send_content and not return_content:
            continue

        # 更新当前上下文
        if station_raw:
            current_station_raw = station_raw
        if test_station:
            current_test_station = test_station
        if category:
            current_category = category

        # 构建设备地址：站点名_测试项（若测试项为空，则使用测试工位名）
        if current_category:
            device_key = f"{current_station_raw}_{current_category}"
            device_name = f"{current_category}测试项"
        elif current_test_station:
            device_key = f"{current_station_raw}_{current_test_station}"
            device_name = f"{current_test_station}测试项"
        else:
            # 如果既无具体类目也无测试工位名，则使用站点名本身
            device_key = current_station_raw
            device_name = f"{current_station_raw}测试项"

        # 如果设备不存在，初始化
        if device_key not in devices:
            devices[device_key] = {
                'device_address': device_key,
                'device_name': device_name,
                'station_name': STATION_NAME_MAP.get(station_no, current_station_raw),
                'test_steps': []
            }

        # 构建通信参数（仅当IP或端口存在时）
        comm_params = {}
        if ip:
            comm_params['communication_ip'] = ip
        if dtu_port is not None:
            comm_params['dtu_port'] = dtu_port
        if comm_port is not None:
            comm_params['communication_port'] = comm_port
        if protocol:
            comm_params['protocol'] = protocol
        # 如果没有任何通信参数，则不添加该字段
        if not comm_params:
            comm_params = None

        # 构建步骤对象
        step = {
            'step_number': step_no if step_no is not None else idx,  # 若步骤号缺失，使用行号作为后备
            'type': type_,
            'message_content_send': send_content,
            'detailed_step': step_desc,
            'associated_object': related_obj,
            'hardware_confirm': hw_confirm,
        }
        if comm_params:
            step['communication_params'] = comm_params

        devices[device_key]['test_steps'].append(step)

    # 转换为列表
    test_items_full = list(devices.values())

    # 生成test_items_summary
    test_items_summary = [
        {
            'device_address': dev['device_address'],
            'device_name': dev['device_name'],
            'station_name': dev['station_name'],
            'test_step_count': len(dev['test_steps'])
        }
        for dev in test_items_full
    ]

    # 保存为JSON文件
    with open('test_items_full.json', 'w', encoding='utf-8') as f:
        json.dump(test_items_full, f, ensure_ascii=False, indent=2)

    with open('test_items_summary.json', 'w', encoding='utf-8') as f:
        json.dump(test_items_summary, f, ensure_ascii=False, indent=2)

    print(f"生成完成：共 {len(test_items_full)} 个测试项，总计步骤数：{sum(len(dev['test_steps']) for dev in test_items_full)}")

if __name__ == '__main__':
    main()