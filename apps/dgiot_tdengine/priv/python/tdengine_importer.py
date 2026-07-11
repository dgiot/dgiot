#!/usr/bin/env python3
"""
TDengine数据导入器 - Python版本（整合成功经验）
支持多种导入模式：
1. TDengine FILE导入（推荐，基于成功经验）
2. Python批量单行INSERT（备用方案）
"""

import sys
import os
import subprocess
import time
import argparse
from datetime import datetime
from typing import List, Tuple, Optional

class TDengineImporter:
    """TDengine数据导入器"""
    
    def __init__(self, db_name: str = "_24b9b4bc50"):
        self.db_name = db_name
        self.container_name = "tdengine-tsdb"
        
    def execute_sql(self, sql: str) -> Tuple[int, str, str]:
        """执行SQL命令"""
        cmd = f'docker exec {self.container_name} taos -s "{sql}"'
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        return result.returncode, result.stdout, result.stderr
    
    def execute_sql_batch(self, sql_commands: List[str]) -> Tuple[int, str, str]:
        """批量执行SQL命令"""
        # 将所有SQL命令合并为一个脚本
        script_content = "\n".join(sql_commands)
        
        # 写入临时文件
        temp_file = f"/tmp/tdengine_batch_{os.getpid()}.sql"
        with open(temp_file, 'w') as f:
            f.write(script_content)
        
        # 复制到Docker并执行
        cmd = f'docker cp {temp_file} {self.container_name}:/tmp/ && docker exec {self.container_name} taos -f /tmp/$(basename {temp_file})'
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        
        # 清理临时文件
        os.remove(temp_file)
        
        return result.returncode, result.stdout, result.stderr
    
    def clear_table(self, table_name: str) -> bool:
        """清空表"""
        print(f"清空表: {table_name}")
        sql = f"USE {self.db_name}; DELETE FROM {table_name};"
        code, out, err = self.execute_sql(sql)
        
        if code != 0:
            print(f"清空表失败: {err[:200]}")
            return False
        
        print("清空表成功")
        return True
    
    def import_csv(self, csv_file: str, table_name: str, batch_size: int = 1000) -> bool:
        """导入CSV文件（整合成功经验）"""
        print(f"开始导入: {csv_file} -> {table_name}")
        
        # 首先尝试TDengine FILE导入（基于成功经验）
        print("尝试TDengine FILE导入（基于成功经验）...")
        if self.try_file_import(csv_file, table_name):
            return True
        
        print("FILE导入失败，使用Python批量导入...")
        
        # 1. 读取CSV文件
        print("读取CSV文件...")
        try:
            with open(csv_file, 'r', encoding='utf-8') as f:
                lines = f.readlines()
        except Exception as e:
            print(f"读取CSV文件失败: {e}")
            return False
        
        # 检查是否有标题行
        first_line = lines[0].strip().lower()
        if 'createdat' in first_line or 'v410_0' in first_line:
            total_lines = len(lines) - 1  # 减去标题行
            data_start = 1
            print(f"检测到标题行，跳过第一行")
        else:
            total_lines = len(lines)
            data_start = 0
        
        print(f"总数据行数: {total_lines}")
        
        if total_lines <= 0:
            print("错误: 没有数据行")
            return False
        
        # 2. 清空表
        if not self.clear_table(table_name):
            return False
        
        # 3. 分批导入
        print("开始分批导入数据...")
        start_time = time.time()
        
        batch_count = 0
        current_batch = []
        imported = 0
        errors = 0
        
        for i, line in enumerate(lines[data_start:], 1):
            line = line.strip()
            if not line:
                continue
            
            # 基于成功经验：保持原始格式，不清理引号和空格
            # TDengine可以处理带引号和空格的CSV格式
            values = line.split(',')
            
            # 确保有31个值（根据表结构）
            if len(values) != 31:
                values = values[:31]
                while len(values) < 31:
                    values.append('0')
            
            # 商用环境限制：时间戳必须在一年范围内
            # 使用相对时间戳：NOW() - offset_seconds
            max_offset_seconds = 31536000  # 一年秒数
            offset_seconds = min((total_lines - i) * 10, max_offset_seconds)
            
            # 使用相对时间：NOW() - offset_seconds
            time_expr = f"NOW() - {offset_seconds}s"
            values[0] = time_expr
            
            # 记录时间范围信息
            if i == 1:
                print(f"时间戳范围: 最早 = NOW() - {offset_seconds}s")
            elif i == total_lines:
                print(f"时间戳范围: 最晚 = NOW()")
            
            # 构建INSERT语句
            values_str = ','.join(values)
            sql = f"USE {self.db_name}; INSERT INTO {table_name} VALUES ({values_str});"
            current_batch.append(sql)
            
            # 达到批处理大小时执行
            if len(current_batch) >= batch_size or i == total_lines:
                batch_count += 1
                
                # 执行当前批次
                code, out, err = self.execute_sql_batch(current_batch)
                
                if code == 0:
                    imported += len(current_batch)
                else:
                    errors += len(current_batch)
                    print(f"批次 {batch_count} 失败: {err[:200]}")
                
                # 清空当前批次
                current_batch = []
                
                # 进度显示
                if batch_count % 10 == 0 or i == total_lines:
                    elapsed = time.time() - start_time
                    rate = i / elapsed if elapsed > 0 else 0
                    percent = i / total_lines * 100
                    remaining = (total_lines - i) / rate if rate > 0 else 0
                    
                    print(f"进度: {i}/{total_lines} ({percent:.1f}%), "
                          f"速度: {rate:.1f} 行/秒, "
                          f"批次: {batch_count}, 错误: {errors}")
                    
                    if remaining > 0:
                        remaining_str = time.strftime("%H:%M:%S", time.gmtime(remaining))
                        print(f"预计剩余时间: {remaining_str}")
        
        # 4. 验证结果
        print("验证导入结果...")
        sql_verify = f"USE {self.db_name}; SELECT count(*) as 总行数, min(createdat) as 最早时间, max(createdat) as 最晚时间 FROM {table_name};"
        code, out, err = self.execute_sql(sql_verify)
        
        if code == 0:
            print("查询结果:")
            # 提取并显示结果
            for line in out.split('\n'):
                line = line.strip()
                if line and not any(x in line for x in ['Welcome', 'taos>', 'Database', 'Copyright']):
                    print(f"  {line}")
        else:
            print(f"查询失败: {err[:200]}")
        
        total_time = time.time() - start_time
        print(f"\n导入完成!")
        print(f"总耗时: {total_time:.1f} 秒")
        print(f"导入行数: {imported}/{total_lines}")
        print(f"错误数: {errors}")
        print(f"平均速度: {imported/total_time:.1f} 行/秒")
        
        return errors == 0
    
    def try_file_import(self, csv_file: str, table_name: str) -> bool:
        """尝试TDengine FILE导入（基于成功经验）"""
        try:
            # 关键发现：TDengine FILE导入不能有标题行
            # 检查文件是否有标题行
            with open(csv_file, 'r', encoding='utf-8') as f:
                first_line = f.readline().strip()
            
            # 如果第一行看起来像标题（包含createdat等字段名），需要创建无标题行版本
            if 'createdat' in first_line.lower() or 'v410_0' in first_line:
                print(f"检测到标题行，创建无标题行版本...")
                noheader_file = f"/tmp/{os.path.basename(csv_file)}_noheader.csv"
                
                # 创建无标题行文件
                with open(csv_file, 'r', encoding='utf-8') as f_in, \
                     open(noheader_file, 'w', encoding='utf-8') as f_out:
                    # 跳过第一行（标题行）
                    f_in.readline()
                    for line in f_in:
                        f_out.write(line)
                
                use_file = noheader_file
                is_temp_file = True
            else:
                use_file = csv_file
                is_temp_file = False
            
            # 复制文件到Docker容器
            docker_file = f"/tmp/tdengine_import_{os.getpid()}.csv"
            copy_cmd = f"docker cp {use_file} tdengine-tsdb:{docker_file}"
            result = subprocess.run(copy_cmd, shell=True, capture_output=True, text=True)
            
            if result.returncode != 0:
                print(f"复制文件到Docker失败: {result.stderr[:200]}")
                if is_temp_file:
                    os.remove(use_file)
                return False
            
            # 尝试两种语法：INSERT FILE 和 file ... into
            syntaxes = [
                f"USE {self.db_name}; INSERT INTO {table_name} FILE '{docker_file}';",
                f"USE {self.db_name}; file '{docker_file}' into {table_name};"
            ]
            
            for i, import_cmd in enumerate(syntaxes):
                syntax_name = "INSERT FILE" if i == 0 else "file ... into"
                print(f"尝试{syntax_name}语法...")
                
                full_cmd = f'docker exec tdengine-tsdb taos -s "{import_cmd}"'
                result = subprocess.run(full_cmd, shell=True, capture_output=True, text=True)
                
                if result.returncode == 0 and ("Insert OK" in result.stdout or "Query OK" in result.stdout):
                    # 提取导入统计信息
                    import re
                    rows_match = re.search(r'Insert OK, ([\d,]+) row', result.stdout)
                    time_match = re.search(r'\(([\d.]+)s\)', result.stdout)
                    
                    rows = rows_match.group(1) if rows_match else "N/A"
                    time_taken = time_match.group(1) if time_match else "N/A"
                    
                    print(f"{syntax_name}语法导入成功！导入 {rows} 行，耗时 {time_taken} 秒")
                    
                    # 清理临时文件
                    if is_temp_file:
                        os.remove(use_file)
                    
                    return True
                else:
                    # 检查常见错误
                    error_output = result.stderr if result.stderr else result.stdout
                    if "Timestamp data out of range" in error_output:
                        print(f"{syntax_name}语法失败: 时间戳超出范围（可能超过一年限制）")
                    elif "syntax error" in error_output and ("createdat" in error_output or "file" in error_output):
                        print(f"{syntax_name}语法失败: 语法错误")
                    elif "invalid data or symbol" in error_output:
                        print(f"{syntax_name}语法失败: 数据格式错误（字段数量或格式不匹配）")
                    else:
                        if i == len(syntaxes) - 1:  # 最后一个语法也失败
                            print(f"所有FILE导入语法都失败，最后错误: {error_output[:200]}")
                        else:
                            print(f"{syntax_name}语法失败，尝试下一种语法...")
            
            # 所有语法都失败
            # 清理临时文件
            if is_temp_file:
                os.remove(use_file)
            
            return False
                
        except Exception as e:
            print(f"FILE导入异常: {e}")
            # 清理临时文件
            if 'use_file' in locals() and is_temp_file and os.path.exists(use_file):
                os.remove(use_file)
            return False
    
    def verify_import(self, table_name: str) -> bool:
        """验证导入结果"""
        print(f"验证表: {table_name}")
        
        sql = f"USE {self.db_name}; SELECT count(*) as 总行数 FROM {table_name};"
        code, out, err = self.execute_sql(sql)
        
        if code != 0:
            print(f"验证失败: {err[:200]}")
            return False
        
        # 提取行数
        for line in out.split('\n'):
            if '|' in line and '总行数' not in line:
                count = line.split('|')[1].strip()
                print(f"表 {table_name} 有 {count} 行数据")
                return True
        
        return False

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='TDengine数据导入器')
    parser.add_argument('csv_file', help='CSV文件路径')
    parser.add_argument('table_name', help='目标表名')
    parser.add_argument('db_name', nargs='?', default='_24b9b4bc50', help='数据库名（默认: _24b9b4bc50）')
    parser.add_argument('--batch-size', type=int, default=1000, help='批处理大小（默认: 1000）')
    parser.add_argument('--verify', action='store_true', help='仅验证，不导入')
    
    args = parser.parse_args()
    
    # 检查文件
    if not os.path.exists(args.csv_file):
        print(f"错误: CSV文件不存在: {args.csv_file}")
        sys.exit(1)
    
    # 创建导入器
    importer = TDengineImporter(args.db_name)
    
    if args.verify:
        # 仅验证模式
        if importer.verify_import(args.table_name):
            print("验证成功")
            sys.exit(0)
        else:
            print("验证失败")
            sys.exit(1)
    else:
        # 导入模式
        if importer.import_csv(args.csv_file, args.table_name, args.batch_size):
            print("导入成功")
            sys.exit(0)
        else:
            print("导入失败")
            sys.exit(1)

if __name__ == "__main__":
    main()
