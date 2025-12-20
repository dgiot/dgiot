#!/usr/bin/env python3
"""
TDengine通用工具包 - 基于经验教训的智能导入导出工具

功能特性：
1. 智能CSV导入：自动处理标题行、字段数量、时间戳等问题
2. 数据导出：支持CSV格式导出
3. 数据验证：导入前验证数据质量
4. 性能监控：监控导入导出过程
5. 错误恢复：自动诊断和恢复常见错误

基于TDengine经验教训：
1. FILE导入不能有标题行
2. 字段数量必须与表结构匹配
3. 商用环境时间戳限制在一年内
4. 支持多种导入语法
"""

import sys
import os
import csv
import json
import time
import logging
import argparse
import subprocess
from datetime import datetime, timedelta
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass, field
from pathlib import Path

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('tdengine_toolkit.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

@dataclass
class TDengineConfig:
    """TDengine配置"""
    host: str = "localhost"
    port: int = 6030
    user: str = "root"
    password: str = "taosdata"
    database: str = ""
    container_name: str = "tdengine-tsdb"
    
    # 导入配置
    batch_size: int = 1000
    max_retries: int = 3
    timeout: int = 300
    
    # CSV配置
    csv_encoding: str = "utf-8"
    csv_delimiter: str = ","
    csv_quote_char: str = '"'
    csv_has_header: bool = True
    
    # 商用环境限制
    commercial_mode: bool = False
    max_timestamp_range_days: int = 365  # 一年
    
    def to_dict(self) -> Dict[str, Any]:
        """转换为字典"""
        return {k: v for k, v in self.__dict__.items() if not k.startswith('_')}

class TDengineToolkit:
    """TDengine通用工具包"""
    
    def __init__(self, config: Optional[TDengineConfig] = None):
        self.config = config or TDengineConfig()
        self.error_messages = self._load_error_messages()
        
    def _load_error_messages(self) -> Dict[str, str]:
        """加载错误消息映射"""
        return {
            "Timestamp data out of range": "时间戳超出范围（可能超过一年限制）",
            "syntax error near 'createdat'": "CSV文件有标题行",
            "invalid data or symbol": "字段数量或格式不匹配",
            "invalid timestamp": "时间戳格式错误",
            "file not found": "文件不存在",
            "permission denied": "权限不足",
            "connection refused": "连接被拒绝",
            "authentication failed": "认证失败"
        }
    
    def diagnose_error(self, error: str) -> str:
        """诊断错误"""
        for pattern, diagnosis in self.error_messages.items():
            if pattern.lower() in error.lower():
                return diagnosis
        return f"未知错误: {error[:100]}"
    
    def execute_sql(self, sql: str, database: str = "") -> Tuple[int, str, str]:
        """执行SQL命令"""
        try:
            # 构建命令
            db_part = f"-D {database}" if database else ""
            cmd = f"taos {db_part} -s \"{sql}\""
            
            # 如果是Docker环境
            if self.config.container_name:
                cmd = f"docker exec {self.config.container_name} {cmd}"
            
            logger.debug(f"执行SQL: {sql}")
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=self.config.timeout)
            
            return result.returncode, result.stdout, result.stderr
        except subprocess.TimeoutExpired:
            return 1, "", "命令执行超时"
        except Exception as e:
            return 1, "", f"执行命令异常: {str(e)}"
    
    def validate_csv_file(self, csv_file: str) -> Dict[str, Any]:
        """验证CSV文件"""
        validation_result = {
            "file_exists": False,
            "file_size": 0,
            "total_rows": 0,
            "data_rows": 0,
            "field_count": 0,
            "has_header": False,
            "encoding": "unknown",
            "timestamp_format": "unknown",
            "issues": []
        }
        
        try:
            # 检查文件是否存在
            if not os.path.exists(csv_file):
                validation_result["issues"].append("文件不存在")
                return validation_result
            
            validation_result["file_exists"] = True
            validation_result["file_size"] = os.path.getsize(csv_file)
            
            # 检查编码
            try:
                with open(csv_file, 'r', encoding='utf-8') as f:
                    f.read(1024)
                validation_result["encoding"] = "utf-8"
            except UnicodeDecodeError:
                validation_result["encoding"] = "非UTF-8"
                validation_result["issues"].append("文件编码不是UTF-8")
            
            # 读取文件内容
            with open(csv_file, 'r', encoding=self.config.csv_encoding, errors='ignore') as f:
                lines = f.readlines()
            
            if not lines:
                validation_result["issues"].append("文件为空")
                return validation_result
            
            validation_result["total_rows"] = len(lines)
            
            # 检查标题行
            first_line = lines[0].strip().lower()
            if 'createdat' in first_line or any(f'v{i}' in first_line for i in range(1000)):
                validation_result["has_header"] = True
                validation_result["data_rows"] = len(lines) - 1
            else:
                validation_result["data_rows"] = len(lines)
            
            # 检查字段数量
            sample_line = lines[1] if validation_result["has_header"] else lines[0]
            field_count = len(sample_line.strip().split(self.config.csv_delimiter))
            validation_result["field_count"] = field_count
            
            # 检查时间戳格式
            if validation_result["data_rows"] > 0:
                data_line = lines[1] if validation_result["has_header"] else lines[0]
                first_field = data_line.strip().split(self.config.csv_delimiter)[0]
                
                # 检查时间戳格式
                timestamp_formats = [
                    r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3}',  # 2023-01-01 00:00:00.000
                    r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}',         # 2023-01-01 00:00:00
                    r'\d{4}-\d{2}-\d{2}',                           # 2023-01-01
                ]
                
                import re
                for fmt in timestamp_formats:
                    if re.match(fmt, first_field.strip('"')):
                        validation_result["timestamp_format"] = fmt
                        break
                
                if validation_result["timestamp_format"] == "unknown":
                    validation_result["issues"].append("时间戳格式无法识别")
            
            # 检查商用环境时间限制
            if self.config.commercial_mode and validation_result["data_rows"] > 0:
                # 检查时间戳范围（简单检查）
                try:
                    timestamps = []
                    for i, line in enumerate(lines):
                        if i == 0 and validation_result["has_header"]:
                            continue
                        if i > 10:  # 只检查前10行
                            break
                        
                        fields = line.strip().split(self.config.csv_delimiter)
                        if fields:
                            timestamp_str = fields[0].strip('"')
                            try:
                                if '.' in timestamp_str:
                                    dt = datetime.strptime(timestamp_str, "%Y-%m-%d %H:%M:%S.%f")
                                else:
                                    dt = datetime.strptime(timestamp_str, "%Y-%m-%d %H:%M:%S")
                                timestamps.append(dt)
                            except ValueError:
                                pass
                    
                    if timestamps:
                        min_ts = min(timestamps)
                        max_ts = max(timestamps)
                        days_diff = (max_ts - min_ts).days
                        
                        if days_diff > self.config.max_timestamp_range_days:
                            validation_result["issues"].append(
                                f"时间戳范围超过商用环境限制: {days_diff}天 > {self.config.max_timestamp_range_days}天"
                            )
                except Exception as e:
                    logger.warning(f"检查时间戳范围失败: {e}")
            
            logger.info(f"CSV验证完成: {validation_result}")
            return validation_result
            
        except Exception as e:
            validation_result["issues"].append(f"验证过程中发生异常: {str(e)}")
            return validation_result
    
    def fix_csv_issues(self, csv_file: str, validation_result: Dict[str, Any]) -> Optional[str]:
        """修复CSV文件问题"""
        try:
            # 创建修复后的文件
            base_name = os.path.splitext(csv_file)[0]
            fixed_file = f"{base_name}_fixed.csv"
            
            with open(csv_file, 'r', encoding=self.config.csv_encoding, errors='ignore') as f_in, \
                 open(fixed_file, 'w', encoding=self.config.csv_encoding) as f_out:
                
                lines = f_in.readlines()
                if not lines:
                    return None
                
                # 处理标题行
                write_header = not self.config.csv_has_header  # 如果配置不需要标题行，就不写
                if validation_result["has_header"] and write_header:
                    f_out.write(lines[0])
                    data_start = 1
                else:
                    data_start = 0
                
                # 处理数据行
                expected_fields = validation_result.get("expected_fields", 31)
                
                for i in range(data_start, len(lines)):
                    line = lines[i].strip()
                    if not line:
                        continue
                    
                    fields = line.split(self.config.csv_delimiter)
                    
                    # 修复字段数量
                    if len(fields) < expected_fields:
                        # 添加缺失字段
                        fields.extend(['0'] * (expected_fields - len(fields)))
                    elif len(fields) > expected_fields:
                        # 删除多余字段
                        fields = fields[:expected_fields]
                    
                    # 商用环境：调整时间戳
                    if self.config.commercial_mode and fields:
                        # 使用相对时间戳
                        offset_seconds = min((len(lines) - i) * 10, self.config.max_timestamp_range_days * 86400)
                        fields[0] = f"NOW() - {offset_seconds}s"
                    
                    f_out.write(self.config.csv_delimiter.join(fields) + '\n')
            
            logger.info(f"CSV文件修复完成: {fixed_file}")
            return fixed_file
            
        except Exception as e:
            logger.error(f"修复CSV文件失败: {e}")
            return None
    
    def import_csv(self, csv_file: str, table_name: str, database: str = "") -> bool:
        """导入CSV文件"""
        logger.info(f"开始导入CSV文件: {csv_file} -> {table_name}")
        
        start_time = time.time()
        
        # 1. 验证CSV文件
        logger.info("验证CSV文件...")
        validation_result = self.validate_csv_file(csv_file)
        
        if validation_result["issues"]:
            logger.warning(f"CSV文件存在问题: {validation_result['issues']}")
            
            # 尝试修复问题
            fixed_file = self.fix_csv_issues(csv_file, validation_result)
            if fixed_file:
                logger.info(f"使用修复后的文件: {fixed_file}")
                csv_file = fixed_file
                validation_result = self.validate_csv_file(csv_file)
            else:
                logger.error("无法修复CSV文件问题")
                return False
        
        # 2. 尝试FILE导入（最快）
        logger.info("尝试FILE导入...")
        if self._try_file_import(csv_file, table_name, database, validation_result):
            elapsed = time.time() - start_time
            logger.info(f"FILE导入成功！耗时: {elapsed:.1f}秒")
            return True
        
        # 3. 尝试批量导入（备用方案）
        logger.info("FILE导入失败，尝试批量导入...")
        if self._try_batch_import(csv_file, table_name, database, validation_result):
            elapsed = time.time() - start_time
            logger.info(f"批量导入成功！耗时: {elapsed:.1f}秒")
            return True
        
        # 4. 所有方法都失败
        logger.error("所有导入方法都失败")
        return False
    
    def _try_file_import(self, csv_file: str, table_name: str, database: str, 
                         validation_result: Dict[str, Any]) -> bool:
        """尝试FILE导入"""
        try:
            # 准备文件（移除标题行如果存在）
            use_file = csv_file
            is_temp_file = False
            
            if validation_result["has_header"]:
                # 创建无标题行版本
                noheader_file = f"/tmp/{os.path.basename(csv_file)}_noheader.csv"
                with open(csv_file, 'r', encoding=self.config.csv_encoding) as f_in, \
                     open(noheader_file, 'w', encoding=self.config.csv_encoding) as f_out:
                    # 跳过标题行
                    f_in.readline()
                    for line in f_in:
                        f_out.write(line)
                
                use_file = noheader_file
                is_temp_file = True
                logger.info(f"创建无标题行文件: {noheader_file}")
            
            # 复制到Docker容器（如果使用Docker）
            docker_file = None
            if self.config.container_name:
                docker_file = f"/tmp/tdengine_import_{os.getpid()}.csv"
                copy_cmd = f"docker cp {use_file} {self.config.container_name}:{docker_file}"
                result = subprocess.run(copy_cmd, shell=True, capture_output=True, text=True)
                
                if result.returncode != 0:
                    logger.error(f"复制文件到Docker失败: {result.stderr}")
                    if is_temp_file:
                        os.remove(use_file)
                    return False
            
            # 尝试多种导入语法
            file_path = docker_file if docker_file else use_file
            syntaxes = [
                f"INSERT INTO {table_name} FILE '{file_path}';",
                f"file '{file_path}' into {table_name};"
            ]
            
            for i, syntax in enumerate(syntaxes):
                syntax_name = "INSERT FILE" if i == 0 else "file ... into"
                logger.info(f"尝试{syntax_name}语法...")
                
                # 构建完整SQL
                db_part = f"USE {database}; " if database else ""
                full_sql = db_part + syntax
                
                code, out, err = self.execute_sql(full_sql, database)
                
                if code == 0 and ("Insert OK" in out or "Query OK" in out):
                    logger.info(f"{syntax_name}语法导入成功")
                    
                    # 清理临时文件
                    if is_temp_file:
                        os.remove(use_file)
                    
                    return True
                else:
                    error = err or out
                    diagnosis = self.diagnose_error(error)
                    logger.warning(f"{syntax_name}语法失败: {diagnosis}")
            
            # 所有语法都失败
            if is_temp_file:
                os.remove(use_file)
            
            return False
            
        except Exception as e:
            logger.error(f"FILE导入异常: {e}")
            return False
    
    def _try_batch_import(self, csv_file: str, table_name: str, database: str,
                          validation_result: Dict[str, Any]) -> bool:
        """尝试批量导入"""
        try:
            # 读取CSV文件
            with open(csv_file, 'r', encoding=self.config.csv_encoding) as f:
                lines = f.readlines()
            
            if not lines:
                logger.error("CSV文件为空")
                return False
            
            # 确定数据起始行
            data_start = 1 if validation_result["has_header"] else 0
            total_rows = len(lines) - data_start
            
            if total_rows <= 0:
                logger.error("没有数据行")
                return False
            
            logger.info(f"开始批量导入，总行数: {total_rows}")
            
            # 分批导入
            batch_size = self.config.batch_size
            imported = 0
            errors = 0
            start_time = time.time()
            
            for batch_start in range(data_start, len(lines), batch_size):
                batch_end = min(batch_start + batch_size, len(lines))
                batch_lines = lines[batch_start:batch_end]
                
                # 构建批量INSERT语句
                insert_statements = []
                for line in batch_lines:
                    line = line.strip()
                    if not line:
                        continue
                    
                    fields = line.split(self.config.csv_delimiter)
                    
                    # 确保字段数量
                    expected_fields = validation_result.get("expected_fields", 31)
                    if len(fields) < expected_fields:
                        fields.extend(['0'] * (expected_fields - len(fields)))
                    elif len(fields) > expected_fields:
                        fields = fields[:expected_fields]
                    
                    # 商用环境：使用相对时间戳
                    if self.config.commercial_mode and fields:
                        row_num = batch_start - data_start + len(insert_statements)
                        offset_seconds = min((total_rows - row_num) * 10, 
                                           self.config.max_timestamp_range_days * 86400)
                        fields[0] = f"NOW() - {offset_seconds}s"
                    
                    values = ','.join(fields)
                    insert_statements.append(f"INSERT INTO {table_name} VALUES ({values});")
                
                if not insert_statements:
                    continue
                
                # 执行批量INSERT
                try:
                    # 构建批量SQL脚本
                    db_part = f"USE {database}; " if database else ""
                    batch_sql = db_part + "\n".join(insert_statements)
                    
                    code, out, err = self.execute_sql(batch_sql, database)
                    
                    if code == 0:
                        imported += len(insert_statements)
                        batch_num = (batch_start - data_start) // batch_size + 1
                        total_batches = (total_rows + batch_size - 1) // batch_size
                        
                        # 显示进度
                        elapsed = time.time() - start_time
                        rate = imported / elapsed if elapsed > 0 else 0
                        percent = imported / total_rows * 100
                        
                        logger.info(
                            f"批次 {batch_num}/{total_batches}: "
                            f"导入 {len(insert_statements)} 行, "
                            f"累计 {imported}/{total_rows} ({percent:.1f}%), "
                            f"速度: {rate:.1f} 行/秒"
                        )
                    else:
                        errors += len(insert_statements)
                        logger.warning(f"批次导入失败: {err[:200]}")
                        
                except Exception as e:
                    errors += len(insert_statements)
                    logger.error(f"批次导入异常: {e}")
            
            # 导入完成统计
            total_time = time.time() - start_time
            success_rate = (imported / total_rows * 100) if total_rows > 0 else 0
            
            logger.info(f"批量导入完成!")
            logger.info(f"总耗时: {total_time:.1f}秒")
            logger.info(f"导入行数: {imported}/{total_rows}")
            logger.info(f"错误数: {errors}")
            logger.info(f"成功率: {success_rate:.1f}%")
            logger.info(f"平均速度: {imported/total_time:.1f} 行/秒")
            
            return errors == 0
            
        except Exception as e:
            logger.error(f"批量导入异常: {e}")
            return False
    
    def export_to_csv(self, table_name: str, output_file: str, database: str = "",
                     start_time: str = "", end_time: str = "", limit: int = 0) -> bool:
        """导出数据到CSV文件"""
        logger.info(f"开始导出数据: {table_name} -> {output_file}")
        
        try:
            # 构建查询语句
            where_clauses = []
            if start_time:
                where_clauses.append(f"createdat >= '{start_time}'")
            if end_time:
                where_clauses.append(f"createdat <= '{end_time}'")
            
            where_clause = " WHERE " + " AND ".join(where_clauses) if where_clauses else ""
            limit_clause = f" LIMIT {limit}" if limit > 0 else ""
            
            query = f"SELECT * FROM {table_name}{where_clause}{limit_clause}"
            
            # 执行查询
            db_part = f"USE {database}; " if database else ""
            full_sql = db_part + query
            
            code, out, err = self.execute_sql(full_sql, database)
            
            if code != 0:
                logger.error(f"查询数据失败: {err}")
                return False
            
            # 解析查询结果
            lines = out.strip().split('\n')
            if len(lines) < 3:  # 至少包含表头和分隔线
                logger.error(f"查询结果格式错误: {out[:200]}")
                return False
            
            # 提取表头和数据
            header_line = None
            data_lines = []
            in_data_section = False
            
            for line in lines:
                line = line.strip()
                if not line:
                    continue
                
                if line.startswith('|'):  # 表头或数据行
                    if '---' in line:  # 分隔线
                        in_data_section = True
                        continue
                    
                    if in_data_section:
                        # 数据行，移除前后的|和空格
                        data = line.strip('|').split('|')
                        data = [cell.strip() for cell in data]
                        data_lines.append(data)
                    else:
                        # 表头行
                        header = line.strip('|').split('|')
                        header = [cell.strip() for cell in header]
                        header_line = header
            
            if not header_line or not data_lines:
                logger.error("无法解析查询结果")
                return False
            
            # 写入CSV文件
            with open(output_file, 'w', encoding=self.config.csv_encoding, newline='') as f:
                writer = csv.writer(f, delimiter=self.config.csv_delimiter,
                                  quotechar=self.config.csv_quote_char, quoting=csv.QUOTE_MINIMAL)
                
                # 写入标题行
                writer.writerow(header_line)
                
                # 写入数据行
                for row in data_lines:
                    writer.writerow(row)
            
            logger.info(f"导出成功: {len(data_lines)} 行 -> {output_file}")
            return True
            
        except Exception as e:
            logger.error(f"导出数据异常: {e}")
            return False
    
    def get_table_info(self, table_name: str, database: str = "") -> Optional[Dict[str, Any]]:
        """获取表信息"""
        try:
            sql = f"DESCRIBE {table_name}"
            code, out, err = self.execute_sql(sql, database)
            
            if code != 0:
                logger.error(f"获取表信息失败: {err}")
                return None
            
            # 解析DESCRIBE结果
            table_info = {
                "table_name": table_name,
                "columns": [],
                "total_rows": 0,
                "create_time": ""
            }
            
            lines = out.strip().split('\n')
            in_data_section = False
            
            for line in lines:
                line = line.strip()
                if not line:
                    continue
                
                if line.startswith('|') and '---' not in line:
                    if not in_data_section:
                        in_data_section = True
                        continue
                    
                    # 解析列信息
                    parts = line.strip('|').split('|')
                    if len(parts) >= 3:
                        column_info = {
                            "name": parts[0].strip(),
                            "type": parts[1].strip(),
                            "length": parts[2].strip() if len(parts) > 2 else "",
                            "note": parts[3].strip() if len(parts) > 3 else ""
                        }
                        table_info["columns"].append(column_info)
            
            # 获取行数
            count_sql = f"SELECT COUNT(*) FROM {table_name}"
            code, count_out, count_err = self.execute_sql(count_sql, database)
            
            if code == 0:
                # 解析计数结果
                for line in count_out.split('\n'):
                    if '|' in line and 'count' not in line.lower():
                        parts = line.strip('|').split('|')
                        if len(parts) >= 1:
                            try:
                                table_info["total_rows"] = int(parts[0].strip())
                            except ValueError:
                                pass
            
            logger.info(f"表信息: {table_info}")
            return table_info
            
        except Exception as e:
            logger.error(f"获取表信息异常: {e}")
            return None

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='TDengine通用工具包')
    subparsers = parser.add_subparsers(dest='command', help='命令')
    
    # 导入命令
    import_parser = subparsers.add_parser('import', help='导入CSV文件')
    import_parser.add_argument('csv_file', help='CSV文件路径')
    import_parser.add_argument('table_name', help='目标表名')
    import_parser.add_argument('--database', default='', help='数据库名')
    import_parser.add_argument('--batch-size', type=int, default=1000, help='批处理大小')
    import_parser.add_argument('--commercial', action='store_true', help='商用环境模式')
    
    # 导出命令
    export_parser = subparsers.add_parser('export', help='导出数据到CSV')
    export_parser.add_argument('table_name', help='源表名')
    export_parser.add_argument('output_file', help='输出CSV文件路径')
    export_parser.add_argument('--database', default='', help='数据库名')
    export_parser.add_argument('--start-time', default='', help='开始时间')
    export_parser.add_argument('--end-time', default='', help='结束时间')
    export_parser.add_argument('--limit', type=int, default=0, help='限制行数')
    
    # 验证命令
    validate_parser = subparsers.add_parser('validate', help='验证CSV文件')
    validate_parser.add_argument('csv_file', help='CSV文件路径')
    
    # 表信息命令
    info_parser = subparsers.add_parser('info', help='获取表信息')
    info_parser.add_argument('table_name', help='表名')
    info_parser.add_argument('--database', default='', help='数据库名')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    # 创建配置
    config = TDengineConfig()
    if hasattr(args, 'batch_size'):
        config.batch_size = args.batch_size
    if hasattr(args, 'commercial'):
        config.commercial_mode = args.commercial
    
    # 创建工具包实例
    toolkit = TDengineToolkit(config)
    
    # 执行命令
    if args.command == 'import':
        success = toolkit.import_csv(
            args.csv_file,
            args.table_name,
            args.database if hasattr(args, 'database') else ""
        )
        sys.exit(0 if success else 1)
    
    elif args.command == 'export':
        success = toolkit.export_to_csv(
            args.table_name,
            args.output_file,
            args.database if hasattr(args, 'database') else "",
            args.start_time if hasattr(args, 'start_time') else "",
            args.end_time if hasattr(args, 'end_time') else "",
            args.limit if hasattr(args, 'limit') else 0
        )
        sys.exit(0 if success else 1)
    
    elif args.command == 'validate':
        result = toolkit.validate_csv_file(args.csv_file)
        print(json.dumps(result, indent=2, ensure_ascii=False))
        sys.exit(0 if not result['issues'] else 1)
    
    elif args.command == 'info':
        info = toolkit.get_table_info(
            args.table_name,
            args.database if hasattr(args, 'database') else ""
        )
        if info:
            print(json.dumps(info, indent=2, ensure_ascii=False))
            sys.exit(0)
        else:
            sys.exit(1)

if __name__ == "__main__":
    main()
