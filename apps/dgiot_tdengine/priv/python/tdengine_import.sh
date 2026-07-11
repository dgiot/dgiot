#!/bin/bash
# TDengine数据导入脚本 - 基于成功经验版本
# 关键发现：TDengine FILE导入不能有标题行
# 基于测试经验：字段顺序必须与表结构完全匹配（31个字段）

set -e

VERSION="4.0"
SCRIPT_NAME=$(basename "$0")

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_color() {
    echo -e "${2}${1}${NC}"
}

print_header() {
    echo "================================================"
    echo "  TDengine数据导入脚本 v${VERSION}（基于成功经验）"
    echo "================================================"
}

print_usage() {
    echo "用法: ${SCRIPT_NAME} [CSV文件] [表名] [选项]"
    echo ""
    echo "必需参数:"
    echo "  CSV文件             要导入的CSV文件路径"
    echo "  表名                目标表名"
    echo ""
    echo "选项:"
    echo "  --batch-size N            批处理大小（默认：1000，仅Python模式使用）"
    echo "  --mode [auto|file|python] 导入模式："
    echo "                             auto    - 自动选择（先尝试FILE导入，失败用Python导入）"
    echo "                             file    - 仅使用TDengine FILE导入"
    echo "                             python  - 仅使用Python批量导入"
    echo "  --help                    显示此帮助信息"
    echo ""
    echo "基于测试经验的关键发现:"
    echo "  1. TDengine FILE导入不能有标题行"
    echo "  2. 字段顺序必须与表结构完全匹配（31个字段）"
    echo "  3. 商用环境时间戳限制：一年内（31536000秒）"
    echo "  4. 使用相对时间戳：NOW() - offset_seconds"
    echo ""
    echo "示例:"
    echo "  ${SCRIPT_NAME} _24b9b4bc50._556dc74e20.csv _556dc74e20"
    echo "  ${SCRIPT_NAME} _24b9b4bc50._556dc74e20.csv _556dc74e20 --batch-size 5000"
    echo "  ${SCRIPT_NAME} _24b9b4bc50._556dc74e20.csv _556dc74e20 --mode python"
}

# 检查必需工具
check_dependencies() {
    local missing_tools=()
    
    for tool in awk python3 docker; do
        if ! command -v $tool &> /dev/null; then
            missing_tools+=("$tool")
        fi
    done
    
    if [ ${#missing_tools[@]} -gt 0 ]; then
        print_color "错误: 缺少必需的工具: ${missing_tools[*]}" "$RED"
        exit 1
    fi
    
    # 检查Docker容器
    if ! docker ps | grep -q tdengine-tsdb; then
        print_color "警告: TDengine容器可能未运行" "$YELLOW"
        print_color "请确保TDengine容器正在运行: docker ps | grep tdengine" "$YELLOW"
    fi
}

# 尝试使用TDengine FILE导入（基于成功经验）
try_file_import() {
    local csv_file="$1"
    local table_name="$2"
    local db_name="$3"
    
    print_color "尝试使用TDengine FILE导入..." "$BLUE"
    
    # 关键发现：检查文件是否有标题行
    local first_line=$(head -1 "$csv_file" 2>/dev/null || echo "")
    local use_file="$csv_file"
    local temp_file=""
    
    # 如果第一行看起来像标题（包含createdat等字段名），需要创建无标题行版本
    if echo "$first_line" | grep -iq "createdat\|v410_0"; then
        print_color "检测到标题行，创建无标题行版本..." "$YELLOW"
        temp_file="/tmp/$(basename "$csv_file")_noheader.csv"
        
        # 创建无标题行文件（跳过第一行）
        tail -n +2 "$csv_file" > "$temp_file"
        use_file="$temp_file"
        
        # 检查创建的文件
        local noheader_lines=$(wc -l < "$temp_file" 2>/dev/null || echo 0)
        print_color "创建无标题行文件: $temp_file (${noheader_lines}行)" "$GREEN"
    fi
    
    # 复制文件到Docker容器
    local docker_file="/tmp/tdengine_import_$$.csv"
    if ! docker cp "$use_file" tdengine-tsdb:"$docker_file" 2>/dev/null; then
        print_color "复制文件到Docker失败" "$RED"
        [ -n "$temp_file" ] && rm -f "$temp_file"
        return 1
    fi
    
    # 尝试两种语法：INSERT FILE 和 file ... into
    local syntaxes=(
        "use $db_name; INSERT INTO $table_name FILE '$docker_file';"
        "use $db_name; file '$docker_file' into $table_name;"
    )
    
    for i in "${!syntaxes[@]}"; do
        local import_cmd="${syntaxes[$i]}"
        local syntax_name="INSERT FILE"
        [ $i -eq 1 ] && syntax_name="file ... into"
        
        print_color "尝试${syntax_name}语法..." "$BLUE"
        
        # 执行导入命令
        local output
        output=$(docker exec tdengine-tsdb taos -s "$import_cmd" 2>&1)
        
        if echo "$output" | grep -q "Insert OK\|Query OK"; then
            # 提取导入行数和时间
            local rows_affected=$(echo "$output" | grep -o "Insert OK, [0-9,]* row" | grep -o "[0-9,]*" | head -1)
            local time_taken=$(echo "$output" | grep -o "([0-9.]*s)" | grep -o "[0-9.]*" | head -1)
            
            if [ -n "$rows_affected" ]; then
                print_color "${syntax_name}语法导入成功！导入 ${rows_affected} 行，耗时 ${time_taken:-N/A} 秒" "$GREEN"
            else
                print_color "${syntax_name}语法导入成功！" "$GREEN"
            fi
            
            # 清理临时文件
            [ -n "$temp_file" ] && rm -f "$temp_file"
            return 0
        else
            # 检查常见错误类型
            local error_output="$output"
            if echo "$error_output" | grep -q "Timestamp data out of range"; then
                print_color "${syntax_name}语法失败：时间戳超出范围（可能超过一年限制）" "$YELLOW"
            elif echo "$error_output" | grep -q "syntax error.*createdat\|syntax error.*file\|invalid data or symbol"; then
                print_color "${syntax_name}语法失败：语法错误或字段数量不匹配" "$YELLOW"
            elif echo "$error_output" | grep -q "invalid timestamp"; then
                print_color "${syntax_name}语法失败：时间戳格式错误" "$YELLOW"
            else
                if [ $i -eq $(( ${#syntaxes[@]} - 1 )) ]; then
                    print_color "所有FILE导入语法都失败" "$YELLOW"
                else
                    print_color "${syntax_name}语法失败，尝试下一种语法..." "$YELLOW"
                    continue
                fi
            fi
            
            # 显示错误详情
            local error_detail=$(echo "$error_output" | grep -A2 "DB error\|error:" | head -3 | tr '\n' ' ')
            if [ -n "$error_detail" ]; then
                print_color "错误详情: ${error_detail:0:200}" "$YELLOW"
            fi
        fi
    done
    
    # 所有语法都失败
    # 清理临时文件
    [ -n "$temp_file" ] && rm -f "$temp_file"
    return 1
}

# 使用Python导入
use_python_import() {
    local csv_file="$1"
    local table_name="$2"
    local db_name="$3"
    local batch_size="$4"
    
    print_color "使用Python批量导入..." "$BLUE"
    
    # 检查Python脚本是否存在
    if [ ! -f "tdengine_importer.py" ]; then
        print_color "错误: 找不到Python导入脚本 tdengine_importer.py" "$RED"
        return 1
    fi
    
    # 执行Python导入
    if python3 tdengine_importer.py "$csv_file" "$table_name" "$db_name" --batch-size "$batch_size"; then
        print_color "Python导入成功！" "$GREEN"
        return 0
    else
        print_color "Python导入失败" "$RED"
        return 1
    fi
}

# 主函数
main() {
    # 显示头部
    print_header
    
    # 检查参数
    if [ $# -lt 2 ] || [[ "$1" == "--help" ]]; then
        print_usage
        exit 0
    fi
    
    # 解析参数
    CSV_FILE="$1"
    TABLE_NAME="$2"
    DB_NAME="_24b9b4bc50"
    BATCH_SIZE=1000
    MODE="auto"
    
    shift 2
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --batch-size)
                BATCH_SIZE="$2"
                shift 2
                ;;
            --mode)
                MODE="$2"
                if [[ ! "$MODE" =~ ^(auto|file|python)$ ]]; then
                    print_color "错误: 无效的模式 '$MODE'，必须是 auto, file 或 python" "$RED"
                    exit 1
                fi
                shift 2
                ;;
            *)
                print_color "错误: 未知参数 '$1'" "$RED"
                print_usage
                exit 1
                ;;
        esac
    done
    
    # 显示配置
    echo "配置:"
    echo "  CSV文件:      $CSV_FILE"
    echo "  表名:         $TABLE_NAME"
    echo "  数据库:       $DB_NAME"
    echo "  批处理大小:   $BATCH_SIZE"
    echo "  导入模式:     $MODE"
    echo ""
    
    # 检查依赖
    check_dependencies
    
    # 检查文件
    if [ ! -f "$CSV_FILE" ]; then
        print_color "错误: CSV文件不存在: $CSV_FILE" "$RED"
        exit 1
    fi
    
    # 1. 准备导入文件（基于测试经验）
    print_color "1. 准备导入文件..." "$BLUE"
    
    # 检查文件基本信息
    ORIGINAL_ROWS=$(wc -l < "$CSV_FILE" 2>/dev/null || echo 0)
    ORIGINAL_SIZE=$(ls -lh "$CSV_FILE" 2>/dev/null | awk '{print $5}' || echo "N/A")
    
    print_color "  原始文件: $CSV_FILE (${ORIGINAL_ROWS}行, ${ORIGINAL_SIZE})" "$GREEN"
    
    # 检查标题行
    local first_line=$(head -1 "$CSV_FILE" 2>/dev/null || echo "")
    if echo "$first_line" | grep -iq "createdat\|v410_0"; then
        print_color "  检测到标题行，FILE导入时会自动跳过" "$YELLOW"
        DATA_ROWS=$((ORIGINAL_ROWS - 1))
        print_color "  有效数据行数: ${DATA_ROWS}" "$GREEN"
    else
        print_color "  未检测到标题行，直接使用原始文件" "$GREEN"
        DATA_ROWS=$ORIGINAL_ROWS
    fi
    
    # 检查字段数量
    if [ -n "$first_line" ]; then
        ORIGINAL_FIELDS=$(echo "$first_line" | tr ',' '\n' | wc -l)
        print_color "  字段数量: ${ORIGINAL_FIELDS}" "$BLUE"
        
        # 根据表结构，应该有31个字段
        if [ "$ORIGINAL_FIELDS" -eq 31 ]; then
            print_color "  字段数量正确（31个），与表结构匹配" "$GREEN"
        else
            print_color "  警告: 字段数量为${ORIGINAL_FIELDS}，表结构需要31个字段" "$YELLOW"
            print_color "  Python导入模式会自动修复字段数量" "$YELLOW"
        fi
    fi
    
    # 显示数据示例
    if [ "$DATA_ROWS" -gt 0 ]; then
        print_color "  数据示例（前2行）:" "$BLUE"
        if echo "$first_line" | grep -iq "createdat\|v410_0"; then
            # 有标题行，显示第2-3行
            head -3 "$CSV_FILE" | tail -2 | awk -F',' '
            {
                printf "    行%d: createdat=%s, vd1020=%s, vd1024=%s\n", 
                       NR, $1, $9, $10
            }' 2>/dev/null || true
        else
            # 无标题行，显示第1-2行
            head -2 "$CSV_FILE" | awk -F',' '
            {
                printf "    行%d: createdat=%s, vd1020=%s, vd1024=%s\n", 
                       NR, $1, $9, $10
            }' 2>/dev/null || true
        fi
    fi
    
    echo ""
    
    # 2. 数据导入
    print_color "2. 数据导入..." "$BLUE"
    
    IMPORT_START_TIME=$(date +%s)
    IMPORT_SUCCESS=false
    
    # 根据模式选择导入方法
    case "$MODE" in
        "file")
            # FILE模式：直接尝试FILE导入
            print_color "  使用FILE导入模式..." "$BLUE"
            if try_file_import "$CSV_FILE" "$TABLE_NAME" "$DB_NAME"; then
                IMPORT_SUCCESS=true
            else
                print_color "FILE导入失败" "$RED"
            fi
            ;;
        "python")
            # Python模式：使用Python批量导入
            print_color "  使用Python导入模式..." "$BLUE"
            if use_python_import "$CSV_FILE" "$TABLE_NAME" "$DB_NAME" "$BATCH_SIZE"; then
                IMPORT_SUCCESS=true
            else
                print_color "Python导入失败" "$RED"
            fi
            ;;
        "auto")
            # 自动模式：先尝试FILE导入，失败则使用Python
            print_color "  自动模式：先尝试FILE导入..." "$BLUE"
            if try_file_import "$CSV_FILE" "$TABLE_NAME" "$DB_NAME"; then
                IMPORT_SUCCESS=true
            else
                print_color "  FILE导入失败，尝试Python导入..." "$YELLOW"
                if use_python_import "$CSV_FILE" "$TABLE_NAME" "$DB_NAME" "$BATCH_SIZE"; then
                    IMPORT_SUCCESS=true
                else
                    print_color "  Python导入失败" "$RED"
                fi
            fi
            ;;
    esac
    
    IMPORT_END_TIME=$(date +%s)
    IMPORT_TIME=$((IMPORT_END_TIME - IMPORT_START_TIME))
    
    IMPORT_END_TIME=$(date +%s)
    IMPORT_TIME=$((IMPORT_END_TIME - IMPORT_START_TIME))
    
    # 3. 验证结果
    if [ "$IMPORT_SUCCESS" = true ]; then
        print_color "3. 验证导入结果..." "$BLUE"
        
        # 检查导入的行数
        VERIFY_CMD="use $DB_NAME; select count(*) as 总行数 from $TABLE_NAME;"
        print_color "查询导入结果..." "$BLUE"
        docker exec tdengine-tsdb taos -s "$VERIFY_CMD" 2>/dev/null | grep -v "Welcome\|taos>\|Database changed\|Copyright\|TDengine"
        
        print_color "导入成功！" "$GREEN"
        print_color "总耗时: ${IMPORT_TIME} 秒" "$GREEN"
    else
        print_color "导入失败！" "$RED"
        exit 1
    fi
    
    # 4. 提供查询命令
    print_color "4. 查询命令..." "$BLUE"
    
    echo ""
    print_color "================================================" "$BLUE"
    print_color "  完成!" "$GREEN"
    print_color "================================================" "$BLUE"
    echo ""
    print_color "基于测试经验的关键提示:" "$YELLOW"
    echo "  1. TDengine FILE导入不能有标题行（脚本已自动处理）"
    echo "  2. 商用环境时间戳限制：一年内（31536000秒）"
    echo "  3. Python模式使用相对时间戳：NOW() - offset_seconds"
    echo "  4. 字段数量必须为31个（与表结构匹配）"
    echo ""
    print_color "查询命令:" "$BLUE"
    echo "  docker exec tdengine-tsdb taos -s \"use $DB_NAME; select count(*) from $TABLE_NAME;\""
    echo "  docker exec tdengine-tsdb taos -s \"use $DB_NAME; select * from $TABLE_NAME limit 3;\""
    echo "  docker exec tdengine-tsdb taos -s \"use $DB_NAME; select min(createdat), max(createdat) from $TABLE_NAME;\""
}

# 执行主函数
main "$@"
