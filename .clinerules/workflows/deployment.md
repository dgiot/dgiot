# 部署工作流

## 概述

本工作流定义了DG-IoT平台插件的部署流程，确保部署过程标准化、可重复且可靠。

## 1. 部署前准备

### 1.1 环境检查清单
```bash
# 1. 检查目标服务器环境
ssh user@target-server "uname -a && df -h && free -h"

# 2. 检查依赖服务状态
ssh user@target-server "systemctl status postgresql"
ssh user@target-server "systemctl status tdengine"
ssh user@target-server "systemctl status redis"

# 3. 检查网络连通性
ssh user@target-server "ping -c 3 8.8.8.8"
ssh user@target-server "nc -zv localhost 5432"  # PostgreSQL
ssh user@target-server "nc -zv localhost 6030"  # TDengine
```

### 1.2 部署包准备
```bash
# 1. 编译发布包
make rel

# 2. 检查发布包内容
ls -la _build/prod/rel/emqx/
tar -tzf _build/prod/rel/emqx/emqx-*.tar.gz | head -20

# 3. 生成部署清单
./scripts/pkg-full-vsn.sh > deployment_manifest.txt
```

## 2. 数据库部署工作流

### 2.1 PostgreSQL部署
```bash
#!/bin/bash
# deploy/postgres_deploy.sh

echo "开始部署PostgreSQL数据库..."
echo "========================================"

# 1. 检查PostgreSQL是否已安装
if ! command -v psql &> /dev/null; then
    echo "PostgreSQL未安装，开始安装..."
    sudo apt-get update
    sudo apt-get install -y postgresql postgresql-contrib
fi

# 2. 启动PostgreSQL服务
sudo systemctl start postgresql
sudo systemctl enable postgresql

# 3. 创建数据库和用户
sudo -u postgres psql <<EOF
CREATE USER dgiot WITH PASSWORD 'CHANGEME456';
CREATE DATABASE dgiot OWNER dgiot;
GRANT ALL PRIVILEGES ON DATABASE dgiot TO dgiot;
\c dgiot
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
EOF

# 4. 导入初始数据
if [ -f "deploy/postgres_init.sql" ]; then
    sudo -u postgres psql -d dgiot -f deploy/postgres_init.sql
fi

echo "========================================"
echo "PostgreSQL数据库部署完成！"
```

### 2.2 TDengine部署
```bash
#!/bin/bash
# deploy/tdengine_deploy.sh

echo "开始部署TDengine时序数据库..."
echo "========================================"

# 1. 检查TDengine是否已安装
if ! command -v taos &> /dev/null; then
    echo "TDengine未安装，开始安装..."
    
    # 下载TDengine安装包
    wget https://www.taosdata.com/assets-download/TDengine-server-3.0.1.6-Linux-x64.tar.gz
    tar -xzf TDengine-server-3.0.1.6-Linux-x64.tar.gz
    cd TDengine-server-3.0.1.6
    
    # 安装TDengine
    sudo ./install.sh
    
    # 清理安装包
    cd ..
    rm -rf TDengine-server-3.0.1.6*
fi

# 2. 启动TDengine服务
sudo systemctl start taosd
sudo systemctl enable taosd

# 3. 创建DG-IoT数据库
taos -s "CREATE DATABASE IF NOT EXISTS dgiot KEEP 365 DAYS 10 BLOCKS 6;"

# 4. 创建超级表
taos -s "
CREATE STABLE IF NOT EXISTS dgiot.devices (
    ts TIMESTAMP,
    value DOUBLE,
    status INT
) TAGS (
    device_id BINARY(64),
    device_type BINARY(32),
    location BINARY(128)
);"

echo "========================================"
echo "TDengine时序数据库部署完成！"
```

## 3. DG-IoT平台部署工作流

### 3.1 单节点部署
```bash
#!/bin/bash
# deploy/dgiot_single_node.sh

echo "开始部署DG-IoT单节点..."
echo "========================================"

# 1. 停止现有服务
sudo systemctl stop emqx 2>/dev/null || true

# 2. 备份现有配置
if [ -d "/opt/emqx" ]; then
    sudo cp -r /opt/emqx /opt/emqx_backup_$(date +%Y%m%d_%H%M%S)
fi

# 3. 解压发布包
sudo tar -xzf _build/prod/rel/emqx/emqx-*.tar.gz -C /opt/
sudo mv /opt/emqx-* /opt/emqx

# 4. 复制配置文件
sudo cp deploy/config/emqx.conf /opt/emqx/etc/
sudo cp deploy/config/acl.conf /opt/emqx/etc/

# 5. 配置环境变量
sudo cp deploy/config/dgiot.env /opt/emqx/etc/

# 6. 设置权限
sudo chown -R emqx:emqx /opt/emqx
sudo chmod -R 755 /opt/emqx

# 7. 创建系统服务
sudo cp deploy/systemd/emqx.service /etc/systemd/system/
sudo systemctl daemon-reload

# 8. 启动服务
sudo systemctl start emqx
sudo systemctl enable emqx

# 9. 验证部署
sleep 5
sudo systemctl status emqx
curl -f http://localhost:18083/api/v4/nodes

echo "========================================"
echo "DG-IoT单节点部署完成！"
```

### 3.2 集群部署
```bash
#!/bin/bash
# deploy/dgiot_cluster.sh

echo "开始部署DG-IoT集群..."
echo "========================================"

# 定义集群节点
NODES=("node1:192.168.1.101" "node2:192.168.1.102" "node3:192.168.1.103")
CLUSTER_NAME="dgiot_cluster"

# 1. 在每个节点上部署单节点
for NODE in "${NODES[@]}"; do
    NODE_NAME=${NODE%:*}
    NODE_IP=${NODE#*:}
    
    echo "部署节点: $NODE_NAME ($NODE_IP)"
    
    # 复制部署包到节点
    scp _build/prod/rel/emqx/emqx-*.tar.gz user@$NODE_IP:/tmp/
    
    # 在远程节点执行部署
    ssh user@$NODE_IP "bash -s" <<EOF
        # 停止现有服务
        sudo systemctl stop emqx 2>/dev/null || true
        
        # 解压发布包
        sudo tar -xzf /tmp/emqx-*.tar.gz -C /opt/
        sudo mv /opt/emqx-* /opt/emqx
        
        # 配置集群
        sudo sed -i "s/name = emqx@127.0.0.1/name = emqx@$NODE_IP/g" /opt/emqx/etc/emqx.conf
        sudo sed -i "s/cluster.name = emqx.cluster/cluster.name = $CLUSTER_NAME/g" /opt/emqx/etc/emqx.conf
        sudo sed -i "s/cluster.discovery = manual/cluster.discovery = static/g" /opt/emqx/etc/emqx.conf
        
        # 添加集群节点
        CLUSTER_NODES=""
        for CLUSTER_NODE in "${NODES[@]}"; do
            CLUSTER_NODE_IP=\${CLUSTER_NODE#*:}
            CLUSTER_NODES="\$CLUSTER_NODES emqx@\$CLUSTER_NODE_IP,"
        done
        sudo sed -i "s/cluster.static.seeds =/cluster.static.seeds = \${CLUSTER_NODES%,}/g" /opt/emqx/etc/emqx.conf
        
        # 启动服务
        sudo systemctl start emqx
        sudo systemctl enable emqx
EOF
done

# 2. 验证集群状态
echo "验证集群状态..."
for NODE in "${NODES[@]}"; do
    NODE_IP=${NODE#*:}
    echo "节点 $NODE_IP 状态:"
    curl -s http://$NODE_IP:18083/api/v4/nodes | jq '.'
done

echo "========================================"
echo "DG-IoT集群部署完成！"
```

## 4. 插件部署工作流

### 4.1 插件热部署
```bash
#!/bin/bash
# deploy/plugin_hot_deploy.sh

PLUGIN_NAME=$1
PLUGIN_VERSION=$2

echo "开始热部署插件: $PLUGIN_NAME ($PLUGIN_VERSION)"
echo "========================================"

# 1. 编译插件
echo "1. 编译插件..."
_build/emqx/rel/emqx/bin/emqx eval "dgiot_plugin:compile($PLUGIN_NAME)."

# 2. 检查插件依赖
echo "2. 检查插件依赖..."
_build/emqx/rel/emqx/bin/emqx eval "application:which_applications()." | grep -i $PLUGIN_NAME

# 3. 热加载插件
echo "3. 热加载插件..."
_build/emqx/rel/emqx/bin/emqx eval "dgiot_plugin:reload_plugin($PLUGIN_NAME)."

# 4. 验证插件状态
echo "4. 验证插件状态..."
_build/emqx/rel/emqx/bin/emqx eval "dgiot_plugin:status($PLUGIN_NAME)."

# 5. 运行插件测试
echo "5. 运行插件测试..."
_build/emqx/rel/emqx/bin/emqx eval "$PLUGIN_NAME:test()."

echo "========================================"
echo "插件热部署完成: $PLUGIN_NAME"
```

### 4.2 插件批量部署
```bash
#!/bin/bash
# deploy/plugins_batch_deploy.sh

echo "开始批量部署插件..."
echo "========================================"

# 插件列表
PLUGINS=(
    "dgiot_drone"
    "dgiot_modbus"
    "dgiot_tdengine"
    "dgiot_bacnet"
    "dgiot_opc"
)

# 部署每个插件
for PLUGIN in "${PLUGINS[@]}"; do
    echo "部署插件: $PLUGIN"
    
    # 检查插件是否存在
    if [ -d "apps/$PLUGIN" ]; then
        # 热部署插件
        ./deploy/plugin_hot_deploy.sh $PLUGIN
        
        # 验证插件功能
        _build/emqx/rel/emqx/bin/emqx eval "$PLUGIN:verify()."
    else
        echo "警告: 插件 $PLUGIN 不存在，跳过部署"
    fi
done

echo "========================================"
echo "插件批量部署完成！"
```

## 5. 配置管理工作流

### 5.1 配置文件管理
```bash
#!/bin/bash
# deploy/config_management.sh

echo "开始管理配置文件..."
echo "========================================"

# 1. 备份当前配置
CONFIG_BACKUP_DIR="/opt/emqx/config_backup_$(date +%Y%m%d_%H%M%S)"
sudo mkdir -p $CONFIG_BACKUP_DIR
sudo cp -r /opt/emqx/etc/* $CONFIG_BACKUP_DIR/

# 2. 应用新配置
if [ -d "deploy/config" ]; then
    echo "应用新配置文件..."
    sudo cp deploy/config/* /opt/emqx/etc/
    
    # 设置环境变量
    if [ -f "deploy/config/dgiot.env" ]; then
        while IFS='=' read -r key value; do
            if [[ ! $key =~ ^# ]] && [[ -n $key ]]; then
                sudo sed -i "s/^$key=.*/$key=$value/" /opt/emqx/etc/dgiot.env
            fi
        done < deploy/config/dgiot.env
    fi
fi

# 3. 验证配置语法
echo "验证配置语法..."
/opt/emqx/bin/emqx check_conf

# 4. 重载配置
echo "重载配置..."
sudo systemctl reload emqx

# 5. 验证配置生效
echo "验证配置生效..."
curl -f http://localhost:18083/api/v4/configs

echo "========================================"
echo "配置文件管理完成！"
```

### 5.2 密钥管理
```bash
#!/bin/bash
# deploy/secret_management.sh

echo "开始管理密钥..."
echo "========================================"

# 1. 生成随机密钥
generate_secret() {
    openssl rand -base64 32 | tr -d '\n'
}

# 2. 更新Parse Server密钥
PARSE_MASTER_KEY=$(generate_secret)
PARSE_READONLY_MASTER_KEY=$(generate_secret)

echo "更新Parse Server密钥..."
sudo sed -i "s/parseServerMasterKey=.*/parseServerMasterKey=$PARSE_MASTER_KEY/" /opt/emqx/etc/dgiot.env
sudo sed -i "s/parseServerReadOnlyMasterKey=.*/parseServerReadOnlyMasterKey=$PARSE_READONLY_MASTER_KEY/" /opt/emqx/etc/dgiot.env

# 3. 更新数据库密码
DB_PASSWORD=$(generate_secret)
sudo sed -i "s/postgresPassword=.*/postgresPassword=$DB_PASSWORD/" /opt/emqx/etc/dgiot.env

# 4. 更新API密钥
API_KEY=$(generate_secret)
sudo sed -i "s/apiKey=.*/apiKey=$API_KEY/" /opt/emqx/etc/dgiot.env

# 5. 保护密钥文件
sudo chmod 600 /opt/emqx/etc/dgiot.env
sudo chown emqx:emqx /opt/emqx/etc/dgiot.env

echo "========================================"
echo "密钥管理完成！"
echo "重要：请妥善保存生成的密钥！"
```

## 6. 监控和日志工作流

### 6.1 监控配置
```bash
#!/bin/bash
# deploy/monitoring_setup.sh

echo "设置监控系统..."
echo "========================================"

# 1. 安装Prometheus
if ! command -v prometheus &> /dev/null; then
    echo "安装Prometheus..."
    wget https://github.com/prometheus/prometheus/releases/download/v2.45.0/prometheus-2.45.0.linux-amd64.tar.gz
    tar -xzf prometheus-2.45.0.linux-amd64.tar.gz
    sudo mv prometheus-2.45.0.linux-amd64 /opt/prometheus
    sudo cp deploy/prometheus/prometheus.yml /opt/prometheus/
    
    # 创建系统服务
    sudo cp deploy/systemd/prometheus.service /etc/systemd/system/
    sudo systemctl daemon-reload
    sudo systemctl start prometheus
    sudo systemctl enable prometheus
fi

# 2. 安装Grafana
if ! command -v grafana-server &> /dev/null; then
    echo "安装Grafana..."
    sudo apt-get install -y adduser libfontconfig1
    wget https://dl.grafana.com/oss/release/grafana-10.0.3.linux-amd64.tar.gz
    tar -xzf grafana-10.0.3.linux-amd64.tar.gz
    sudo mv grafana-10.0.3 /opt/grafana
    
    # 创建系统服务
    sudo cp deploy/systemd/grafana.service /etc/systemd/system/
    sudo systemctl daemon-reload
    sudo systemctl start grafana-server
    sudo systemctl enable grafana-server
fi

# 3. 配置DG-IoT监控
echo "配置DG-IoT监控..."
sudo cp deploy/grafana/dgiot_dashboard.json /opt/grafana/public/dashboards/
sudo cp deploy/prometheus/dgiot_rules.yml /opt/prometheus/

# 4. 重启监控服务
sudo systemctl restart prometheus
sudo systemctl restart grafana-server

echo "========================================"
echo "监控系统设置完成！"
echo "访问地址:"
echo "Prometheus: http://localhost:9090"
echo "Grafana: http://localhost:3000 (admin/admin)"
```

### 6.2 日志管理
```bash
#!/bin/bash
# deploy/log_management.sh

echo "设置日志管理..."
echo "========================================"

# 1. 配置日志轮转
sudo cp deploy/logrotate/emqx /etc/logrotate.d/
sudo chmod 644 /etc/logrotate.d/emqx

# 2. 设置日志级别
sudo sed -i "s/log.level = info/log.level = debug/" /opt/emqx/etc/emqx.conf

# 3. 启用访问日志
sudo sed -i "s/log.to = file/log.to = both/" /opt/emqx/etc/emqx.conf

# 4. 创建日志目录
sudo mkdir -p /var/log/emqx/archive
sudo chown -R emqx:emqx /var/log/emqx

# 5. 配置日志收集（可选）
if [ -f "deploy/fluentd/fluent.conf" ]; then
    echo "配置Fluentd日志收集..."
    sudo cp deploy/fluentd/fluent.conf /etc/fluent/
    sudo systemctl restart fluentd
fi

echo "========================================"
echo "日志管理设置完成！"
```

## 7. 验证和测试工作流

### 7.1 部署验证
```bash
#!/bin/bash
# deploy/validation_test.sh

echo "开始部署验证测试..."
echo "========================================"

# 1. 验证服务状态
echo "1. 验证服务状态..."
sudo systemctl status emqx
sudo systemctl status postgresql
sudo systemctl status taosd

# 2. 验证API端点
echo "2. 验证API端点..."
curl -f http://localhost:18083/api/v4/nodes
curl -f http://localhost:18083/api/v4/clients
curl -f http://localhost:18083/api/v4/subscriptions

# 3. 验证数据库连接
echo "3. 验证数据库连接..."
psql -h localhost -U dgiot -d dgiot -c "SELECT version();"
taos -s "SHOW DATABASES;"

# 4. 验证插件功能
echo "4. 验证插件功能..."
_build/emqx/rel/emqx/bin/emqx eval "application:which_applications()." | grep -E "dgiot|emqx"

# 5. 性能基准测试
echo "5. 性能基准测试..."
./deploy/performance_benchmark.sh

echo "========================================"
echo "部署验证测试完成！"
```

### 7.2 端到端测试
```bash
#!/bin/bash
# deploy/end_to_end_test.sh

echo "开始端到端测试..."
echo "========================================"

# 1. 启动测试环境
echo "1. 启动测试环境..."
./deploy/setup_test_environment.sh

# 2. 模拟设备连接
echo "2. 模拟设备连接..."
python3 test/device_simulator.py --count 10 --interval 1000

# 3. 发送测试数据
echo "3. 发送测试数据..."
python3 test/data_generator.py --type telemetry --count 100

# 4. 验证数据存储
echo "4. 验证数据存储..."
taos -s "SELECT COUNT(*) FROM dgiot.devices;"
psql -h localhost -U dgiot -d dgiot -c "SELECT COUNT(*) FROM devices;"

# 5. 验证数据查询
echo "5. 验证数据查询..."
curl -X POST http://localhost:18083/api/v4/data/query \
  -H "Content-Type: application/json" \
  -d '{"device_id": "test_device_001", "start_time": "2023-01-01T00:00:00Z", "end_time": "2023-01-02T00:00:00Z"}'

# 6. 清理测试环境
echo "6. 清理测试环境..."
./deploy/cleanup_test_environment.sh

echo "========================================"
echo "端到端测试完成！"
```

### 7.3 性能测试
```bash
#!/bin/bash
# deploy/performance_benchmark.sh

echo "开始性能基准测试..."
echo "========================================"

# 1. 连接性能测试
echo "1. 连接性能测试..."
./test/connection_benchmark.sh --clients 1000 --duration 60

# 2. 消息吞吐量测试
echo "2. 消息吞吐量测试..."
./test/message_throughput.sh --rate 1000 --duration 60

# 3. 数据插入性能测试
echo "3. 数据插入性能测试..."
python3 test/data_insert_benchmark.py --count 10000 --batch 100

# 4. 查询性能测试
echo "4. 查询性能测试..."
python3 test/query_benchmark.py --queries 1000 --concurrent 10

# 5. 生成性能报告
echo "5. 生成性能报告..."
./test/generate_performance_report.sh

echo "========================================"
echo "性能基准测试完成！"
```

## 8. 回滚和恢复工作流

### 8.1 部署回滚
```bash
#!/bin/bash
# deploy/rollback_deployment.sh

echo "开始部署回滚..."
echo "========================================"

# 1. 停止当前服务
echo "1. 停止当前服务..."
sudo systemctl stop emqx

# 2. 查找最近的备份
BACKUP_DIR=$(ls -td /opt/emqx_backup_* | head -1)

if [ -z "$BACKUP_DIR" ]; then
    echo "错误：未找到备份目录"
    exit 1
fi

echo "找到备份目录: $BACKUP_DIR"

# 3. 恢复备份
echo "2. 恢复备份..."
sudo rm -rf /opt/emqx
sudo cp -r $BACKUP_DIR /opt/emqx

# 4. 恢复配置文件
echo "3. 恢复配置文件..."
sudo cp $BACKUP_DIR/etc/* /opt/emqx/etc/

# 5. 启动服务
echo "4. 启动服务..."
sudo systemctl start emqx

# 6. 验证回滚
echo "5. 验证回滚..."
sleep 5
sudo systemctl status emqx
curl -f http://localhost:18083/api/v4/nodes

echo "========================================"
echo "部署回滚完成！"
```

### 8.2 数据恢复
```bash
#!/bin/bash
# deploy/data_recovery.sh

echo "开始数据恢复..."
echo "========================================"

# 1. 备份当前数据
echo "1. 备份当前数据..."
BACKUP_TIMESTAMP=$(date +%Y%m%d_%H%M%S)
sudo -u postgres pg_dump dgiot > /tmp/dgiot_backup_$BACKUP_TIMESTAMP.sql
taos -s "BACKUP DATABASE dgiot;" > /tmp/tdengine_backup_$BACKUP_TIMESTAMP.log

# 2. 恢复PostgreSQL数据
echo "2. 恢复PostgreSQL数据..."
if [ -f "deploy/backup/postgres_latest.sql" ]; then
    sudo -u postgres psql -d dgiot -f deploy/backup/postgres_latest.sql
fi

# 3. 恢复TDengine数据
echo "3. 恢复TDengine数据..."
if [ -f "deploy/backup/tdengine_latest.tar.gz" ]; then
    tar -xzf deploy/backup/tdengine_latest.tar.gz -C /var/lib/taos/
    taos -s "RESTORE DATABASE dgiot;"
fi

# 4. 验证数据恢复
echo "4. 验证数据恢复..."
psql -h localhost -U dgiot -d dgiot -c "SELECT COUNT(*) FROM devices;"
taos -s "SELECT COUNT(*) FROM dgiot.devices;"

echo "========================================"
echo "数据恢复完成！"
```

## 9. 自动化部署工作流

### 9.1 CI/CD流水线
```yaml
# .github/workflows/deploy.yml
name: Deploy DG-IoT

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run tests
        run: make test

  build:
    needs: test
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Build release
        run: make rel

  deploy:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v3
      - name: Deploy to production
        run: |
          scp _build/prod/rel/emqx/emqx-*.tar.gz user@production-server:/tmp/
          ssh user@production-server "bash -s" < deploy/remote_deploy.sh
```

### 9.2 远程部署脚本
```bash
#!/bin/bash
# deploy/remote_deploy.sh

echo "开始远程部署..."
echo "========================================"

# 1. 停止服务
sudo systemctl stop emqx

# 2. 备份当前版本
BACKUP_DIR="/opt/emqx_backup_$(date +%Y%m%d_%H%M%S)"
sudo cp -r /opt/emqx $BACKUP_DIR

# 3. 解压新版本
sudo tar -xzf /tmp/emqx-*.tar.gz -C /opt/
sudo mv /opt/emqx-* /opt/emqx_new

# 4. 迁移配置
sudo cp $BACKUP_DIR/etc/* /opt/emqx_new/etc/

# 5. 切换版本
sudo rm -rf /opt/emqx
sudo mv /opt/emqx_new /opt/emqx

# 6. 启动服务
sudo systemctl start emqx

# 7. 验证部署
sleep 10
sudo systemctl status emqx
curl -f http://localhost:18083/api/v4/nodes

echo "========================================"
echo "远程部署完成！"
```

## 10. 最佳实践和故障排除

### 10.1 部署最佳实践
1. **始终备份**：部署前备份配置和数据
2. **分阶段部署**：先在测试环境验证，再部署到生产环境
3. **监控部署过程**：实时监控日志和指标
4. **自动化测试**：部署后运行自动化测试验证功能
5. **回滚计划**：准备完整的回滚方案

### 10.2 常见故障排除
```bash
# 1. 服务启动失败
# 检查日志：sudo journalctl -u emqx -f
# 检查端口占用：sudo netstat -tlnp | grep :1883
# 检查权限：sudo chown -R emqx:emqx /opt/emqx

# 2. 数据库连接失败
# 检查服务状态：systemctl status postgresql
# 检查连接配置：cat /opt/emqx/etc/dgiot.env
# 测试连接：psql -h localhost -U dgiot -d dgiot

# 3. 插件加载失败
# 检查依赖：_build/emqx/rel/emqx/bin/emqx eval "application:which_applications()."
# 重新编译：_build/emqx/rel/emqx/bin/emqx eval "dgiot_plugin:compile(dgiot_drone)."
# 重新加载：_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_drone).'

# 4. 性能问题
# 监控资源：top, htop, vmstat
# 检查日志：tail -f /var/log/emqx/emqx.log
# 调整配置：/opt/emqx/etc/emqx.conf
```

### 10.3 监控指标
- **服务可用性**：HTTP API响应时间，MQTT连接成功率
- **性能指标**：消息吞吐量，连接数，CPU/内存使用率
- **数据指标**：数据插入速率，查询响应时间，存储使用量
- **业务指标**：设备在线率，数据完整性，告警数量

## 11. 更新和维护

### 11.1 定期维护任务
```bash
# 每周维护任务
0 2 * * 0 /opt/emqx/scripts/weekly_maintenance.sh

# 每月维护任务
0 3 1 * * /opt/emqx/scripts/monthly_maintenance.sh

# 季度维护任务
0 4 1 */3 * /opt/emqx/scripts/quarterly_maintenance.sh
```

### 11.2 版本升级
```bash
#!/bin/bash
# deploy/upgrade_version.sh

echo "开始版本升级..."
echo "========================================"

# 1. 检查当前版本
CURRENT_VERSION=$(/opt/emqx/bin/emqx versions | grep emqx | awk '{print $2}')
echo "当前版本: $CURRENT_VERSION"

# 2. 下载新版本
NEW_VERSION="4.4.0"
wget https://github.com/emqx/emqx/releases/download/v$NEW_VERSION/emqx-$NEW_VERSION-ubuntu20.04-amd64.tar.gz

# 3. 执行升级
sudo systemctl stop emqx
sudo tar -xzf emqx-$NEW_VERSION-ubuntu20.04-amd64.tar.gz -C /opt/
sudo mv /opt/emqx-$NEW_VERSION /opt/emqx_new

# 4. 迁移配置和数据
sudo cp -r /opt/emqx/etc /opt/emqx_new/
sudo cp -r /opt/emqx/data /opt/emqx_new/
sudo cp -r /opt/emqx/log /opt/emqx_new/

# 5. 切换版本
sudo rm -rf /opt/emqx
sudo mv /opt/emqx_new /opt/emqx

# 6. 启动服务
sudo systemctl start emqx

# 7. 验证升级
sleep 10
NEW_VERSION_ACTUAL=$(/opt/emqx/bin/emqx versions | grep emqx | awk '{print $2}')
echo "升级后版本: $NEW_VERSION_ACTUAL"

if [ "$NEW_VERSION" = "$NEW_VERSION_ACTUAL" ]; then
    echo "版本升级成功！"
else
    echo "版本升级失败！"
    exit 1
fi

echo "========================================"
echo "版本升级完成！"
```

## 总结

本部署工作流提供了完整的DG-IoT平台部署指南，包括：
1. **环境准备**：检查依赖和准备部署包
2. **数据库部署**：PostgreSQL和TDengine的安装配置
3. **平台部署**：单节点和集群部署方案
4. **插件部署**：热部署和批量部署策略
5. **配置管理**：配置文件和密钥管理
6. **监控日志**：监控系统和日志管理设置
7. **验证测试**：部署验证和性能测试
8. **回滚恢复**：部署回滚和数据恢复方案
9. **自动化部署**：CI/CD流水线和远程部署
10. **最佳实践**：故障排除和维护指南

通过遵循本工作流，可以确保DG-IoT平台的部署过程标准化、可重复且可靠，提高部署效率和质量。

---

**更新记录：**
- 2025-12-03：创建部署工作流文档
- 基于DG-IoT平台部署最佳实践

**相关资源：**
- [DG-IoT官方文档](https://github.com/dgiot/dgiot)
- [EMQX部署指南](https://docs.emqx.com/zh/emqx/latest/deploy/install.html)
- [TDengine部署指南](https://docs.taosdata.com/get-started/)
