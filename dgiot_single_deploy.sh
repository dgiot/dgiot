#!/bin/bash
# DG-IoT 单机版一键部署脚本
# 精简版本，专注于核心功能
# 创建时间: 2026-01-03

set -e  # 遇到错误立即退出

# 颜色设置
RED='\033[0;31m'
GREEN='\033[1;32m'
YELLOW='\033[33m'
NC='\033[0m'

# 路径设置
install_dir="/data/dgiot"
lanip=""

###############################################################################
# 帮助函数
###############################################################################

function show_help() {
  echo "DG-IoT 单机版一键部署脚本"
  echo "用法: $(basename $0) [-h]"
  echo "选项:"
  echo "  -h, --help     显示帮助信息"
  echo ""
  echo "功能:"
  echo "  1. 自动检测操作系统"
  echo "  2. 安装Docker和必要工具"
  echo "  3. 部署TDengine时序数据库(Docker)"
  echo "  4. 部署PostgreSQL数据库"
  echo "  5. 安装Nginx反向代理"
  echo "  6. 部署DG-IoT核心服务
  7. 配置工控机WiFi连接（可选）"
  exit 0
}

###############################################################################
# 系统检测函数
###############################################################################

function detect_system() {
  echo -e "${GREEN}[1/6] 检测系统环境...${NC}"

  # 检查root权限
  if [ "$(id -u)" != "0" ]; then
    echo -e "${RED}错误: 请使用root权限运行此脚本${NC}"
    exit 1
  fi

  # 检测操作系统
  if [ -f /etc/os-release ]; then
    . /etc/os-release
    OS=$NAME
    VER=$VERSION_ID
  else
    OS=$(uname -s)
    VER=$(uname -r)
  fi

  # 中标麒麟/银河麒麟/OpenEuler 兼容检测
  if echo "$OS" | grep -qwi "Kylin"; then
    OS_TYPE="kylin"
    echo -e "检测到中标麒麟/银河麒麟 $VER"
  elif echo "$OS" | grep -qwi "openEuler"; then
    OS_TYPE="openEuler"
    echo -e "检测到 openEuler $VER"
  elif echo "$OS" | grep -qwi "CentOS"; then
    OS_TYPE="centos"
  elif echo "$OS" | grep -qwi "Ubuntu"; then
    OS_TYPE="ubuntu"
  elif echo "$OS" | grep -qwi "Debian"; then
    OS_TYPE="debian"
  else
    OS_TYPE="linux"
  fi

  echo -e "操作系统: $OS $VER ($OS_TYPE)"

  # 获取IP地址
  lanip=$(hostname -I | awk '{print $1}')
  echo -e "服务器IP: $lanip"

  # 检查CPU架构
  ARCH=$(uname -m)
  echo -e "CPU架构: $ARCH"

  # 麒麟/openEuler 特需: 关闭安全策略(部署期间)
  if [ "$OS_TYPE" = "kylin" ] || [ "$OS_TYPE" = "openEuler" ]; then
    echo -e "${YELLOW}麒麟/openEuler 系统: 关闭 SELinux/KYSEC...${NC}"
    setenforce 0 2>/dev/null || true
    if systemctl is-active --quiet firewalld; then
      systemctl stop firewalld 2>/dev/null || true
      systemctl disable firewalld 2>/dev/null || true
    fi
  fi
}

###############################################################################
# 安装基础工具
###############################################################################

function install_basic_tools() {
  echo -e "${GREEN}[2/6] 安装基础工具...${NC}"
  
  # 创建安装目录
  mkdir -p $install_dir
  
  # 配置Docker存储目录
  echo -e "配置Docker存储目录..."
  mkdir -p /data/dgiot/docker
  mkdir -p /data/dgiot/docker/tmp
  mkdir -p /data/dgiot/docker/image/vfs/imagedb/content/sha256
  
  # 创建Docker配置文件
  cat > /etc/docker/daemon.json <<EOF
{
  "data-root": "/data/dgiot/docker",
  "storage-driver": "overlay2",
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "10m",
    "max-file": "3"
  }
}
EOF
  
  # 根据系统类型安装工具
  if command -v dnf &> /dev/null; then
    # openEuler / 中标麒麟 / 银河麒麟 / Fedora
    echo -e "安装必要工具 (dnf)..."
    dnf install -y wget curl git nginx > /dev/null 2>&1
    # Docker: 麒麟/openEuler 可能没有docker包, 用官方脚本
    if ! command -v docker &> /dev/null; then
      curl -fsSL https://get.docker.com | bash > /dev/null 2>&1
    fi
    systemctl start docker > /dev/null 2>&1
    systemctl enable docker > /dev/null 2>&1
  elif command -v yum &> /dev/null; then
    # CentOS/RHEL
    echo -e "安装必要工具 (yum)..."
    yum install -y wget curl git docker docker-compose nginx > /dev/null 2>&1
    systemctl start docker > /dev/null 2>&1
    systemctl enable docker > /dev/null 2>&1
  elif command -v apt-get &> /dev/null; then
    # Debian/Ubuntu
    echo -e "更新包列表..."
    apt-get update > /dev/null 2>&1
    echo -e "安装必要工具..."
    apt-get install -y wget curl git docker.io docker-compose nginx > /dev/null 2>&1
  else
    echo -e "${YELLOW}警告: 未知包管理器，尝试安装Docker...${NC}"
    curl -fsSL https://get.docker.com | bash > /dev/null 2>&1
  fi
  
  # 启动Docker服务
  systemctl daemon-reload
  systemctl restart docker 2>/dev/null || true
  systemctl enable docker 2>/dev/null || true
  
  echo -e "${GREEN}基础工具安装完成！${NC}"
}

###############################################################################
# 部署TDengine时序数据库
###############################################################################

function deploy_tdengine() {
  echo -e "${GREEN}[3/6] 部署TDengine时序数据库...${NC}"
  
  # 停止并删除现有容器
  docker stop tdengine 2>/dev/null || true
  docker rm tdengine 2>/dev/null || true
  
  # 创建数据目录
  mkdir -p $install_dir/tdengine/data
  mkdir -p $install_dir/tdengine/log
  
  # 检查是否已有TDengine镜像
  if docker images | grep -q "tdengine/tdengine"; then
    echo -e "使用本地TDengine Docker镜像..."
  else
    echo -e "${YELLOW}未找到本地TDengine镜像，尝试下载离线包...${NC}"
    
    # 下载TDengine离线安装包
    echo -e "下载TDengine离线安装包..."
    wget -q https://dgaiot-1308220533.cos.ap-guangzhou.myqcloud.com/tdengine-tsdb-oss-docker-3.3.8.8-linux-x64.tar.gz \
      -O $install_dir/tdengine-tsdb-oss-docker-3.3.8.8-linux-x64.tar.gz
    
    if [ $? -eq 0 ]; then
      echo -e "加载TDengine Docker镜像..."
      # 确保Docker存储目录存在
      mkdir -p /data/dgiot/docker/tmp
      mkdir -p /data/dgiot/docker/image/vfs/imagedb/content/sha256
      
      # 使用docker load加载镜像
      docker load -i $install_dir/tdengine-tsdb-oss-docker-3.3.8.8-linux-x64.tar.gz
      
      if [ $? -eq 0 ]; then
        echo -e "标记TDengine镜像..."
        # 标记镜像为正确的名称
        docker tag tdengine/tsdb-amd64:3.3.8.8 tdengine/tsdb:3.3.8.8
        
        echo -e "${GREEN}TDengine离线包安装完成！${NC}"
      else
        echo -e "${RED}TDengine镜像加载失败${NC}"
        echo -e "请手动安装TDengine:"
        echo -e "1. 确保目录存在: mkdir -p /data/dgiot/docker/tmp /data/dgiot/docker/image/vfs/imagedb/content/sha256"
        echo -e "2. 执行: docker load -i $install_dir/tdengine-tsdb-oss-docker-3.3.8.8-linux-x64.tar.gz"
        echo -e "3. 执行: docker tag tdengine/tsdb-amd64:3.3.8.8 tdengine/tsdb:3.3.8.8"
        return 1
      fi
      
      # 清理临时文件
      rm -rf $install_dir/tdengine-tsdb-oss-docker-3.3.8.8-linux-x64.tar.gz
      
    else
      echo -e "${RED}TDengine离线包下载失败${NC}"
      echo -e "请手动下载并安装TDengine"
      echo -e "下载地址: https://dgaiot-1308220533.cos.ap-guangzhou.myqcloud.com/tdengine-tsdb-oss-docker-3.3.8.8-linux-x64.tar.gz"
      return 1
    fi
  fi
  
  # 运行TDengine容器
  echo -e "启动TDengine容器..."
  docker run -d \
    --name tdengine \
    --hostname tdengine \
    --restart always \
    -p 6030:6030 \
    -p 6041:6041 \
    -p 6043:6043 \
    -p 6044-6049:6044-6049 \
    -p 6044-6045:6044-6045/udp \
    -p 6060:6060 \
    -v $install_dir/tdengine/data:/var/lib/taos \
    -v $install_dir/tdengine/log:/var/log/taos \
    tdengine/tsdb:3.3.8.8
  
  # 等待TDengine启动
  echo -e "等待TDengine启动..."
  sleep 10
  
  # 测试连接
  if docker exec tdengine taos -s "show databases;" &> /dev/null; then
    echo -e "${GREEN}TDengine部署成功！${NC}"
  else
    echo -e "${YELLOW}TDengine启动中，可能需要更多时间...${NC}"
    sleep 10
  fi
}

###############################################################################
# 部署PostgreSQL数据库
###############################################################################

function deploy_postgresql() {
  echo -e "${GREEN}[4/6] 部署PostgreSQL数据库...${NC}"
  
  # 停止并删除现有容器
  docker stop postgresql 2>/dev/null || true
  docker rm postgresql 2>/dev/null || true
  
  # 创建数据目录
  mkdir -p $install_dir/postgresql/data
  
  # 检查是否已有PostgreSQL镜像
  if docker images | grep -q "postgres.*13-alpine"; then
    echo -e "使用本地PostgreSQL Docker镜像..."
  else
    echo -e "${YELLOW}警告: 未找到本地PostgreSQL镜像${NC}"
    echo -e "请手动加载PostgreSQL镜像: docker load -i postgres.tar"
    echo -e "或从离线包安装"
    return 1
  fi
  
  # 生成随机密码
  pg_password=$(openssl rand -hex 8)
  
  # 运行PostgreSQL容器
  echo -e "启动PostgreSQL容器..."
  docker run -d \
    --name postgresql \
    --restart always \
    -p 5432:5432 \
    -e POSTGRES_PASSWORD=$pg_password \
    -e POSTGRES_DB=dgiot \
    -v $install_dir/postgresql/data:/var/lib/postgresql/data \
    postgres:13-alpine
  
  # 等待PostgreSQL启动
  echo -e "等待PostgreSQL启动..."
  sleep 15
  
  # 创建DG-IoT数据库和用户
  echo -e "初始化DG-IoT数据库..."
  docker exec postgresql psql -U postgres -c "CREATE DATABASE parse;" 2>/dev/null || true
  docker exec postgresql psql -U postgres -c "CREATE USER dgiot WITH PASSWORD 'CHANGEME';" 2>/dev/null || true
  docker exec postgresql psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE parse TO dgiot;" 2>/dev/null || true
  
  echo -e "${GREEN}PostgreSQL部署成功！${NC}"
  echo -e "数据库信息:"
  echo -e "  地址: localhost:5432"
  echo -e "  数据库: parse"
  echo -e "  用户名: dgiot"
  echo -e "  密码: CHANGEME"
}

###############################################################################
# 配置Nginx反向代理
###############################################################################

function configure_nginx() {
  echo -e "${GREEN}[5/6] 配置Nginx反向代理...${NC}"
  
  # 创建Nginx配置
  cat > /etc/nginx/conf.d/dgiot.conf <<EOF
server {
    listen 80;
    server_name _;
    
    # DG-IoT API
    location / {
        proxy_pass http://127.0.0.1:5080;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
    }
    
    # DG-IoT Dashboard
    location /dashboard/ {
        proxy_pass http://127.0.0.1:18083/;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
    }
    
    # TDengine REST API
    location /rest/ {
        proxy_pass http://127.0.0.1:6041/;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
    }
}
EOF
  
  # 测试Nginx配置
  nginx -t
  
  # 重启Nginx
  systemctl restart nginx
  systemctl enable nginx
  
  echo -e "${GREEN}Nginx配置完成！${NC}"
}

###############################################################################
# 部署DG-IoT核心服务
###############################################################################

function deploy_dgiot_core() {
  echo -e "${GREEN}[6/6] 部署DG-IoT核心服务...${NC}"
  
  # 创建DG-IoT目录
  mkdir -p $install_dir/dgiot
  
  # 下载DG-IoT（这里需要从官方源下载）
  echo -e "下载DG-IoT核心服务..."
  
  # 注意：这里需要实际的DG-IoT安装包
  # 暂时创建目录结构
  mkdir -p $install_dir/dgiot/bin
  mkdir -p $install_dir/dgiot/etc
  mkdir -p $install_dir/dgiot/data
  
  # 创建正确的systemd服务文件（基于用户提供的配置）
  cat > /etc/systemd/system/dgiot.service <<EOF
[Unit]
Description=dgiot server
After=network-online.target
Wants=network-online.target

[Service]
Type=forking
ExecStart=/bin/sh $install_dir/dgiot/bin/emqx start
User=root
Group=root
Environment=HOME=$install_dir/dgiot
KillMode=mixed
KillSignal=SIGINT
TimeoutSec=300
OOMScoreAdjust=-1000
TimeoutStopSec=1000000s
TimeoutStartSec=0
StandardOutput=null
Restart=always
StartLimitBurst=3
StartLimitInterval=60s

[Install]
WantedBy=multi-user.target
EOF
  
  # 创建启动脚本
  cat > $install_dir/dgiot/start.sh <<EOF
#!/bin/bash
# DG-IoT启动脚本

echo "启动DG-IoT服务..."
export HOME=$install_dir/dgiot

# 检查PostgreSQL连接
echo "检查数据库连接..."
sleep 10

# 启动DG-IoT
$install_dir/dgiot/bin/emqx start

echo "DG-IoT服务启动完成"
EOF
  
  chmod +x $install_dir/dgiot/start.sh
  
  # 创建配置文件示例
  cat > $install_dir/dgiot/etc/emqx.conf <<EOF
## DG-IoT配置示例

## 监听端口
listener.tcp.external = 0.0.0.0:5080
listener.ws.external = 0.0.0.0:8083
listener.wss.external = 0.0.0.0:8084

## 数据库配置
dgiot.postgres.host = localhost
dgiot.postgres.port = 5432
dgiot.postgres.database = parse
dgiot.postgres.username = dgiot
dgiot.postgres.password = CHANGEME

## TDengine配置
dgiot.tdengine.host = localhost
dgiot.tdengine.port = 6030
dgiot.tdengine.database = dgiot

## 日志配置
log.to = file
log.file = $install_dir/dgiot/log/emqx.log
log.level = info
EOF
  
  # 创建数据目录
  mkdir -p $install_dir/dgiot/log
  mkdir -p $install_dir/dgiot/data/mnesia
  
  # 设置权限
  chmod -R 755 $install_dir/dgiot
  
  # 创建dgiot_parse_server服务文件
  cat > /etc/systemd/system/dgiot_parse_server.service <<EOF
[Unit]
Description=dgiot_parse_server server
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
ExecStart=$install_dir/dgiot_parse_server/script/node/bin/node $install_dir/dgiot_parse_server/server/index.js
KillMode=mixed
KillSignal=SIGINT
TimeoutSec=300
OOMScoreAdjust=-1000
TimeoutStopSec=1000000s
LimitNOFILE=infinity
LimitNPROC=infinity
LimitCORE=infinity
TimeoutStartSec=0
StandardOutput=null
Restart=always
StartLimitBurst=3
StartLimitInterval=60s

[Install]
WantedBy=multi-user.target
EOF
  
  # 创建dgiot_redis服务文件
  cat > /etc/systemd/system/dgiot_redis.service <<EOF
[Unit]
Description=dgiot_redis server
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
ExecStart=$install_dir/dgiot_parse_server/script/redis/src/redis-server $install_dir/dgiot_parse_server/script/redis.conf
KillMode=mixed
KillSignal=SIGINT
TimeoutSec=300
OOMScoreAdjust=-1000
TimeoutStopSec=1000000s
LimitNOFILE=infinity
LimitNPROC=infinity
LimitCORE=infinity
TimeoutStartSec=0
StandardOutput=null
Restart=always
StartLimitBurst=3
StartLimitInterval=60s

[Install]
WantedBy=multi-user.target
EOF
  
  # 创建dgiot_report_server服务文件
  cat > /etc/systemd/system/dgiot_report_server.service <<EOF
[Unit]
Description=dgiot_report_server
After=network-online.target
Wants=network-online.target

[Service]
Type=forking
ExecStart=$install_dir/dgiot_report_server-4.0.0/bin/startup.sh
ExecStop=$install_dir/dgiot_report_server-4.0.0/bin/shutdown.sh

[Install]
WantedBy=multi-user.target
EOF
  
  # 创建n2n edge服务文件
  cat > /etc/systemd/system/n2n-edge.service <<EOF
[Unit]
Description=n2n edge process
After=network-online.target syslog.target nfw.target
Wants=network-online.target

[Service]
Type=simple
ExecStartPre=
ExecStart=/usr/sbin/edge /etc/n2n/edge.conf -f
Restart=on-abnormal
RestartSec=5

[Install]
WantedBy=multi-user.target
Alias=
EOF
  
  # 创建dgiot_parse_server目录结构
  mkdir -p $install_dir/dgiot_parse_server
  mkdir -p $install_dir/dgiot_parse_server/script/node/bin
  mkdir -p $install_dir/dgiot_parse_server/script/redis/src
  mkdir -p $install_dir/dgiot_parse_server/server
  
  # 创建Redis配置文件
  cat > $install_dir/dgiot_parse_server/script/redis.conf <<EOF
# DG-IoT Redis配置
port 6379
bind 0.0.0.0
protected-mode no
daemonize no
pidfile /var/run/redis_6379.pid
loglevel notice
logfile ""
databases 16
save 900 1
save 300 10
save 60 10000
stop-writes-on-bgsave-error yes
rdbcompression yes
rdbchecksum yes
dbfilename dump.rdb
dir ./
requirepass CHANGEME
maxclients 10000
maxmemory 2gb
maxmemory-policy allkeys-lru
appendonly yes
appendfilename "appendonly.aof"
appendfsync everysec
no-appendfsync-on-rewrite no
auto-aof-rewrite-percentage 100
auto-aof-rewrite-min-size 64mb
aof-load-truncated yes
aof-use-rdb-preamble yes
lua-time-limit 5000
slowlog-log-slower-than 10000
slowlog-max-len 128
latency-monitor-threshold 0
notify-keyspace-events ""
hash-max-ziplist-entries 512
hash-max-ziplist-value 64
list-max-ziplist-size -2
list-compress-depth 0
set-max-intset-entries 512
zset-max-ziplist-entries 128
zset-max-ziplist-value 64
hll-sparse-max-bytes 3000
stream-node-max-bytes 4096
stream-node-max-entries 100
activerehashing yes
client-output-buffer-limit normal 0 0 0
client-output-buffer-limit replica 256mb 64mb 60
client-output-buffer-limit pubsub 32mb 8mb 60
hz 10
dynamic-hz yes
aof-rewrite-incremental-fsync yes
rdb-save-incremental-fsync yes
EOF
  
  # 创建示例index.js文件
  cat > $install_dir/dgiot_parse_server/server/index.js <<EOF
// DG-IoT Parse Server
// 这是一个示例文件，实际需要从官方源获取

console.log('DG-IoT Parse Server starting...');
console.log('Database connection: postgresql://localhost:5432/parse');
console.log('Redis connection: redis://localhost:6379');
console.log('Server listening on port 1337');

// 保持进程运行
setInterval(() => {
  console.log('Parse server is running...');
}, 60000);
EOF
  
  # 启用并启动服务
  systemctl daemon-reload
  systemctl enable dgiot
  systemctl enable dgiot_parse_server
  
  echo -e "${GREEN}DG-IoT服务配置完成！${NC}"
  echo -e "${YELLOW}注意: 需要手动安装以下组件:${NC}"
  echo -e "${YELLOW}  1. DG-IoT二进制文件到 $install_dir/dgiot/bin/${NC}"
  echo -e "${YELLOW}  2. Node.js到 $install_dir/dgiot_parse_server/script/node/bin/${NC}"
  echo -e "${YELLOW}  3. Parse Server代码到 $install_dir/dgiot_parse_server/server/${NC}"
  echo -e "${YELLOW}可以从官方源下载: https://github.com/dgiot/dgiot${NC}"
}

###############################################################################
# 安装VSCode Tunnel和n2n（可选）
###############################################################################

###############################################################################
# WiFi配置函数
###############################################################################

function configure_wifi() {
  echo -e "${GREEN}[7/7] 配置WiFi连接...${NC}"
  echo ""

  # 询问是否配置WiFi
  read -p "是否配置工控机连接dgiot_edge WiFi热点？(y/n): " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "${YELLOW}跳过WiFi配置${NC}"
    return 0
  fi

  # 导入WiFi配置模块
  local script_dir="$(dirname "$(realpath "$0")")"
  if [ -f "${script_dir}/wifi_config_module.sh" ]; then
    echo -e "${GREEN}加载WiFi配置模块...${NC}"
    source "${script_dir}/wifi_config_module.sh"
    # 调用模块中的configure_wifi函数
    configure_wifi
  else
    echo -e "${YELLOW}WiFi配置模块未找到，跳过WiFi配置${NC}"
    echo -e "${YELLOW}请确保wifi_config_module.sh与部署脚本在同一目录${NC}"
  fi

  echo ""
}
