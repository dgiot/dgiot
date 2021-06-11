#!/bin/bash

#1. 保存环境变量
export PATH=$PATH:/usr/local/bin
workdir=`pwd`

#2. 停止postgres数据库
systemctl stop {{pg_writer_name}}
rm /lib/systemd/system/{{pg_writer_name}}.service -rf
killall postgres

#3. 准备postgres的安装运行环境
# 网络检查
ping -c2 baidu.com

#关闭防火墙，selinux
systemctl stop firewalld && sudo systemctl disable firewalld
sed -ri s/SELINUX=enforcing/SELINUX=disabled/g /etc/selinux/config
setenforce 0

### 配置阿里云yum源

mv /etc/yum.repos.d/CentOS-Base.repo /etc/yum.repos.d/CentOS-Base.repo.bak
curl -o /etc/yum.repos.d/CentOS-Base.repo http://mirrors.aliyun.com/repo/Centos-7.repo

#echo "isntalling tools"
yum -y install vim net-tools wget ntpdate
yum -y groupinstall "Development Tools"

# 时间同步
echo "*/10 * * * * /usr/sbin/ntpdate ntp.aliyun.com > /dev/null 2>&1" >>/etc/crontab

# 加快ssh连接
sed -ri s/"#UseDNS yes"/"UseDNS no"/g /etc/ssh/sshd_config
systemctl restart sshd

#部署监控程序
cd /data


if [ ! -f /data/{{node_exporter}}.tar.gz ]; then
  wget http://ci.iotn2n.com/dgiot/package/oem/{{node_exporter}}.tar.gz -O /data/{{node_exporter}}.tar.gz
fi

rm {{node_exporter}} -rf

tar xf {{node_exporter}}.tar.gz

systemctl stop node_exporter
rm /usr/lib/systemd/system/node_exporter.service  -rf

cat > /lib/systemd/system/node_exporter.service << "EOF"
[Unit]
Description=node_exporter
After=network.target

[Service]
Type=simple
ExecStart=/data/{{node_exporter}}/node_exporter
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl start node_exporter
systemctl status node_exporter
systemctl enable node_exporter

curl 127.0.0.1:9100
curl 127.0.0.1:9100/metrics

## 创建目录和用户,以及配置环境变量
userdel postgres
groupadd postgres
useradd -g postgres postgres
## 密码设置在引号内输入自己的密码
echo "{{pg_pwd}}" | passwd --stdin postgres

echo "export PATH=/usr/local/pgsql/12/bin:$PATH" >/etc/profile.d/pgsql.sh
source /etc/profile.d/pgsql.sh


## 环境准备，根据自身需要，减少或者增加
yum install -y wget git gcc gcc-c++  epel-release llvm5.0 llvm5.0-devel clang libicu-devel perl-ExtUtils-Embed readline readline-devel zlib zlib-devel openssl openssl-devel pam-devel libxml2-devel libxslt-devel openldap-devel systemd-devel tcl-devel python-devel

if [ ! -f /tmp/{{postgresql}}.tar.gz ]; then
   wget http://ci.iotn2n.com/dgiot/package/oem/{{postgresql}}.tar.gz -O /tmp/{{postgresql}}.tar.gz
fi

cd /tmp
tar xf {{postgresql}}.tar.gz
cd {{postgresql}}

randtime=`date +%F_%T`
echo $randtime

if [ -d /data/{{pg_writer_name}} ]; then
   mv /data/{{pg_writer_name}}/ /data/{{pg_writer_name}}_bk_$randtime
fi

mkdir /data/{{pg_writer_name}}/data -p
mkdir /data/{{pg_writer_name}}/archivedir -p

chown -R postgres.postgres /data/{{pg_writer_name}}

./configure --prefix=/usr/local/pgsql/12 --with-pgport={{pg_writer_port}} --enable-nls --with-python --with-tcl --with-gssapi --with-icu --with-openssl --with-pam --with-ldap --with-systemd --with-libxml --with-libxslt
make && make install

cd $workdir
rm $workdir/postgresql-12.0 -rf

#3.搭建主库

##初始化

sudo -u postgres /usr/local/pgsql/12/bin/initdb -D /data/{{pg_writer_name}}/data/ -U postgres --locale=en_US.UTF8 -E UTF8

cp /data/{{pg_writer_name}}/data/{pg_hba.conf,pg_hba.conf.bak}
cp /data/{{pg_writer_name}}/data/{postgresql.conf,postgresql.conf.bak}
##配置postgresql.conf
cat <<-eof >/data/{{pg_writer_name}}/data/postgresql.conf
listen_addresses = '*'
port = {{pg_writer_port}}
max_connections = 100
superuser_reserved_connections = 10
full_page_writes = on
wal_log_hints = off
max_wal_senders = 50
hot_standby = on
log_destination = 'csvlog'
logging_collector = on
log_directory = 'log'
log_filename = 'postgresql-%Y-%m-%d_%H%M%S'
log_rotation_age = 1d
log_rotation_size = 10MB
log_statement = 'mod'
log_timezone = 'PRC'
timezone = 'PRC'
unix_socket_directories = '/tmp'
shared_buffers = 512MB
temp_buffers = 16MB
work_mem = 32MB
effective_cache_size = 2GB
maintenance_work_mem = 128MB
#max_stack_depth = 2MB
dynamic_shared_memory_type = posix
## PITR
full_page_writes = on
wal_buffers = 16MB
wal_writer_delay = 200ms
commit_delay = 0
commit_siblings = 5
wal_level = replica
archive_mode = off
archive_command = 'test ! -f /data/{{pg_writer_name}}/archivedir/%f && cp %p /data/{{pg_writer_name}}/archivedir/%f'
archive_timeout = 60s
eof

cat > /lib/systemd/system/{{pg_writer_name}}.service << "EOF"
[Unit]
Description={{pg_writer_name}} database server
After=network.target

[Service]
Type=notify
User=postgres
Group=postgres
Environment=DATA_DIR=/data/{{pg_writer_name}}/data
ExecStart=/usr/local/pgsql/12/bin/postgres -D ${DATA_DIR}
ExecReload=/bin/kill -HUP $MAINPID
KillMode=mixed
KillSignal=SIGINT
TimeoutSec=300
OOMScoreAdjust=-1000
[Install]
WantedBy=multi-user.target
EOF

chown postgres:postgres /data/{{pg_writer_name}} -R
systemctl daemon-reload
systemctl enable {{pg_writer_name}}
systemctl start {{pg_writer_name}}

psql -U postgres -c "ALTER USER postgres WITH PASSWORD '{{pg_writer_pwd}}';"
psql -U postgres -c "CREATE USER  repl WITH PASSWORD '{{pg_writer_pwd}}' REPLICATION;"
psql -U postgres -c "CREATE DATABASE parse;"

#4. 安装parse server
cd /data/{{parse_server}}
./run delete
rm  /data/{{parse_server_software}} -rf
rm /data/{{parse_server}}  -rf
wget http://www.iotn2n.com/package/{{parse_server_software}} -O /data/{{parse_server_software}}
cd /data/
tar xf {{parse_server_software}}

cat <<-eof >/data/{{parse_server}}/script/.env
# 基础配置
SERVER_NAME = {{parse_server}}
SERVER_PORT = {{parse_server_port}}
SERVER_DOMAIN = http://{{standard_public_ip}}:{{parse_server_port}}
SERVER_PUBLIC = http://{{standard_public_ip}}:{{parse_server_port}}
SERVER_PATH = /parse
GRAPHQL_PATH = http://{{standard_public_ip}}:{{parse_server_port}}/graphql

# 管理配置
DASHBOARD_PATH = /dashboard
DASHBOARD_USER = {{parse_server_username}}
DASHBOARD_PASS = {{parse_server_pwd}}

# 数据配置
DATABASE = postgres://postgres:{{pg_pwd}}@127.0.0.1:{{pg_writer_port}}/parse
REDIS_SOCKET = redis://127.0.0.1:16379/0
REDIS_CACHE = redis://127.0.0.1:16379/1

# 邮箱配置
EMAIL_FROM_ADDRESS = noreply@notice.server
EMAIL_FROM_NAME = 系统通知
EMAIL_SENDMAIL = true

# 接口配置
KEY_APPID = {{parse_server_appid}}
KEY_MASTER = {{parse_server_master_key}}
KEY_READONLY_MASTER = {{parse_server_readonly_master_key}}
KEY_FILE = {{parse_server_file_key}}
KEY_CLIENT = {{parse_server_client_key}}
KEY_JAVASCRIPT = {{parse_server_js_key}}
KEY_RESTAPI = {{parse_server_rest_key}}
KEY_DOTNET = {{parse_server_donet_key}}
KEY_WEBHOOK = {{parse_server_webhook_key}}

# 会话配置
SESSION_LENGTH = 604800

eof

cd /data/{{parse_server}}/
./run install
./run build
./run delete
./run make

systemctl stop {{parse_server}}

rm /usr/lib/systemd/system/{{parse_server}}.service  -rf
cat > /lib/systemd/system/{{parse_server}}.service << "EOF"
[Unit]
Description={{parse_server}}_service
After=network.target {{pg_writer_name}}.service
Requires={{pg_writer_name}}.service

[Service]
Type=simple
ExecStart=/data/{{parse_server}}/script/node/bin/node /data/{{parse_server}}/server/index.js
ExecReload=/bin/kill -HUP $MAINPID
KillMode=mixed
KillSignal=SIGINT
TimeoutSec=300
OOMScoreAdjust=-1000
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable {{parse_server}}
systemctl start {{parse_server}}


#6. 安装erlang/otp环境
yum install -y make gcc gcc-c++ kernel-devel m4 ncurses-devel openssl-devel libstdc++-devel ncurses-devel openssl-devel unixODBC unixODBC-devel libtool-ltdl libtool-ltdl-devel

if [ ! -f /tmp/{{otp_src}}.tar.gz ]; then
  wget http://ci.iotn2n.com/dgiot/package/oem/{{otp_src}}.tar.gz -O /tmp/{{otp_src}}.tar.gz
fi

cd /tmp/
tar xf {{otp_src}}.tar.gz

cd /tmp/{{otp_src}}
./configure
make uninstall
make clean
make -j5
make install

cd $workdir
rm /tmp/{{otp_src}} -rf

#7. 部署dgiot_iot
cd /data
randtime=`date +%F_%T`
echo $randtime

if [ -d /data/{{dgiot_iot}} ]; then
   mv /data/{{dgiot_iot}}/ /data/{{dgiot_iot}}_bk_$randtime
fi

if [ ! -f  /data/{{dgiot_iot_software}} ]; then
  wget http://ci.iotn2n.com/dgiot/package/{{dgiot_iot_software}} -O /data/{{dgiot_iot_software}}
fi

tar xf {{dgiot_iot_software}}
cd  /data/dgiot_iot

count=`ps -ef |grep beam.smp |grep -v "grep" |wc -l`
if [ 0 == $count ];then
   echo $count
  else
   killall -9 beam.smp
fi

#配置licens
sed -i '/^dgiot_auth.license/cdgiot_auth.license = {{standard_dgiot_license}}' /data/{{dgiot_iot}}/etc/plugins/dgiot_license.conf

#parse 连接 配置
sed -i '/^parse.parse_server/cparse.parse_server = http://{{standard_private_ip}}:{{parse_server_port}}' /data/{{dgiot_iot}}/etc/plugins/dgiot_parse.conf
sed -i '/^parse.parse_path/cparse.parse_path = /parse/' /data/{{dgiot_iot}}/etc/plugins/dgiot_parse.conf
sed -i '/^parse.parse_appid/cparse.parse_appid = {{parse_server_appid}}' /data/{{dgiot_iot}}/etc/plugins/dgiot_parse.conf
sed -i '/^parse.parse_master_key/cparse.parse_master_key = {{parse_server_master_key}}' /data/{{dgiot_iot}}/etc/plugins/dgiot_parse.conf
sed -i '/^parse.parse_js_key/cparse.parse_js_key = {{parse_server_js_key}}' /data/{{dgiot_iot}}/etc/plugins/dgiot_parse.conf
sed -i '/^parse.parse_rest_key/cparse.parse_rest_key = {{parse_server_rest_key}}' /data/{{dgiot_iot}}/etc/plugins/dgiot_parse.conf

#修改emq.conf
sed -i '/^node.name/cnode.name = dgiot_iot@{{standard_public_ip}}' /data/dgiot_iot/etc/emqx.conf
mv /data/{{dgiot_iot}}/data/loaded_plugins /data/{{dgiot_iot}}/data/loaded_plugins_bk
cat > /data/{{dgiot_iot}}/data/loaded_plugins << "EOF"
{emqx_management, true}.
{emqx_recon, true}.
{emqx_retainer, true}.
{emqx_dashboard, true}.
{emqx_rule_engine, true}.
{emqx_bridge_mqtt, true}.
{emqx_cube, false}.
{dgiot_statsd, true}.
{dgiot_license, true}.
{dgiot_public, true}.
{dgiot_mqtt, true}.
{dgiot_framework, true}.
{dgiot_device_shadow, true}.
{dgiot_parse, true}.
{dgiot_web_manager,true}.
{dgiot_bridge,true}.
{dgiot_modbus,true}.

EOF


systemctl stop {{dgiot_iot}}

/data/{{dgiot_iot}}/bin/dgiot_iot start

rm /usr/lib/systemd/system/{{dgiot_iot}}.service  -rf
cat > /lib/systemd/system/{{dgiot_iot}}.service << "EOF"
[Unit]
Description={{dgiot_iot}}_service
After=network.target {{parse_server}}.service
Requires={{parse_server}}.service

[Service]
Type=forking
Environment=HOME=/data/{{dgiot_iot}}/erts-10.3
ExecStart=/bin/sh /data/{{dgiot_iot}}/bin/dgiot_iot start
LimitNOFILE=1048576
ExecStop=/bin/sh data/{{dgiot_iot}}/bin/dgiot_iot stop
ExecReload=/bin/kill -HUP $MAINPID
KillMode=mixed
KillSignal=SIGINT
TimeoutSec=300
OOMScoreAdjust=-1000
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable {{dgiot_iot}}
systemctl start {{dgiot_iot}}

sleep 15

#4. 安装td server
#setup mosquitto
yum -y install gcc gcc-c++ libstdc++-devel
yum -y install openssl-devel
yum -y install c-ares-devel
yum -y install uuid-devel
yum -y install libuuid-devel

rm /tmp/mosquitto* -rf
if [ ! -f /tmp/{{mosquitto}}.tar.gz ]; then
    wget http://ci.iotn2n.com/dgiot/package/oem/{{mosquitto}}.tar.gz /tmp/{{mosquitto}}.tar.gz
fi
cd /tmp
tar xvf {{mosquitto}}.tar.gz
cd  {{mosquitto}}
make uninstall
make clean
make install
sudo ln -s /usr/local/lib/libmosquitto.so.1 /usr/lib/libmosquitto.so.1
ldconfig
cd ..
rm mosquitto*  -rf

#5. 安装 tdengine
rmtaos
rm /tmp/{{TDengine}} -rf
if [ ! -f /tmp/{{TDengine}}.tar.gz ]; then
    wget http://ci.iotn2n.com/dgiot/package/oem/{{TDengine}}.tar.gz -O /tmp/{{TDengine}}.tar.gz
fi
cd /tmp
tar xf {{TDengine}}.tar.gz
cd /tmp/{{TDengine}}
/bin/sh install.sh
chmod 777 dgiot_td
cp ./dgiot_td /usr/sbin -rf
rm /tmp/{{TDengine}} -rf

mkdir /data/taos

ldconfig
sed -i '/^# defaultPass/cdefaultPass           {{td_passwd}}' /etc/taos/taos.cfg
sed -i '/^# defaultUser/cdefaultUser           {{td_username}}' /etc/taos/taos.cfg
sed -i '/^# httpPort/chttpPort           {{td_port}}' /etc/taos/taos.cfg
sed -i '/^# dataDir/cdataDir           /data/lib/taos' /etc/taos/taos.cfg
sed -i '/^# logDir/clogDir           /data/log/taos' /etc/taos/taos.cfg
sudo systemctl start taosd

systemctl stop {{td_bridge_name}}
rm /usr/lib/systemd/system/{{td_bridge_name}}.service  -rf
cat > /lib/systemd/system/{{td_bridge_name}}.service << "EOF"
[Unit]
Description={{td_bridge_name}}_service
After=network.target {{dgiot_iot}}.service

[Service]
Type=simple
ExecStart=/usr/sbin/dgiot_td 127.0.0.1 {{appid}} {{appsecret}}
ExecReload=/bin/kill -HUP $MAINPID
KillMode=mixed
KillSignal=SIGINT
TimeoutSec=300
OOMScoreAdjust=-1000
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable {{td_bridge_name}}
systemctl start {{td_bridge_name}}

##应用部署和安装验证
wget http://127.0.0.1:5080/install/iot

content=`wget -q -O -  http://127.0.0.1:5080/iotapi/health`
if [ ${content} == 'ok' ]; then
    psql -U postgres -d parse -c "alter table \"License\" drop constraint \"License_pkey\";"
    psql -U postgres -d parse -c "alter table \"License\" add primary key(key);"
    psql -U postgres -d parse -c "alter table \"App\" drop constraint \"App_pkey\";"
    psql -U postgres -d parse -c "alter table \"App\" add primary key(name);"
    psql -U postgres -d parse -c "alter table \"Project\" drop constraint \"Project_pkey\";"
    psql -U postgres -d parse -c "alter table \"Project\" add primary key(title);"
    psql -U postgres -d parse -c "alter table \"Product\" drop constraint \"Product_pkey\";"
    psql -U postgres -d parse -c "alter table \"Product\" add primary key(name,\"devType\");"
    psql -U postgres -d parse -c "alter table \"Device\" drop constraint \"Device_pkey\";"
    psql -U postgres -d parse -c "alter table \"Device\" add primary key(product,devaddr);"
    psql -U postgres -d parse -c "alter table \"Dict\" drop constraint \"Dict_pkey\";"
    psql -U postgres -d parse -c "alter table \"Dict\" add primary key(type,key);"
    psql -U postgres -d parse -c "alter table \"Channel\" drop constraint \"Channel_pkey\";"
    psql -U postgres -d parse -c "alter table \"Channel\" add primary key(name,\"cType\");"
    psql -U postgres -d parse -c "alter table \"Crond\" drop constraint \"Crond_pkey\";"
    psql -U postgres -d parse -c "alter table \"Crond\" add primary key(name,type);"
    psql -U postgres -d parse -c "alter table \"Department\" drop constraint \"Department_pkey\";"
    psql -U postgres -d parse -c "alter table \"Department\" add primary key(name,org_type);"
    wget "http://{{license_host}}:5080/iotapi/setup_result?license={{standard_dgiot_license}}&result=installed"
else
    wget "http://{{license_host}}:5080/iotapi/setup_result?license={{standard_dgiot_license}}&result=install_fail"
fi

# 安装python3.8
# 1、依赖包安装
yum install -y wget curl zlib-devel bzip2-devel openssl-devel ncurses-devel sqlite-devel readline-devel tk-devel gdbm-devel db4-devel libpcap-devel xz-devel libffi-devel
# 2、下载包
wget https://www.python.org/ftp/python/3.8.0/Python-3.8.0.tgz
# 3、解压
tar -zxvf Python-3.8.0.tgz
# 4、安装
cd Python-3.8.0
./configure --prefix=/usr/local/python3
make && make install
# 5、建立软连接
/usr/bin/
mv python python_bk
mv pip pip_bk
ln -s /usr/local/python3/bin/python3 /usr/bin/python
ln -s /usr/local/python3/bin/pip3 /usr/bin/pip

# 安装numpy
pip install numpy
# 安装matplotlib
pip install matplotlib
# 安装pylab
pip install pylab
