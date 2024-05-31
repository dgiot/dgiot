#!/bin/bash
# This file is used to install dgiot on linux systems. The operating system
# is required to use systemd to manage services at boot
export PATH=$PATH:/usr/local/bin

function help() {
  echo "Usage: $(basename $0) -v [single | cluster | devops | ci |  otp | atomgit] -s [dgiot_n] -p [your_dgiot_plugin] -m [dgiotmd5 | atomplugin] -e [pg_eip] -a [pg_auth] -n [islanip]"
  exit 0
}

function check_os_type() {
  # get the operating system type for using the corresponding init file
  # ubuntu/debian(deb), centos/fedora(rpm), others: opensuse, redhat, ..., no verification
  #osinfo=$(awk -F= '/^NAME/{print $2}' /etc/os-release)
  if [[ -e /etc/os-release ]]; then
    osinfo=$(cat /etc/os-release | grep "NAME" | cut -d '"' -f2) || :
  else
    osinfo=""
  fi
  echo "osinfo: ${osinfo}"
  os_type=0
  if echo $osinfo | grep -qwi "ubuntu"; then
    #  echo "This is ubuntu system"
    os_type=1
  elif echo $osinfo | grep -qwi "debian"; then
    #  echo "This is debian system"
    os_type=1
  elif echo $osinfo | grep -qwi "Kylin"; then
    #  echo "This is Kylin system"
    os_type=1
  elif echo $osinfo | grep -qwi "centos"; then
    #  echo "This is centos system"
    os_type=2
  elif echo $osinfo | grep -qwi "fedora"; then
    #  echo "This is fedora system"
    os_type=2
  elif echo $osinfo | grep -qwi "Amazon"; then
    #  echo "This is Amazon system"
    os_type=2
  elif echo $osinfo | grep -qwi "Red"; then
    #  echo "This is Red Hat system"
    os_type=2
  else
    echo " osinfo: ${osinfo}"
    echo " This is an officially unverified linux system,"
    echo " if there are any problems with the installation and operation, "
    echo " please feel free to contact www.iotn2n.com for support."
    os_type=3
  fi
}

function dgiot_shell() {
  # Color setting
  RED='\033[0;31m'
  GREEN='\033[1;32m'
  BLUE='\033[34m'
  BLACK='\033[30m'
  YELLOW='\033[33m'
  PINK='\033[35m'
  GREEN_DARK='\033[0;32m'
  GREEN_UNDERLINE='\033[4;32m'
  NC='\033[0m'
  script_dir=$(dirname $(readlink -f "$0"))
  csudo=""
  if command -v sudo >/dev/null; then
    csudo="sudo"
  fi
}

function dgiot_fqdn() {
  lanip=""
  wlanip=""
  serverFqdn=""
  dgiot_data=""
  # 网络检查pre_install
  get_lanip
  get_wanip
  # 获取进程数
  get_processor
}

function install_openssl() {
  yum install -y glibc-headers &>/dev/null
  yum install -y openssl openssl-devel &>/dev/null
  yum install -y libstdc++-devel openssl-devel &>/dev/null
}

# rand password
#https://bcrypt-generator.com/
function dgiot_password() {
  install_openssl
  pg_pwd=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_user=dgiot
  parse_pwd=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_appid=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_master=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_readonly_master=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_file=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_client=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_javascript=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_restapi=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_dotnet=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
  parse_webhook=$(openssl rand -hex 8 | md5sum | cut -f1 -d ' ')
}

function dgiot_path() {
  # package from path
  fileserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"
  fileparseserver="https://dgiot-parse-server-1306147891.cos.ap-nanjing.myqcloud.com"
  fileversionserver="https://dgiot-version-1306147891.cos.ap-nanjing.myqcloud.com"
  updateserver="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/update"
  otpversion="24.3.4.2"
  #install path
  install_dir="/data/dgiot"

  #backup path
  randtime=$(date +%F_%T)
  backup_dir=${install_dir}/${randtime}
  mkdir ${backup_dir} -p

  #service path
  service_dir="/lib/systemd/system"
}

function dgiot_auto_variables() {
  # ============================= get auto variables =================================================
  dgiot_path
  dgiot_fqdn
  dgiot_password
}

## 1.3. 部署前处理
function pre_install() {
  ## 1.4 关闭防火墙，selinux
  if systemctl is-active --quiet firewalld; then
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} firewalld is running, stopping it...${NC}"
    ${csudo} systemctl stop firewalld $1 &>/dev/null || echo &>/dev/null
    ${csudo} systemctl disable firewalld $1 &>/dev/null || echo &>/dev/null
  fi

  #  setenforce 0
  sed -ri s/SELINUX=enforcing/SELINUX=disabled/g /etc/selinux/config

  ## 1.5 配置yum源
  if [ ! -f /etc/yum.repos.d/CentOS-Base.repo ]; then
    curl -o /etc/yum.repos.d/CentOS-Base.repo https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/CentOS-Base.repo &>/dev/null
  fi

  ## 1.6 echo "installing tools"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} installing tools${NC}"
  yum -y install vim net-tools wget ntpdate &>/dev/null
  yum -y groupinstall "Development Tools" &>/dev/null
  yum install -y wget git &>/dev/null

  ## 1.7 时间同步
  echo "*/10 * * * * /usr/sbin/ntpdate ntp.aliyun.com > /dev/null 2>&1" >>/etc/crontab

  ## 1.8 加快ssh连接
  sed -ri s/"#UseDNS yes"/"UseDNS no"/g /etc/ssh/sshd_config
  systemctl restart sshd
}

function get_processor() {
  processor=$(cat /proc/cpuinfo | grep "processor" | wc -l)
}

function get_wanip() {
  ping -c2 baidu.com &>/dev/null
  wlanip=$(curl whatismyip.akamai.com)
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} islanip=${islanip} ${NC}"
  if [ "${islanip}" == "true" ]; then
    wlanip=${lanip}
  fi
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} wlanip: ${wlanip}${NC}"
}

function get_lanip() {
  for nic in $(ls /sys/class/net); do
    # shellcheck disable=SC1073
    # echo -e  "`date +%F_%T` $LINENO: ${GREEN} nic: ${nic}${NC}"
    # ingore docker ip
    if [ "${nic}" == "*docker*" ]; then
      continue
    fi

    # ingore lo ip
    if [ "${nic}" == "lo" ]; then
      continue
    fi

    # ingore bridge ip
    if [ "${nic}" == "*br-*" ]; then
      continue
    fi

    tmpip=$(/sbin/ifconfig ${nic} | sed -nr 's/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p')
    if [ -z "$tmpip" ]; then
      continue
    fi

    lanip=$tmpip
  done

  if [ -z "$lanip" ]; then
    lanip=$(/sbin/ifconfig | grep inet | grep -v inet6 | grep -v 127.0.0.1 | awk '{print $2}' | awk -F ":" '{print $2}') || :
  fi

  if [ -z "$lanip" ]; then
    lanip="127.0.0.1"
  fi
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} lanip: ${lanip}${NC}"
}

function kill_process() {
  pid=$(ps -ef | grep "$1" | grep -v "grep" | awk '{print $2}')
  if [ -n "$pid" ]; then
    ${csudo} kill -9 $pid || :
  fi
}

function clean_services() {
  clean_service dgiot_pg_writer
  clean_service dgiot
  clean_service dgiot_parse_server
  #  clean_service dgiot_redis
  clean_service grafana-server
  clean_service go_fastdfs
  clean_service taosd
  clean_service dgiot_tdengine_mqtt
  clean_service prometheus
  clean_service pushgateway
  clean_service node_exporter
  clean_service postgres_exporter
  clean_service nginx
}

function clean_service() {
  if systemctl is-active --quiet $1; then
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} $1 is running, stopping it...${NC}"
    ${csudo} systemctl stop $1 &>/dev/null || echo &>/dev/null
  fi
  ${csudo} systemctl disable $1 &>/dev/null || echo &>/dev/null
  ${csudo} rm -f ${service_dir}/$1.service
}

# $1:service  $2:Type  $3:ExecStart  $4:User  $5:Environment  $6:ExecStop
function install_service() {
  rm -rf ${service_dir}/$1.service
  service_config="${service_dir}/$1.service"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} build ${service_config}${NC}"
  ${csudo} bash -c "echo '[Unit]'                             > ${service_config}"
  ${csudo} bash -c "echo 'Description=$1 server'              >> ${service_config}"
  ${csudo} bash -c "echo 'After=network-online.target'        >> ${service_config}"
  ${csudo} bash -c "echo 'Wants=network-online.target'        >> ${service_config}"
  ${csudo} bash -c "echo                                      >> ${service_config}"
  ${csudo} bash -c "echo '[Service]'                          >> ${service_config}"
  ${csudo} bash -c "echo 'Type=$2'                            >> ${service_config}"
  ${csudo} bash -c "echo 'ExecStart=$3'                        >> ${service_config}"
  if [ ! x"$4" = x ]; then
    ${csudo} bash -c "echo 'User=$4'                          >> ${service_config}"
    ${csudo} bash -c "echo 'Group=$4'                         >> ${service_config}"
  fi
  if [ ! x"$5" = x ]; then
    ${csudo} bash -c "echo 'Environment=$5'                   >> ${service_config}"
  fi
  if [ -$# -eq 6 ]; then
    ${csudo} bash -c "echo 'ExecStop=$6'                       >> ${service_config}"
  fi
  ${csudo} bash -c "echo 'KillMode=mixed'                      >> ${service_config}"
  ${csudo} bash -c "echo 'KillSignal=SIGINT'                   >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutSec=300'                      >> ${service_config}"
  ${csudo} bash -c "echo 'OOMScoreAdjust=-1000'                >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutStopSec=1000000s'             >> ${service_config}"
  ${csudo} bash -c "echo 'LimitNOFILE=infinity'                >> ${service_config}"
  ${csudo} bash -c "echo 'LimitNPROC=infinity'                 >> ${service_config}"
  ${csudo} bash -c "echo 'LimitCORE=infinity'                  >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutStartSec=0'                   >> ${service_config}"
  ${csudo} bash -c "echo 'StandardOutput=null'                 >> ${service_config}"
  ${csudo} bash -c "echo 'Restart=always'                      >> ${service_config}"
  ${csudo} bash -c "echo 'StartLimitBurst=3'                   >> ${service_config}"
  ${csudo} bash -c "echo 'StartLimitInterval=60s'              >> ${service_config}"
  ${csudo} bash -c "echo                                       >> ${service_config}"
  ${csudo} bash -c "echo '[Install]'                           >> ${service_config}"
  ${csudo} bash -c "echo 'WantedBy=multi-user.target'          >> ${service_config}"
  ${csudo} systemctl daemon-reload &>/dev/null
  ${csudo} systemctl enable $1 &>/dev/null
  ${csudo} systemctl start $1 &>/dev/null
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} systemctl start $1${NC}"
}

# $1:service  $2:Type  $3:ExecStart  $4:Environment $5:WorkingDirectory
function install_service1() {
  service_config="${service_dir}/$1.service"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} build ${service_config}${NC}"
  ${csudo} bash -c "echo '[Unit]'                             > ${service_config}"
  ${csudo} bash -c "echo 'Description=$1 server'              >> ${service_config}"
  ${csudo} bash -c "echo 'After=network-online.target'        >> ${service_config}"
  ${csudo} bash -c "echo 'Wants=network-online.target'        >> ${service_config}"
  ${csudo} bash -c "echo                                      >> ${service_config}"
  ${csudo} bash -c "echo '[Service]'                          >> ${service_config}"
  ${csudo} bash -c "echo 'Type=$2'                            >> ${service_config}"
  ${csudo} bash -c "echo 'ExecStart=$3'                        >> ${service_config}"
  if [ ! x"$5" = x ]; then
    ${csudo} bash -c "echo 'WorkingDirectory=$5'               >> ${service_config}"
  fi
  if [ ! x"$4" = x ]; then
    ${csudo} bash -c "echo 'Environment=$4'                   >> ${service_config}"
  fi
  ${csudo} bash -c "echo 'KillMode=mixed'                      >> ${service_config}"
  ${csudo} bash -c "echo 'KillSignal=SIGINT'                   >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutSec=300'                      >> ${service_config}"
  ${csudo} bash -c "echo 'OOMScoreAdjust=-1000'                >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutStopSec=1000000s'             >> ${service_config}"
  ${csudo} bash -c "echo 'LimitNOFILE=infinity'                >> ${service_config}"
  ${csudo} bash -c "echo 'LimitNPROC=infinity'                 >> ${service_config}"
  ${csudo} bash -c "echo 'LimitCORE=infinity'                  >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutStartSec=0'                   >> ${service_config}"
  ${csudo} bash -c "echo 'StandardOutput=null'                 >> ${service_config}"
  ${csudo} bash -c "echo 'Restart=always'                      >> ${service_config}"
  ${csudo} bash -c "echo 'StartLimitBurst=3'                   >> ${service_config}"
  ${csudo} bash -c "echo 'StartLimitInterval=60s'              >> ${service_config}"
  ${csudo} bash -c "echo                                       >> ${service_config}"
  ${csudo} bash -c "echo '[Install]'                           >> ${service_config}"
  ${csudo} bash -c "echo 'WantedBy=multi-user.target'          >> ${service_config}"
  ${csudo} systemctl daemon-reload &>/dev/null
  ${csudo} systemctl enable $1 &>/dev/null
  ${csudo} systemctl start $1 &>/dev/null
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} systemctl start $1${NC}"
}

# $1:service  $2:Type  $3:ExecStart
function install_service2() {
  service_config="${service_dir}/$1.service"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} build ${service_config}${NC}"
  ${csudo} bash -c "echo '[Unit]'                             > ${service_config}"
  ${csudo} bash -c "echo 'Description=$1 server'              >> ${service_config}"
  ${csudo} bash -c "echo 'After=network-online.target'        >> ${service_config}"
  ${csudo} bash -c "echo 'Wants=network-online.target'        >> ${service_config}"
  ${csudo} bash -c "echo                                      >> ${service_config}"
  ${csudo} bash -c "echo '[Service]'                          >> ${service_config}"
  ${csudo} bash -c "echo 'Type=$2'                            >> ${service_config}"
  ${csudo} bash -c "echo 'ExecStart=$3'                        >> ${service_config}"
  ${csudo} bash -c "echo 'KillMode=mixed'                      >> ${service_config}"
  ${csudo} bash -c "echo 'KillSignal=SIGINT'                   >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutSec=300'                      >> ${service_config}"
  ${csudo} bash -c "echo 'OOMScoreAdjust=-1000'                >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutStopSec=1000000s'             >> ${service_config}"
  ${csudo} bash -c "echo 'LimitNOFILE=infinity'                >> ${service_config}"
  ${csudo} bash -c "echo 'LimitNPROC=infinity'                 >> ${service_config}"
  ${csudo} bash -c "echo 'LimitCORE=infinity'                  >> ${service_config}"
  ${csudo} bash -c "echo 'TimeoutStartSec=0'                   >> ${service_config}"
  ${csudo} bash -c "echo 'StandardOutput=null'                 >> ${service_config}"
  ${csudo} bash -c "echo 'Restart=always'                      >> ${service_config}"
  ${csudo} bash -c "echo 'StartLimitBurst=3'                   >> ${service_config}"
  ${csudo} bash -c "echo 'StartLimitInterval=60s'              >> ${service_config}"
  ${csudo} bash -c "echo                                       >> ${service_config}"
  ${csudo} bash -c "echo '[Install]'                           >> ${service_config}"
  ${csudo} bash -c "echo 'WantedBy=multi-user.target'          >> ${service_config}"
  ${csudo} systemctl daemon-reload &>/dev/null
  ${csudo} systemctl enable $1 &>/dev/null
  ${csudo} systemctl start $1 &>/dev/null
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} systemctl start $1${NC}"
}

find_in_file() {
  grep "$1" "$2" >/dev/null
  return $?
}

### set host
set_host() {
  IP=$1
  HOST=$2

  RECORD="${IP} ${HOST}"

  find_in_file "${RECORD}" /etc/hosts
  if [ $? -ne 0 ]; then
    echo ${RECORD} >>/etc/hosts
  fi
}

##---------------------------------------------------------------##
#2 部署档案数据库

## 2.1 部署postgres数据库
### 2.1.1 环境准备，根据自身需要，减少或者增加
function yum_install_postgres() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} yum install postgres${NC}"
  ${csudo} yum install -y gcc gcc-c++ epel-release &>/dev/null
  # ${csudo} yum install -y llvm llvm-devel &> /dev/null
  ${csudo} yum install -y clang libicu-devel perl-ExtUtils-Embed &>/dev/null
  ${csudo} yum install -y readline readline-devel &>/dev/null
  ${csudo} yum install -y zlib zlib-devel &>/dev/null
  ${csudo} yum install -y pam-devel libxml2-devel libxslt-devel &>/dev/null
  ${csudo} yum install -y openldap-devel systemd-devel &>/dev/null
  ${csudo} yum install -y tcl-devel python-devel &>/dev/null
}

### 2.1.2 编译安装postgres
function install_postgres() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} install_postgres${NC}"
  ##下载软件
  if [ ! -f ${script_dir}/postgresql-12.0.tar.gz ]; then
    ${csudo} wget ${fileserver}/postgresql-12.0.tar.gz -O ${script_dir}/postgresql-12.0.tar.gz &>/dev/null
  fi

  ### 创建目录和用户,以及配置环境变化
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} create postgres user and group ${NC}"
  set +uxe
  egrep "^postgres" /etc/passwd >/dev/null
  if [ $? -eq 0 ]; then
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} postgres user and group exist ${NC}"
  else
    ${csudo} groupadd postgres &>/dev/null
    ${csudo} useradd -g postgres postgres &>/dev/null
  fi

  ###  密码设置在引号内输入自己的密码
  echo ${pg_pwd} | passwd --stdin postgres
  echo "export PATH=/usr/local/pgsql/12/bin:$PATH" >/etc/profile.d/pgsql.sh
  source /etc/profile.d/pgsql.sh

  if [ -d ${script_dir}/postgresql-12.0 ]; then
    ${csudo} rm ${script_dir}/postgresql-12.0 -rf
  fi

  tar xvf postgresql-12.0.tar.gz &>/dev/null
  cd ./postgresql-12.0/
  set -ue
  yum_install_postgres
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} configure postgres${NC}"
  ./configure --prefix=/usr/local/pgsql/12 --with-pgport=7432 --enable-nls --with-python --with-tcl --with-gssapi --with-icu --with-openssl --with-pam --with-ldap --with-systemd --with-libxml --with-libxslt &>/dev/null

  echo -e "$(date +%F_%T) $LINENO: ${GREEN} make install postgres${NC}"
  make install -j${processor} &>/dev/null

  ### 添加pg_stat_statements数据库监控插件
  cd ${script_dir}/postgresql-12.0/contrib/pg_stat_statements/
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} make install pg_stat_statements ${NC}"
  make install -j${processor} &>/dev/null
  sleep 5

  cd ${script_dir}/
  rm ${script_dir}/postgresql-12.0 -rf
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} build postgres sueccess${NC}"
}

function init_postgres_database() {
  ### 2.1.4.搭建主数据库
  if [ -d ${install_dir}/dgiot_pg_writer ]; then
    mv ${install_dir}/dgiot_pg_writer/ ${backup_dir}/
  fi

  mkdir ${install_dir}/dgiot_pg_writer/data -p
  mkdir ${install_dir}/dgiot_pg_writer/archivedir -p
  chown -R postgres:postgres /data/
  chown -R postgres:postgres /usr/local/pgsql
  chown -R postgres:postgres ${install_dir}/dgiot_pg_writer
  cd ${install_dir}/dgiot_pg_writer/
  sudo -u postgres /usr/local/pgsql/12/bin/initdb -D ${install_dir}/dgiot_pg_writer/data/ -U postgres --locale=en_US.UTF8 -E UTF8 &>/dev/null
  ${csudo} bash -c "cp ${install_dir}/dgiot_pg_writer/data/{pg_hba.conf,pg_hba.conf.bak}"
  ${csudo} bash -c "cp ${install_dir}/dgiot_pg_writer/data/{postgresql.conf,postgresql.conf.bak}"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} initdb postgres success${NC}"

  ### 2.1.5 配置postgresql.conf
  postgresql_conf="${install_dir}/dgiot_pg_writer/data/postgresql.conf"
  cat >${postgresql_conf} <<"EOF"
  listen_addresses = '*'
  port = 7432
  max_connections = 100
  superuser_reserved_connections = 10
  full_page_writes = on
  wal_log_hints = off
  max_wal_senders = 50
  hot_standby = on
  log_destination = 'csvlog'
  logging_collector = off
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
  archive_timeout = 60s
  #pg_stat_statements
  shared_preload_libraries = 'pg_stat_statements'
  pg_stat_statements.max = 1000
  pg_stat_statements.track = all
EOF
  archivedir="${install_dir}/dgiot_pg_writer/archivedir"
  echo "  archive_command = 'test ! -f ${archivedir}/%f && cp %p ${archivedir}/%f'" >>${postgresql_conf}
  pg_hba_conf="${install_dir}/dgiot_pg_writer/data/pg_hba.conf"
  # METHOD "trust", "reject","md5","password","scram-sha-256","gss","sspi","ident","peer","pam","ldap","radius","cert"
  ${csudo} bash -c "echo 'host    all             all             ${pg_eip}/24           password'    >> ${pg_hba_conf}"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} ${postgresql_conf}${NC}"
}

### 2.1.4 安装部署postgres
function deploy_postgres() {
  clean_service dgiot_pg_writer
  install_postgres
  init_postgres_database
  DATA_DIR="${install_dir}/dgiot_pg_writer/data"
  install_service dgiot_pg_writer "notify" "/usr/local/pgsql/12/bin/postgres -D ${DATA_DIR}" "postgres" "DATA_DIR=${DATA_DIR}"
  sleep 2
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "CREATE USER  repl WITH PASSWORD '${pg_pwd}' REPLICATION;" &>/dev/null
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} deploy postgres success${NC}"
}

function restore_parse_data() {
  ### 下载dgiot_parse_server初始数据
  if [ ! -f ${script_dir}/parse_4.0.sql.tar.gz ]; then
    wget ${fileserver}/parse_4.0.sql.tar.gz -O ${script_dir}/parse_4.0.sql.tar.gz &>/dev/null
  fi
  cp ${script_dir}/parse_4.0.sql.tar.gz ${install_dir}/dgiot_pg_writer/
  cd ${install_dir}/dgiot_pg_writer/
  tar xvf parse_4.0.sql.tar.gz &>/dev/null

  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "ALTER USER postgres WITH PASSWORD '${pg_pwd}';" &>/dev/null
  retval=$(sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "SELECT datname FROM pg_database WHERE datistemplate = false;" &>/dev/null)
  if [[ $retval == *"parse"* ]]; then
    sudo -u postgres /usr/local/pgsql/12/bin/pg_dump -F p -f ${backup_dir}/dgiot_pg_writer/parse_4.0_backup.sql -C -E UTF8 -h 127.0.0.1 -U postgres parse &>/dev/null
    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "DROP DATABASE parse;" &>/dev/null
  fi
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "CREATE DATABASE parse;" &>/dev/null
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -f ${install_dir}/dgiot_pg_writer/parse_4.0.sql parse &>/dev/null
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} restore parse data success${NC}"
}

## 2.2 部署parse server
### 1.2.2 下载parse初始数据
### 2.1.3 安装postgres_service系统服务
function install_parse_server() {
  ### 备份dgiot_parse_server
  if [ -d ${install_dir}/dgiot_parse_server ]; then
    mv ${install_dir}/dgiot_parse_server/ ${backup_dir}/dgiot_parse_server
    chown -R postgres:postgres ${backup_dir}/dgiot_parse_server/
  fi

  ###下载dgiot_parse_server软件
  if [ ! -f ${script_dir}/dgiot_parse_server.tar.gz ]; then
    wget ${fileparseserver}/dgiot_parse_server.tar.gz -O ${script_dir}/dgiot_parse_server.tar.gz &>/dev/null
  fi

  cd ${script_dir}/
  tar xf dgiot_parse_server.tar.gz
  mv ./dgiot_parse_server ${install_dir}/dgiot_parse_server

  ###  配置dgiot_parse_server配置参数
  parseconfig=${install_dir}/dgiot_parse_server/script/.env
  ${csudo} bash -c "echo '# 基础配置'                                                      > ${parseconfig}"
  ${csudo} bash -c "echo 'SERVER_NAME = dgiot_parse_server'                                >> ${parseconfig}"
  ${csudo} bash -c "echo 'SERVER_PORT = 1337'                                              >> ${parseconfig}"
  ${csudo} bash -c "echo 'SERVER_DOMAIN = http://${wlanip}:1337'                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'SERVER_PUBLIC = http://${wlanip}:1337'                          >> ${parseconfig}"
  ${csudo} bash -c "echo  'SERVER_PATH = /parse'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'GRAPHQL_PATH = http://${wlanip}:1337/graphql'                   >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 管理配置'                                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_PATH = /dashboard'                                    >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_USER = ${parse_user}'                                 >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_PASS = ${parse_pwd}'                                  >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_ENCPASS = false'                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 数据配置'                                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'DATABASE = postgres://postgres:${pg_auth}@127.0.0.1:7432/parse'  >> ${parseconfig}"
  ${csudo} bash -c "echo  'REDIS_SOCKET = redis://127.0.0.1:16379/0'                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'REDIS_CACHE = redis://127.0.0.1:16379/1'                        >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 邮箱配置'                                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_FROM_ADDRESS = lsxredrain@163.com'                         >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_FROM_NAME = 系统通知'                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SENDMAIL = true'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_HOST = smtp.163.com'                                  >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_PORT = 465'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_SECURE = true'                                        >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_USER = lsxredrain'                                    >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_PASS = youpasswd'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 接口配置'                                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_APPID = ${parse_appid}'                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_MASTER = ${parse_master}'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_READONLY_MASTER = ${parse_readonly_master}'                   >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_FILE = ${parse_file}'                                         >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_CLIENT = ${parse_client}'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_JAVASCRIPT = ${parse_javascript}'                             >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_RESTAPI = ${parse_restapi}'                                   >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_DOTNET = ${parse_dotnet}'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_WEBHOOK = ${parse_webhook}'                                   >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 会话配置'                                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'SESSION_LENGTH = 604800'                                          >> ${parseconfig}"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} create ${parseconfig} success${NC}"

  cd ${script_dir}/
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} install parse_server success${NC}"
}

function install_dgiot_redis() {
  ### 2.2.3 部署dgiot_redis缓存服务
  cd ${install_dir}/dgiot_parse_server/script/redis/
  make -j${processor} &>/dev/null
  cd ${script_dir}/
}

function deploy_parse_server() {
  clean_service dgiot_parse_server
  #  clean_service dgiot_redis
  install_parse_server
  #install_dgiot_redis
  #echo -e "`date +%F_%T` $LINENO: ${GREEN} install install_dgiot_redis success${NC}"
  parsehome="${install_dir}/dgiot_parse_server"
  #  install_service2 "dgiot_redis" "simple" "${parsehome}/script/redis/src/redis-server ${parsehome}/script/redis.conf"
  install_service2 "dgiot_parse_server" "simple" "${parsehome}/script/node/bin/node  ${parsehome}/server/index.js"

}

#3 部署时序数据服务
##3.1 部署tdengine server
function deploy_tdengine_server() {
  ### 1.2.4 下载TDengine-server
  if [ -f ${install_dir}/taos ]; then
    clean_service "taosd"
    mv ${install_dir}/taos/ ${backup_dir}/taos/
  fi

  if [ ! -f ${script_dir}/TDengine-server-3.0.2.4-Linux-x64.tar.gz ]; then
    wget ${fileserver}/TDengine-server-3.0.2.4-Linux-x64.tar.gz -O ${script_dir}/TDengine-server-3.0.2.4-Linux-x64.tar.gz &>/dev/null
  fi

  cd ${script_dir}/
  if [ -f ${script_dir}/TDengine-server-3.0.2.4 ]; then
    rm -rf ${script_dir}/TDengine-server-3.0.2.4/
  fi

  tar xf TDengine-server-3.0.2.4-Linux-x64.tar.gz
  cd ${script_dir}/TDengine-server-3.0.2.4/
  mkdir ${install_dir}/taos3/log/ -p
  mkdir ${install_dir}/taos3/data/ -p
  echo | /bin/sh remove.sh &>/dev/null
  echo | /bin/sh install.sh &>/dev/null
  ldconfig
  cd ${script_dir}/
  rm ${script_dir}/TDengine-server-3.0.2.4 -rf
  ${csudo} bash -c "echo 'logDir                    ${install_dir}/taos3/log/'   > /etc/taos/taos.cfg"
  ${csudo} bash -c "echo 'dataDir                   ${install_dir}/taos3/data/'   >> /etc/taos/taos.cfg"
  ${csudo} bash -c "echo 'supportVnodes             100'   >> /etc/taos/taos.cfg"
  systemctl start taosd
  systemctl start taosadapter
  systemctl start taoskeeper
  ${csudo} systemctl enable taosadapter &>/dev/null
  ${csudo} systemctl enable taoskeeper &>/dev/null
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} tdengine_server start success${NC}"
  #  install_dgiot_tdengine_mqtt
}

## 3.3 部署dgiot_tdengine_mqtt桥接服务
function install_dgiot_tdengine_mqtt() {
  ## 3.2.setup mosquitto
  ### 1.2.5 下载mosquitto
  if [ ! -f ${script_dir}/mosquitto-1.6.7.tar.gz ]; then
    wget ${fileserver}/mosquitto-1.6.7.tar.gz -O ${script_dir}/mosquitto-1.6.7.tar.gz &>/dev/null
  fi
  if [ -d ${script_dir}/mosquitto-1.6.7/ ]; then
    rm ${script_dir}/mosquitto-1.6.7/ -rf
  fi
  cd ${script_dir}/
  tar xvf mosquitto-1.6.7.tar.gz &>/dev/null
  cd ${script_dir}/mosquitto-1.6.7
  if [ -f /usr/lib/libmosquitto.so.1 ]; then
    rm /usr/lib/libmosquitto.so.1 -rf
  fi

  make -j${processor} &>/dev/null
  make install &>/dev/null
  sudo ln -s /usr/local/lib/libmosquitto.so.1 /usr/lib/libmosquitto.so.1
  ldconfig
  cd ${script_dir}/
  rm ${script_dir}/mosquitto-1.6.7/ -rf
  #wget ${fileserver}/dgiot_tdengine_mqtt -O /usr/sbin/dgiot_tdengine_mqtt &> /dev/null
  #rm ${script_dir}/dgiot_tdengine_mqtt/ -rf
  rm /usr/sbin/dgiot_tdengine_mqtt -rf
  if [ -d ${script_dir}/dgiot_tdengine_mqtt/ ]; then
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} git clone dgiot_tdengine_mqtt ${NC}"
    git clone https://gitee.com/dgiiot/dgiot_tdengine_mqtt.git &>/dev/null
  fi
  cd ${script_dir}/dgiot_tdengine_mqtt/c/
  make &>/dev/null
  cp ${script_dir}/dgiot_tdengine_mqtt/c/dgiot_tdengine_mqtt /usr/sbin/dgiot_tdengine_mqtt
  chmod 777 /usr/sbin/dgiot_tdengine_mqtt
  install_service2 dgiot_tdengine_mqtt "simple" "/usr/sbin/dgiot_tdengine_mqtt 127.0.0.1"
}

#4 安装文件数据服务器
function install_go_fastdfs() {
  if [ ! -f ${script_dir}/go_fastdfs.zip ]; then
    wget ${fileserver}/go_fastdfs.zip -O ${script_dir}/go_fastdfs.zip &>/dev/null
  fi

  if [ -d ${install_dir}/go_fastdfs/ ]; then
    clean_service go_fastdfs
    mv ${install_dir}/go_fastdfs ${backup_dir}/
  fi
  cd ${script_dir}/
  unzip go_fastdfs.zip &>/dev/null
  cd ${script_dir}/go_fastdfs/
  chmod 777 ${script_dir}/go_fastdfs/file
  mv ${script_dir}/go_fastdfs ${install_dir}
  ${csudo} bash -c "sed -i 's!{{ip}}!${lanip}!g'  ${install_dir}/go_fastdfs/cfg.json"
  ${csudo} bash -c "sed -i 's/{{port}}/1250/g'  ${install_dir}/go_fastdfs/cfg.json"
  ${csudo} bash -c "sed -i 's/{{domain_name}}/${lanip}/g'  ${install_dir}/go_fastdfs/cfg.json"

  if [ -f ${install_dir}/go_fastdfs/conf/ ]; then
    rm ${install_dir}/go_fastdfs/conf/ -rf
  fi
  mkdir ${install_dir}/go_fastdfs/conf/ -p
  cp ${install_dir}/go_fastdfs/cfg.json ${install_dir}/go_fastdfs/conf/cfg.json
  go_fastdhome=${install_dir}/go_fastdfs
  install_service1 gofastdfs "simple" "${install_dir}/go_fastdfs/file ${go_fastdhome}" "GO_FASTDFS_DIR=${go_fastdhome}" "${go_fastdhome}"

  if [ ! -d ${install_dir}/go_fastdfs/files/ ]; then
    mkdir -p ${install_dir}/go_fastdfs/files/
  fi

  if [ ! -f ${script_dir}/dgiot_file.tar.gz ]; then
    wget ${fileserver}/dgiot_file.tar.gz -O ${script_dir}/dgiot_file.tar.gz &>/dev/null
  fi
  cd ${script_dir}/
  if [ -d ${script_dir}/dgiot_file/ ]; then
    rm ${script_dir}/dgiot_file/ -rf
  fi
  tar xf dgiot_file.tar.gz &>/dev/null
  mv ${script_dir}/dgiot_file ${install_dir}/go_fastdfs/files/

  if [ ! -f ${script_dir}/dgiot_swagger.tar.gz ]; then
    wget ${fileserver}/dgiot_swagger.tar.gz -O ${script_dir}/dgiot_swagger.tar.gz &>/dev/null
  fi

  if [ -d ${script_dir}/dgiot_swagger/ ]; then
    rm ${script_dir}/dgiot_swagger/ -rf
  fi
  tar xf dgiot_swagger.tar.gz &>/dev/null
  mv ${script_dir}/dgiot_swagger ${install_dir}/go_fastdfs/files/

  if [ ! -f ${script_dir}/dgiot_dashboard.tar.gz ]; then
    wget ${fileserver}/dgiot_dashboard.tar.gz -O ${script_dir}/dgiot_dashboard.tar.gz &>/dev/null
  fi
  cd ${script_dir}/
  if [ -d ${script_dir}/dgiot_dashboard/ ]; then
    rm ${script_dir}/dgiot_dashboard/ -rf
  fi
  tar xf dgiot_dashboard.tar.gz &>/dev/null

  mv ${script_dir}/dgiot_dashboard ${install_dir}/go_fastdfs/files/ &>/dev/null
}

#4 安装文件数据服务器
function install_word_report() {
  clean_service dgiot_report
  if [ ! -f ${script_dir}/dgiot_report.tar.gz ]; then
    wget ${fileserver}/dgiot_report.tar.gz -O ${script_dir}/dgiot_report.tar.gz &>/dev/null
    tar xf dgiot_report.tar.gz &>/dev/null
  fi
  # $1:service  $2:Type  $3:ExecStart  $4:User  $5:Environment  $6:ExecStop
  install_service dgiot_report "simple" "${install_dir}/dgiot_report/bin/startup.sh" "root" "test=1" "${install_dir}/dgiot_report/bin/shutdown.sh"
}

function install_n2n() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} yum install n2n${NC}"
  ${csudo} yum install -y libzstd &>/dev/null
  if [ ! -f ${script_dir}/n2n-3.0.0-1038.x86_64.rpm ]; then
    wget ${fileserver}/n2n-3.0.0-1038.x86_64.rpm -O ${script_dir}/n2n-3.0.0-1038.x86_64.rpm &>/dev/null
  fi
  rpm -ivh --replacepkgs n2n-3.0.0-1038.x86_64.rpm --nodeps --force &>/dev/null
}

function yum_install_git() {
  rm /etc/yum.repos.d/wandisco-git.repo -rf
  cat >/etc/yum.repos.d/wandisco-git.repo <<"EOF"
[wandisco-git]
name=Wandisco GIT Repository
baseurl=http://opensource.wandisco.com/centos/7/git/$basearch/
enabled=1
gpgcheck=1
gpgkey=http://opensource.wandisco.com/RPM-GPG-KEY-WANdisco
EOF
  sudo rpm --import http://opensource.wandisco.com/RPM-GPG-KEY-WANdisco &>/dev/null
  yum remove -y git &>/dev/null
}

function yum_install_jenkins() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} yum_install_jenkins${NC}"
  ${csudo} wget -O /etc/yum.repos.d/jenkins.repo https://pkg.jenkins.io/redhat-stable/jenkins.repo &>/dev/null
  ${csudo} rpm --import https://pkg.jenkins.io/redhat-stable/jenkins.io.key &>/dev/null
  ${csudo} yum install epel-release # repository that provides 'daemonize'
  ${csudo} yum install java-11-openjdk-devel &>/dev/null
  ${csudo} yum install jenkins &>/dev/null
  # https://www.jenkins.io/doc/book/installing/linux/
}

function yum_install_erlang_otp() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} yum_install_erlang_otp${NC}"
  yum install -y make gcc gcc-c++ &>/dev/null
  yum install -y kernel-devel m4 ncurses-devel &>/dev/null
  yum install -y unixODBC unixODBC-devel &>/dev/null
  yum install -y libtool-ltdl libtool-ltdl-devel &>/dev/null
}

#5. 部署应用服务器
# 5.1 安装erlang/otp环境
function install_erlang_otp() {
  yum_install_erlang_otp
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} install_erlang_otp ${otpversion} ${NC}"
  if [ ! -f ${script_dir}/otp_src_${otpversion}.tar.gz ]; then
    wget ${fileserver}/otp_src_${otpversion}.tar.gz -O ${script_dir}/otp_src_${otpversion}.tar.gz &>/dev/null
  fi
  if [ -d ${install_dir}/otp_src_${otpversion}/ ]; then
    rm ${script_dir}/otp_src_${otpversion} -rf
  fi
  cd ${script_dir}/
  tar xf otp_src_${otpversion}.tar.gz &>/dev/null

  cd ${script_dir}/otp_src_${otpversion}/
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} otp configure${NC}"
  ./configure &>/dev/null
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} otp make install${NC}"
  make install &>/dev/null
  cd ${script_dir}/
  rm ${script_dir}/otp_src_${otpversion} -rf
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} install erlang otp success${NC}"
}

function update_dgiot() {
  #  dgiot
  if [ ! -d ${install_dir}/go_fastdfs/files/package/ ]; then
    mkdir -p ${install_dir}/go_fastdfs/files/package/
  fi
  cd ${install_dir}/go_fastdfs/files/package/
  if [ -f ${software}.tar.gz ]; then
    md5=$(md5sum ${software}.tar.gz | cut -d ' ' -f1)
    if [ "${md5}" != "${dgiotmd5}" ]; then
      rm -rf ${software}.tar.gz
    fi
  fi
  if [ ! -f ${software}.tar.gz ]; then
    wget ${fileversionserver}/${software}.tar.gz &>/dev/null
    md51=$(md5sum ${software}.tar.gz | cut -d ' ' -f1)
    if [ "${md51}" != "${dgiotmd5}" ]; then
      echo -e "$(date +%F_%T) $LINENO: ${RED} download ${software} failed${NC}"
      exit 1
    fi
  fi
  tar xf ${software}.tar.gz
  rm ${script_dir}/dgiot/etc/plugins/dgiot_parse.conf -rf
  cp ${install_dir}/dgiot/etc/plugins/dgiot_parse.conf ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_parse.conf
  cp ${install_dir}/dgiot/etc/plugins/dgiot_http.conf ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_http.conf
  cp ${install_dir}/dgiot/etc/emqx.conf ${install_dir}/go_fastdfs/files/package/dgiot/etc/emqx.conf
  if [ -d ${install_dir}/dgiot/ ]; then
    clean_service dgiot
    mv ${install_dir}/dgiot ${backup_dir}/dgiot/
  fi
  mv ${install_dir}/go_fastdfs/files/package/dgiot/ ${install_dir}/

  install_service "dgiot" "forking" "/bin/sh ${install_dir}/dgiot/bin/emqx start" "root" "HOME=${install_dir}/dgiot/erts-12.3.2.2" "/bin/sh /data/dgiot/bin/emqx stop"
}

function update_tdengine_server() {
  version=$(taos --v | awk '{print $2}') || :
  if [ "${version}" != "2.4.0.4" ]; then
    deploy_tdengine_server
  fi
}

function update_dashboard() {
  #  dgiot_dashboard
  cd ${install_dir}/go_fastdfs/files/
  if [ -f dgiot_dashboard.tar.gz ]; then
    dashboardmd5=$(md5sum dgiot_dashboard.tar.gz | cut -d ' ' -f1)
    if [ "${dashboardmd5}" != "bc18331e4f5c4b3480d0806b5ee2d1dc" ]; then
      rm -rf dgiot_dashboard.tar.gz &>/dev/null
    fi
  fi
  if [ ! -f dgiot_dashboard.tar.gz ]; then
    wget ${fileserver}/dgiot_dashboard.tar.gz &>/dev/null
    dashboardmd52=$(md5sum dgiot_dashboard.tar.gz | cut -d ' ' -f1)
    if [ "${dashboardmd52}" != "bc18331e4f5c4b3480d0806b5ee2d1dc" ]; then
      echo -e "$(date +%F_%T) $LINENO: ${RED} download dgiot_dashboard.tar.gz failed${NC}"
      exit 1
    fi
  fi
  if [ -d dgiot_dashboard/ ]; then
    mv dgiot_dashboard/ ${backup_dir}/
  fi
  tar xf dgiot_dashboard.tar.gz &>/dev/null
}

function update_html() {
  #  dgiot_dashboard
  cd ${install_dir}/nginx
  if [ -f ${html_software}.zip ]; then
    htmlmd51=$(md5sum ${html_software}.zip | cut -d ' ' -f1)
    if [ "${htmlmd51}" != "${htmlmd5}" ]; then
      rm -rf ${html_software}.zip &>/dev/null
    fi
  fi
  if [ ! -f ${html_software}.zip ]; then
    wget ${fileserver}/${html_software}.zip &>/dev/null
    htmlmd52=$(md5sum ${html_software}.zip | cut -d ' ' -f1)
    if [ "${htmlmd52}" != "${htmlmd5}" ]; then
      echo -e "$(date +%F_%T) $LINENO: ${RED} download ${html_software}.zip failed${NC}"
      exit 1
    fi
  fi
  if [ -d html/ ]; then
    mv html/ ${backup_dir}/
  fi
  unzip ${html_software}.zip &>/dev/null
  sed -ri '/dgiot_api/d' conf/nginx.conf
}

function atomgit_plugin() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} ${script_dir}/apps/dgiot_atomgit/${NC}"
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} ${script_dir}/apps/dgiot_${dgiotmd5}${NC}"
  cd ${script_dir}/apps/
  rm -rf dgiot_atomgit
  git clone https://atomgit.com/dgiot/dgiot_atomgit.git
  rm -rf dgiot_atomgit/.git
  if [ -d ${script_dir}/apps/dgiot_atomgit/ ]; then
    mv ${script_dir}/apps/dgiot_atomgit ${script_dir}/apps/dgiot_${dgiotmd5}
    for file in `find ${script_dir}/apps/dgiot_${dgiotmd5}/ -type f`; do
      if [[ $file != *HJ212-2017.pdf* ]]; then
        new_name=$(echo "$file" | sed "s/atomgit/${dgiotmd5}/")
        echo -e "$(date +%F_%T) $LINENO: ${GREEN} file= $file${NC}"
        sed -i "s/atomgit/${dgiotmd5}/g" $file
        if  [ ${file} == ${new_name} ]
        then
          echo -e "$(date +%F_%T) $LINENO: ${GREEN} file= ${file}${NC}"
        else
          echo -e "$(date +%F_%T) $LINENO: ${GREEN} new_name= ${new_name}${NC}"
          mv "${file}" "${new_name}"
        fi
      fi
    done
    sed -i "s/atomgit/${dgiotmd5}/g" ${script_dir}/rebar.config.erl
    sed -i "s/atomgit/${dgiotmd5}/g" ${script_dir}/data/loaded_plugins.tmpl
  fi
}

function install_dgiot() {
  make_ssl
  if [ ! -d ${install_dir}/go_fastdfs/files/package/ ]; then
    mkdir -p ${install_dir}/go_fastdfs/files/package/
  fi

  if [ ! -f ${install_dir}/go_fastdfs/files/package/${software}.tar.gz ]; then
    if [ ! -f ${script_dir}/${software}.tar.gz ]; then
      wget ${fileversionserver}/${software}.tar.gz -O ${install_dir}/go_fastdfs/files/package/${software}.tar.gz &>/dev/null
    else
      cp ${script_dir}/${software}.tar.gz ${install_dir}/go_fastdfs/files/package/
    fi
  fi
  cd ${install_dir}/go_fastdfs/files/package/
  tar xf ${software}.tar.gz
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} install_dgiot dgiot_parse${NC}"
  #修改 dgiot_parse 连接 配置
  ${csudo} bash -c "sed -ri '/^parse.parse_server/cparse.parse_server = http://127.0.0.1:1337' ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c "sed -ri '/^parse.parse_path/cparse.parse_path = /parse/'  ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c "sed -ri '/^parse.parse_appid/cparse.parse_appid = ${parse_appid}' ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c "sed -ri '/^parse.parse_master_key/cparse.parse_master_key = ${parse_master}' ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c "sed -ri '/^parse.parse_js_key/cparse.parse_js_key = ${parse_javascript}' ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c "sed -ri '/^parse.parse_rest_key/cparse.parse_rest_key = ${parse_restapi}' ${install_dir}/go_fastdfs/files/package/dgiot/etc/plugins/dgiot_parse.conf"

  echo -e "$(date +%F_%T) $LINENO: ${GREEN} install_dgiot dgiot${NC}"

  if [ -d ${install_dir}/dgiot/ ]; then
    clean_service dgiot
    mv ${install_dir}/dgiot ${backup_dir}/dgiot/
  fi
  mv ${install_dir}/go_fastdfs/files/package/dgiot ${install_dir}/

  install_service "dgiot" "forking" "/bin/sh ${install_dir}/dgiot/bin/emqx start" "root" "HOME=${install_dir}/dgiot/erts-12.3.2.2" "/bin/sh /data/dgiot/bin/emqx stop"
}

#6 运维监控
# 6.1 安装node_exporter-0.18.1.linux-amd64.tar.gz
function install_node_exporter() {
  if [ ! -f ${script_dir}/node_exporter-0.18.1.linux-amd64.tar.gz ]; then
    wget ${fileserver}/node_exporter-0.18.1.linux-amd64.tar.gz -O ${script_dir}/node_exporter-0.18.1.linux-amd64.tar.gz &>/dev/null
  fi

  if [ -d ${install_dir}/node_exporter-0.18.1.linux-amd64/ ]; then
    clean_service node_exporter
    mv ${install_dir}/node_exporter-0.18.1.linux-amd64/ ${backup_dir}/
  fi

  tar -zxvf node_exporter-0.18.1.linux-amd64.tar.gz &>/dev/null
  mv ${script_dir}/node_exporter-0.18.1.linux-amd64/ ${install_dir}/
  install_service2 node_exporter "simple" "${install_dir}/node_exporter-0.18.1.linux-amd64/node_exporter"
  cd ${script_dir}
}

##6.2 install postgres exporter
function install_postgres_exporter() {
  if [ ! -f ${script_dir}/postgres_exporter-0.10.0.linux-amd64.tar.gz ]; then
    wget ${fileserver}/postgres_exporter-0.10.0.linux-amd64.tar.gz -O ${script_dir}/postgres_exporter-0.10.0.linux-amd64.tar.gz &>/dev/null
  fi

  retval=$(sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "SELECT * FROM pg_available_extensions;" &>/dev/null)
  if [[ $retval == *"pg_stat_statements"* ]]; then
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} pg_stat_statements has installed${NC}"
  else
    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "CREATE EXTENSION pg_stat_statements SCHEMA public;" &>/dev/null
    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "CREATE USER postgres_exporter PASSWORD '${pg_pwd}';" &>/dev/null
    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "ALTER USER postgres_exporter SET SEARCH_PATH TO postgres_exporter,pg_catalog;" &>/dev/null
    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "CREATE SCHEMA postgres_exporter AUTHORIZATION postgres_exporter;" &>/dev/null
    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "CREATE FUNCTION postgres_exporter.f_select_pg_stat_activity()
    RETURNS setof pg_catalog.pg_stat_activity
    LANGUAGE sql
    SECURITY DEFINER
    AS \$\$
    SELECT * from pg_catalog.pg_stat_activity;
    \$\$;" &>/dev/null

    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "CREATE FUNCTION postgres_exporter.f_select_pg_stat_replication()
    RETURNS setof pg_catalog.pg_stat_replication
    LANGUAGE sql
    SECURITY DEFINER
    AS \$\$
      SELECT * from pg_catalog.pg_stat_replication;
    \$\$;" &>/dev/null

    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "CREATE VIEW postgres_exporter.pg_stat_replication
    AS
      SELECT * FROM postgres_exporter.f_select_pg_stat_replication();" &>/dev/null

    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "CREATE VIEW postgres_exporter.pg_stat_activity
    AS
      SELECT * FROM postgres_exporter.f_select_pg_stat_activity();" &>/dev/null

    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "GRANT SELECT ON postgres_exporter.pg_stat_replication TO postgres_exporter;" &>/dev/null
    sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "GRANT SELECT ON postgres_exporter.pg_stat_activity TO postgres_exporter;" &>/dev/null
  fi

  systemctl restart dgiot_pg_writer

  if [ -d ${install_dir}/postgres_exporter-0.10.0.linux-amd64/ ]; then
    clean_service postgres_exporter
    mv ${install_dir}/postgres_exporter-0.10.0.linux-amd64/ ${backup_dir}/
  fi

  tar xvf postgres_exporter-0.10.0.linux-amd64.tar.gz &>/dev/null
  mv ${script_dir}/postgres_exporter-0.10.0.linux-amd64/ ${install_dir}/
  export DATA_SOURCE_NAME=postgresql://postgres:postgres@127.0.0.1:7432/parse?sslmode=disable
  install_service2 postgres_exporter "simple" "${install_dir}/postgres_exporter --extend.query-path='${install_dir}/queries.yaml'"
}

##6.3 安装prometheus-2.17.1.linux-amd64.tar.gz
function install_prometheus() {
  if [ ! -f ${script_dir}/prometheus-2.17.1.linux-amd64.tar.gz ]; then
    wget $fileserver/prometheus-2.17.1.linux-amd64.tar.gz -O ${script_dir}/prometheus-2.17.1.linux-amd64.tar.gz &>/dev/null
  fi

  cd ${script_dir}/
  tar -zxvf prometheus-2.17.1.linux-amd64.tar.gz &>/dev/null

  if [ -d ${install_dir}/prometheus-2.17.1.linux-amd64/ ]; then
    clean_service pushgateway
    clean_service prometheus
    mv ${install_dir}/prometheus-2.17.1.linux-amd64/ ${backup_dir}/prometheus-2.17.1.linux-amd64/
  fi

  mv ${script_dir}/prometheus-2.17.1.linux-amd64/ ${install_dir}/
  install_service2 pushgateway "simple" "${install_dir}/prometheus-2.17.1.linux-amd64/pushgateway"
  install_service2 prometheus "simple" "${install_dir}/prometheus-2.17.1.linux-amd64/prometheus \
    --config.file=${install_dir}/prometheus-2.17.1.linux-amd64/prometheus.yml \
    --storage.tsdb.path=${install_dir}/prometheus-2.17.1.linux-amd64/data"
}

function yum_install_grafana() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} yum_install_grafana${NC}"
  yum install -y libXcomposite libXdamage libXtst cups libXScrnSaver &>/dev/null
  yum install -y pango atk adwaita-cursor-theme adwaita-icon-theme &>/dev/null
  yum install -y at at-spi2-atk at-spi2-core cairo-gobject colord-libs &>/dev/null
  yum install -y dconf desktop-file-utils ed emacs-filesystem gdk-pixbuf2 &>/dev/null
  yum install -y glib-networking gnutls gsettings-desktop-schemas &>/dev/null
  yum install -y gtk-update-icon-cache gtk3 hicolor-icon-theme jasper-libs &>/dev/null
  yum install -y json-glib libappindicator-gtk3 libdbusmenu libdbusmenu-gtk3 libepoxy &>/dev/null
  yum install -y liberation-fonts liberation-narrow-fonts liberation-sans-fonts liberation-serif-fonts &>/dev/null
  yum install -y libgusb libindicator-gtk3 libmodman libproxy libsoup libwayland-cursor libwayland-egl &>/dev/null
  yum install -y libxkbcommon m4 mailx nettle patch psmisc redhat-lsb-core redhat-lsb-submod-security &>/dev/null
  yum install -y rest spax time trousers xdg-utils xkeyboard-config alsa-lib &>/dev/null
}
###6.3.4 安装grafana-7.1.1.tar.gz
function install_grafana() {
  ## 部署图片报表插件
  yum_install_grafana
  if [ ! -f ${script_dir}/grafana-7.1.1.tar.gz ]; then
    wget ${fileserver}/grafana-7.1.1.tar.gz -O ${script_dir}/grafana-7.1.1.tar.gz &>/dev/null
  fi

  if [ -d ${install_dir}/grafana-7.1.1/ ]; then
    clean_service grafana-server
    mv ${install_dir}/grafana-7.1.1/ ${backup_dir}/grafana-7.1.1/
  fi
  cd ${script_dir}/
  tar xvf grafana-7.1.1.tar.gz &>/dev/null

  mv ${script_dir}/grafana-7.1.1/ ${install_dir}/
  grafanahome=${install_dir}/grafana-7.1.1
  ${csudo} bash -c "sed -ri '/^plugins/cplugins = ${install_dir}/grafana-7.1.1/data/plugins/'  ${install_dir}/grafana-7.1.1/conf/defaults.ini"
  install_service2 grafana-server "simple" "${grafanahome}/bin/grafana-server --config=${grafanahome}/conf/defaults.ini  --homepath=${grafanahome}"
}

function build_nginx() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} build_nginx ${NC}"
  clean_service nginx
  if systemctl is-active --quiet nginx; then
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} nginx is running, stopping it...${NC}"
    rpm -e nginx
  fi
  ### 创建目录和用户,以及配置环境变化
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} create nginx user and group ${NC}" &>/dev/null
  set +uxe
  egrep "^nginx" /etc/passwd >/dev/null
  if [ $? -eq 0 ]; then
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} nginx user and group exist ${NC}" &>/dev/null
  else
    ${csudo} groupadd nginx &>/dev/null
    ${csudo} useradd -g nginx nginx &>/dev/null
  fi
  ### install nginx
  yum -y install pcre-devel &>/dev/null
  yum install openssl-devel &>/dev/null
  if [ ! -f ${script_dir}/nginx-1.25.1.zip ]; then
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/nginx-1.25.1.zip -O ${script_dir}/nginx-1.25.1.zip &>/dev/null
  fi
  cd ${script_dir}/
  unzip nginx-1.25.1.zip &>/dev/null
  cd nginx-1.25.1
  ./configure --prefix=/data/dgiot/nginx --with-http_realip_module --with-http_ssl_module --with-http_gzip_static_module --with-stream &>/dev/null
  make &>/dev/null
  make install &>/dev/null
  if [ ! -f ${script_dir}/ssl_cert.zip ]; then
    wget ${fileserver}/ssl_cert.zip -O ${script_dir}/ssl_cert.zip &>/dev/null
  fi
  if [ -f ${script_dir}/ssl_cert.zip ]; then
    unzip -o ${script_dir}/ssl_cert.zip -d /etc/ssl/certs/ &>/dev/null
  fi
  #dashboard
  if [ ! -f ${script_dir}/${html_software}.zip ]; then
    wget ${fileserver}/${html_software}.zip -O ${script_dir}/${html_software}.zip &>/dev/null
  fi
  cd ${script_dir}/
  rm -rf /data/dgiot/nginx/html &>/dev/null
  unzip -d /data/dgiot/nginx/ ${html_software}.zip &>/dev/null

  install_service2 "nginx" "forking" "/data/dgiot/nginx/sbin/nginx"
}

function make_ssl() {
  if [ ! -d /etc/ssl/dgiot/ ]; then
    mkdir -p /etc/ssl/dgiot/
    cd /etc/ssl/dgiot/

    # 生成自签名的CA key和证书
    openssl genrsa -out ca.key 2048 &>/dev/null
    openssl req -x509 -new -nodes -key ca.key -sha256 -days 3650 -subj "/CN=${wlanip}" -out ca.pem &>/dev/null

    # 生成服务器端的key和证书
    openssl genrsa -out server.key 2048 &>/dev/null
    openssl req -new -key ./server.key -out server.csr -subj "/CN=0.0.0.0" &>/dev/null
    openssl x509 -req -in ./server.csr -CA ca.pem -CAkey ca.key -CAcreateserial -out server.pem -days 3650 -sha256 &>/dev/null

    # 生成客户端key和证书
    openssl genrsa -out client.key 2048 &>/dev/null
    openssl req -new -key ./client.key -out client.csr -subj "/CN=0.0.0.0" &>/dev/null
    openssl x509 -req -in ./client.csr -CA ca.pem -CAkey ca.key -CAcreateserial -out client.pem -days 3650 -sha256 &>/dev/null

    cd ${script_dir}/ &>/dev/null
  fi
}

function install_node() {
  if [ ! -d ${script_dir}/node-v16.15.1-linux-x64/bin/ ]; then
      if [ ! -f ${script_dir}/node-v16.15.1-linux-x64.tar.xz ]; then
        wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/node-v16.15.1-linux-x64.tar.xz &>/dev/null
        tar xvf node-v16.15.1-linux-x64.tar.xz &>/dev/null
        if [ ! -f usr/bin/node ]; then
          rm /usr/bin/node -rf
        fi
        rm /usr/bin/npm -rf
        rm /usr/bin/node -rf
        rm /usr/bin/yarn -rf
        ln -s ${script_dir}/node-v16.15.1-linux-x64/bin/node /usr/bin/node
        ln -s ${script_dir}/node-v16.15.1-linux-x64/bin/yarn /usr/bin/yarn
        sudo /bin/dd if=/dev/zero of=/var/swap.1 bs=1M count=1024
        sudo /sbin/mkswap /var/swap.1
        sudo /sbin/swapon /var/swap.1
      fi
    fi
  }

function build_iotView() {
  cd ${script_dir}/
  if [ ! -d ${script_dir}/iotView/ ]; then
    git clone -b master https://gitee.com/dgiiot/iotView.git.git iotView
  fi

  cd ${script_dir}/iotView
  cd ${script_dir}/iotView
  git reset --hard
  git pull

  export PATH=$PATH:/usr/local/bin:${script_dir}/node-v16.15.1-linux-x64/bin/
  rm ${script_dir}/iotView/dist/ -rf
  ${script_dir}/node-v16.15.1-linux-x64/bin/npm install
  ${script_dir}/node-v16.15.1-linux-x64/bin/npm run build:prod
  echo "build_iotView "
}

function build_iotEdit() {
  cd ${script_dir}/
  if [ ! -d ${script_dir}/iotEdit/ ]; then
    git clone -b master https://gitee.com/dgiiot/iotEdit.git.git iotEdit
  fi

  cd ${script_dir}/iotEdit
  git reset --hard
  git pull

  export PATH=$PATH:/usr/local/bin:${script_dir}/node-v16.15.1-linux-x64/bin/
  rm ${script_dir}/iotEdit/dist/ -rf
  ${script_dir}/node-v16.15.1-linux-x64/bin/yarn install
   ${script_dir}/node-v16.15.1-linux-x64/bin/yarn build
  echo "build_iotEdit"
}

function pre_build_dgiot() {
  ## 关闭dgiot
  clean_service dgiot
  count=$(ps -ef | grep beam.smp | grep -v "grep" | wc -l)
  if [ 0 == $count ]; then
    echo $count
  else
    killall -9 beam.smp
  fi

  cd ${script_dir}/

  if [ ! -d ${script_dir}/$plugin/ ]; then
    git clone https://gitee.com/dgiiot/dgiot.git $plugin
  fi

  cd ${script_dir}/$plugin/
  git reset --hard
  git pull

  if [ $plugin == 'dgiot' ]; then
    echo "dgiot plugin"
  else
    cd ${script_dir}/$plugin/apps/
    rm $plugin -rf
    git clone root@git.iotn2n.com:dgiot/$plugin.git
  fi

  cd ${script_dir}
  rm ${script_dir}/$plugin/_build/emqx/rel/ -rf

  if [ ! -d ${script_dir}/iotView/dist ]; then
      build_iotView
  fi

  if [ -d ${script_dir}/iotView/dist ]; then
      rm ${script_dir}/$plugin/_build/emqx/lib/dgiot_api -rf
      rm ${script_dir}/$plugin/apps/dgiot_api/priv/www/ -rf
      cp ${script_dir}/iotView/dist/ ${script_dir}/$plugin/apps/dgiot_api/priv/www/ -rf
  fi

  if [ ! -d ${script_dir}/iotEdit/dist ]; then
      build_iotEdit
  fi

  if [ -d ${script_dir}/iotEdit/dist ]; then
    cp ${script_dir}/iotEdit/dist/ ${script_dir}/$plugin/apps/dgiot_api/priv/www/admin -rf
  fi

  if [ -d ${script_dir}/dgiot/emqx/rel/ ]; then
    rm ${script_dir}/dgiot/emqx/rel -rf
  fi

  rm ${script_dir}/$plugin/rebar.config -rf
  cp ${script_dir}/$plugin/apps/$plugin/conf/rebar.config ${script_dir}/$plugin/rebar.config -rf
  rm ${script_dir}/$plugin/rebar.config.erl -rf
  cp ${script_dir}/$plugin/apps/$plugin/conf/rebar.config.erl ${script_dir}/$plugin/rebar.config.erl -rf
  rm ${script_dir}/$plugin/data/loaded_plugins.tmpl -rf
  cp ${script_dir}/$plugin/apps/$plugin/conf/loaded_plugins.tmpl ${script_dir}/$plugin/data/loaded_plugins.tmpl
  rm ${script_dir}/$plugin/apps/dgiot_parse/etc/dgiot_parse.conf -rf
  cp ${script_dir}/$plugin/apps/$plugin/conf/dgiot_parse.conf ${script_dir}/$plugin/apps/dgiot_parse/etc/dgiot_parse.conf -rf

  rm ${script_dir}/$plugin/apps/dgiot_http/etc/dgiot_http.conf -rf
  cp ${script_dir}/$plugin/apps/$plugin/conf/dgiot_http.conf ${script_dir}/$plugin/apps/dgiot_http/etc/dgiot_http.conf -rf

  if [ -f ${script_dir}/$plugin/apps/$plugin/conf/other.sh ]; then
    cd ${script_dir}/$plugin/apps/
    echo | /bin/sh ${script_dir}/$plugin/apps/$plugin/conf/other.sh &>/dev/null
  fi

  cd ${script_dir}/$plugin/
}

function post_build_dgiot() {
  mv ${script_dir}/$plugin/_build/emqx/rel/emqx/ ${script_dir}/$plugin/_build/emqx/rel/dgiot
  cd ${script_dir}/$plugin/_build/emqx/rel

  tar czf ${software}.tar.gz ./dgiot
  rm ./dgiot -rf
  if [ ! -d ${install_dir}/go_fastdfs/files/package/ ]; then
    mkdir -p ${install_dir}/go_fastdfs/files/package/
  fi
  cp ./${software}.tar.gz ${install_dir}/go_fastdfs/files/package/
}

function build_keepalived() {
  cd ~
  wget https://www.keepalived.org/software/keepalived-2.0.20.tar.gz
  yum install -y gcc openssl-devel popt-devel ipvsadm
  tar -zxvf keepalived-2.0.20.tar.gz
  mv keepalived-2.0.20 /usr/
  cd /usr/keepalived-2.0.20
  ./configure --prefix=/usr/keepalived-2.0.20
  make && make install
  chmod 644 keepalived.conf
  chmod +x /usr/keepalived-2.0.20/container_check.sh
  systemctl enable keepalived.service
  systemctl start keepalived.service
}

function install_python() {
  yum -y groupinstall "Development tools"
  yum -y install zlib-devel bzip2-devel openssl-devel ncurses-devel sqlite-devel readline-devel tk-devel gdbm-devel db4-devel libpcap-devel xz-devel
  yum install libffi-devel -y
  wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/Python-3.7.1.tar.xz
  tar -xvf Python-3.7.1.tar.xz
  cd Python-3.7.1/
  ./configure --prefix=/usr/local/python3 && make && make install
  ln -s /usr/local/python3/bin/python3 /usr/bin/python3
  ln -s /usr/local/python3/bin/pip3 /usr/bin/pip3
  pip3 install --upgrade pip
}

function install_selenium() {
  wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/install-google-chrome.sh
  sh ./install-google-chrome.sh
  pip3 install selenium
  wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/chromedriver_linux64.zip
  unzip chromedriver_linux64.zip
  rm /usr/bin/chromedriver -rf
  mv chromedriver /usr/bin/
}

function install_dotnet() {
  sudo rpm -Uvh https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/packages-microsoft-prod.rpm
  sudo yum install -y dotnet-sdk-5.0
}

function install_ffmpeg() {
  sudo yum install yasm
  wget http://www.ffmpeg.org/releases/ffmpeg-4.1.tar.gz
  tar -zxvf ffmpeg-4.1.tar.gz
  cd ffmpeg-4.1
  ./configure --prefix=/usr/local/ffmpeg
  make && make install
  sudo ln -s /usr/local/ffmpeg/bin/ffmpeg /usr/bin/ffmpeg
}

## ==============================begin deploy type ============================
# 计划基于msys64来构建 类linux 的windows一键式部署和开发环境, 只支持单机版本
function windows() {
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} dgiot ${verType} windos deploy start${NC}"
  set +uxe
  # initdb -D /data/dgiot/dgiot_pg_writer/data/
  # pg_ctl -D /data/dgiot/dgiot_pg_writer/data/ -l logfile start
  # pg_ctl.exe register -N pgsql -D /data/dgiot/dgiot_pg_writer/data/
  # net stop pgsql
  # net start pgsql
  /usr/local/lib/erlang/install
  cd /data/bin/
  wget ${fileserver}/parse_4.0.sql.tar.gz -O /data/bin/parse_4.0.sql.tar.gz &>/dev/null
  tar xvf parse_4.0.sql.tar.gz &>/dev/null
  ./dgiot_hub.exe start
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} install parse_server success${NC}"
}

function centos() {
  # Install app and data node
  if [ -x ${install_dir}/dgiot ]; then
    update_dgiot
    update_dashboard
    update_html
    #update_tdengine_server
    #restore_parse_data       # 加载默认档案数据
  else
    pre_install
    clean_services
    deploy_postgres
    restore_parse_data     # 档案数据
    deploy_tdengine_server # 时序数据
    install_go_fastdfs     # 文件数据
    #install_word_report    # 报告服务
    deploy_parse_server # Api网关
    #install_erlang_otp
    install_dgiot
#    install_n2n
    build_nginx
    #install_node_exporter
  fi
}

function single() {
  pg_eip=${lanip}
  pg_auth=${pg_pwd}
  ## windows单机版本部署(待完成)
  if [ "${os_type}" == 3 ]; then
    windows
  else ## linux单机版本目前只支持centos 7.6/7.9
    centos
  fi
}

function cluster() {
  #如果数据节点ip和本机ip相同，则部署数据节点
  if [ ${pg_eip} == "changeyourip" ]; then
    pg_eip=${lanip}
    pg_auth=${pg_pwd}
    cluster_data
  else
    cluster_app
  fi
}

## dgiot集群部署包含三类节点：
# 1、数据存储节点(data),可以通过eip给数据消费节点和数据生产节点提供数据读写功能
# 2、数据消费节点(consume),可以通过eip给外部用户提供高可用的数据消费,对设备数据只能读，不能写
# 3、数据生产节点(product)，可以从内部子系统或者设备的数据源获取数据，并通过eip存储到数据节点
# 部署顺序是首先安装数据存储节点，然后再部署数据生产节点和数据消费节点
# 部署数据生产节点和数据消费节点时需要输入数据存储节点的eip地址
### ----begin  dgiot cluster ----- ##
##---- User layer -----------------##
##       |eip|                     ##
##----- dgiot consume node --------##
##       |eip|                     ##
##----- dgiot data node -----------##
##       |eip|                     ##
##----- dgiot product node --------##
##     |c1| |c2|  |..|   |cn|      ##
##---- Data source layer ----------##

function cluster_data() {
  #数据存储节点部署
  if [ -x ${install_dir}/dgiot ]; then
    build_nginx
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} please update dgiot ${verType} node by youself${NC}"
  else
    pre_install
    deploy_postgres           # 配置数据
    restore_parse_data        # 配置数据
    deploy_tdengine_server    # 时序数据
    install_go_fastdfs        # 文件数据
    install_prometheus        # 运维统计服务
    install_grafana           # 运维可视化服务
    install_postgres_exporter # postgres数据运维监视
    install_node_exporter     # 服务器系统数据运维监视
  fi
}

function cluster_app() {
  #数据生产节点部署
  if [ -x ${install_dir}/dgiot ]; then
    update_dgiot
    update_dashboard
    update_tdengine_server
  else
    pre_install
    deploy_parse_server # Api网关
    install_erlang_otp
    install_dgiot
    build_nginx
    install_node_exporter
  fi
}

### ----end  dgiot cluster -------

function devops() {
  #一键式开发环境
  pre_install
  install_node
  install_erlang_otp
  pre_build_dgiot
  make
  post_build_dgiot
}

function ci() {
  #一键式持续集成
  install_node
  install_erlang_otp
  pre_build_dgiot
  make ci
  post_build_dgiot
}
## ==============================end deploy type ============================
function deploy_dgiot() {
  set -e
  echo -e "$(date +%F_%T) $LINENO: ${GREEN} dgiot ${deployType} deploy start${NC}"
  # centos单机版本部署(已完成)
  if [ "${deployType}" == "single" ]; then
    single
  # cluster step 1 数据存储节点部署 (待完成)
  elif [ "${deployType}" == "cluster" ]; then
    cluster
  #  本地开发环境部署 (已完成)
  elif [ "${deployType}" == "devops" ]; then
    devops
  #  持续集成环境部署 (已完成)
  elif [ "${deployType}" == "ci" ]; then
    ci
  elif [ "${deployType}" == "otp" ]; then
    install_erlang_otp
  #  持续集成环境部署 (已完成)
  elif [ "${deployType}" == "atomgit" ]; then
    atomgit_plugin
  else
    echo "please input correct deployType"
  fi

  echo -e "$(date +%F_%T) $LINENO: ${GREEN} dgiot ${deployType} deploy success end${NC}"
}

## ==============================Main program starts from here============================
if [ "$1" == "-h" ]; then
  help
fi

###---------------------------- prepare config-------------------------------------------------
## 当前支持cetos 7.6/7.9, 计划支持windows
check_os_type
## shell parameter init
dgiot_shell

# =============================  get input parameters =================================================
# dgiot_install.sh -v [single | cluster | devops | otp | ci | atomgit] -s [dgiot_n] -p [dgiot_your_plugin] -m [dgiotmd5 | atomgit] -e [datanode_eip] -s [pg_auth]
# set parameters by default value
deployType=single                           # [single | cluster | devops | otp|  ci | atomgit]
plugin="dgiot"                              # [dgiot | dgiot_your_plugin]
software="dgiot_b40"                        # [dgiot_b20| dgiot_n]
dgiotmd5="c0a54cea57298c688628f70baa41cc16" # [dgiotmd5]
pg_eip="changeyourip"                       # [datanode_eip]
pg_auth='changeyourpassword'                # [pg_auth]
islanip="false"                             # [islanip]
html_software="dgiot_html_4.8.7"            # [dgiot_html_4.8.2| dgiot_html_n]
htmlmd5="c2f6f842363c29852b43da42373edb73"  # [htmlmd5]
atomplugin="atomgit"                        # [atomgit | jt808]

while getopts "v:s:p:m:d:e:a:n" arg; do
  case $arg in
  v)
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} deployType=$OPTARG${NC}"
    deployType=$(echo $OPTARG)
    ;;
  s)
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} software=$OPTARG${NC}"
    software=$(echo $OPTARG)
    ;;
  m)
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} dgiotmd5=$OPTARG${NC}"
    dgiotmd5=$(echo $OPTARG)
    ;;
  p)
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} plugin=$OPTARG${NC}"
    plugin=$(echo $OPTARG)
    ps
    ;;
  e)
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} pg_eip=$OPTARG${NC}"
    pg_eip=$(echo $OPTARG)
    ;;
  a)
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} pg_auth=$OPTARG${NC}"
    pg_auth=$(echo $OPTARG)
    ;;
  n)
    echo -e "$(date +%F_%T) $LINENO: ${GREEN} islanip=$OPTARG${NC}"
    islanip=$(echo $OPTARG)
    ;;
  ?) #unknow option
    help
    ;;
  esac
done

## 不需要人机交互的参数初始化
dgiot_auto_variables

###---------------------------- install dgiot-------------------------------------------------
deploy_dgiot

###---------------------------- install dgiot-------------------------------------------------
echo -e "$(date +%F_%T) $LINENO: ${BLUE} database login info${NC}"
echo -e "$(date +%F_%T) $LINENO: ${BLUE} pg_info: postgres://postgres:${pg_auth}@${pg_eip}:7432/parse${NC}"
echo -e "$(date +%F_%T) $LINENO: ${BLUE} pg_info: pg_eip=${pg_eip} pg_auth=${pg_auth}${NC}"
echo -e "$(date +%F_%T) $LINENO: ${BLUE} parse_info: http://${wlanip}:1337/dashboard/# ${NC}"
echo -e "$(date +%F_%T) $LINENO: ${BLUE} parse_info: username=${parse_user} password=${parse_pwd} ${NC}"
echo -e "$(date +%F_%T) $LINENO: ${BLUE} dashboard: http://${wlanip}/# ${NC}"
echo -e "$(date +%F_%T) $LINENO: ${BLUE} dashboard: username=dgiot_dev password=dgiot_dev ${NC}"
echo -e "$(date +%F_%T) $LINENO: ${BLUE} software: ${software}  ${html_software} ${NC}"
