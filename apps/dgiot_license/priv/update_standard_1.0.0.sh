#!/bin/bash

#1. 保存环境变量
export PATH=$PATH:/usr/local/bin
workdir=`pwd`
#7. 部署dgiot_iot
cd /data
randtime=`date +%F_%T`
echo $randtime

if [ -d /data/{{dgiot_iot}} ]; then
   mv /data/{{dgiot_iot}}/ /data/{{dgiot_iot}}_bk_$randtime
fi

if [ ! -f  /data/{{dgiot_iot_software}} ]; then
  wget http://www.iotn2n.com/package/{{dgiot_iot_software}} -O /data/{{dgiot_iot_software}}
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
    psql -U postgres -d parse -c " alter table \"Product\" add primary key(name,\"devType\");"
    psql -U postgres -d parse -c "alter table \"Device\" drop constraint \"Device_pkey\";"
    psql -U postgres -d parse -c "alter table \"Device\" add primary key(product,devaddr);"
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

systemctl restart {{td_bridge_name}}
systemctl restart dgiot_demon

