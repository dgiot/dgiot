#!/bin/bash

#1. 保存环境变量
export PATH=$PATH:/usr/local/bin
workdir=`pwd`
echo  $workdir

wget https://dl.grafana.com/oss/release/grafana-6.7.0-1.x86_64.rpm
sudo yum install grafana-6.7.0-1.x86_64.rpm

 [Unit]
 Description=Prometheus Node Exporter
 Wants=network-online.target
 After=network-online.target

 [Service]
 User=node_exporter
 Group=node_exporter
 Type=simple

 ExecStart=/opt/node_exporter/node_exporter --web.listen-address=0.0.0.0:9100 --log.level=info --log.format=json --collector.logind --collector.systemd --collector.tcpstat --collector.ntp --collector.interrupts  --collector.meminfo_numa --collector.processes --no-collector.bonding --no-collector.bcache --no-collector.arp --no-collector.edac --no-collector.infiniband --no-collector.ipvs --no-collector.mdadm --no-collector.nfs --no-collector.nfsd --no-collector.textfile --no-collector.wifi --no-collector.hwmon --no -collector.conntrack --no-collector.timex --no-collector.zfs --collector.systemd.unit-whitelist=(crond|sshd|node_exporter)\\.service

 ExecReload=/bin/kill -HUP $MAINPID
 TimeoutStopSec=10s
 SendSIGKILL=no
 SyslogIdentifier=prometheus_node_exporter
 Restart=always

 [Install]
 WantedBy=multi-user.target