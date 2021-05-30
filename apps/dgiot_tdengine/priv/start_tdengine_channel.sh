#!/bin/bash
export PATH=$PATH:/usr/local/bin
workdir=`pwd`
echo  $workdir

systemctl stop {{dgiot_td_channel}}

rm /usr/lib/systemd/system/{{dgiot_td_channel}}.service  -rf
cat > /lib/systemd/system/{{dgiot_td_channel}}.service << "EOF"
[Unit]
Description={{dgiot_td_channel}}_service

[Service]
Type=simple
ExecStart=/usr/sbin/dgiot_td {{host}} {{channelid}} {{app}}
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
systemctl enable {{dgiot_td_channel}}
systemctl start {{dgiot_td_channel}}



