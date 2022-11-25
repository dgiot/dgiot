##--------------------------------------------------------------------
## Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##--------------------------------------------------------------------

FROM centos:centos7
MAINTAINER The CentOS Project <cloud-ops@centos.org>

# Labels consumed by Red Hat build service
LABEL Component="dgiot_edge" \
      Name="dgiot_edge/centos7" \
      Version="4.7.0" \
      Release="1"

# Labels could be consumed by OpenShift
LABEL io.k8s.description="dgiot_edge Open source platform for IoT,30 min Quick Deployment,10M devices connection,Carrier level Stability." \
      io.k8s.display-name="dgiot_edge 4.7.1" \
      io.openshift.expose-services="5080:http" \
      io.openshift.tags="dgiot_edge"

#VOLUME [ "/data/dgiot/"]

ARG dockerserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/docker"
ARG dgiot="dgiot_edge_n7"

ADD data /
ADD data/service /data/
ADD bin/entrypoint.sh /usr/bin/
ADD ${dockerserver}/erlang-23.3.4.11-1.el7.x86_64.rpm /data/dgiot/

ADD ${dockerserver}/${dgiot}.tar.gz /data/dgiot/

RUN yum -y install openssl && \

#    dgiot
    cd  /data/dgiot && \
    tar xvf ./${dgiot}.tar.gz  && \
    rm ./${dgiot}.tar.gz -rf   && \

#   rpm erlang
    rpm -i ./erlang-23.3.4.11-1.el7.x86_64.rpm  && \
    rm ./erlang-23.3.4.11-1.el7.x86_64.rpm -rf  && \

#   service
    mv /data/*.service  /lib/systemd/system/  && \

    yum clean all

#CMD ["docker run -itd --privileged -p 80:8080 --hostname dgiot dgiot/dgiot:4.7.0 init"]

ENTRYPOINT ["/usr/bin/entrypoint.sh"]

