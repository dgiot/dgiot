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
LABEL Component="dgiot_dashboard" \
      Name="dgiot_dashboard/centos7" \
      Version="4.7.0" \
      Release="1"

# Labels could be consumed by OpenShift
LABEL io.k8s.description="dgiot_dashboard Open source platform for IoT,30 min Quick Deployment,10M devices connection,Carrier level Stability." \
      io.k8s.display-name="dgiot_dashboard 4.7.0" \
      io.openshift.expose-services="80:http" \
      io.openshift.tags="dgiot_dashboard"

#VOLUME [ "/data/dgiot/"]

ARG dockerserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/docker"
ARG fileserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"

ADD data /
ADD data/service /data/
ADD bin/entrypoint.sh /usr/bin/
ADD ${dockerserver}/nginx.tar.gz  /data/dgiot/
ADD ${dockerserver}/nginx.conf  /data/dgiot/
ADD ${dockerserver}/dgiot_dashboard.tar.gz  /data/dgiot/

RUN yum -y install unzip && \
    yum -y install wget && \

    cd  /data/dgiot/ && \

#   nginx
    groupadd nginx  && \
    useradd -g nginx nginx  && \
    tar xvf ./nginx.tar.gz  && \
    rm ./nginx.tar.gz -rf   && \
    rm  ./nginx/conf/nginx.conf -rf  && \
    mv ./nginx.conf ./nginx/conf/   && \

#   dgiot_dashboard
    tar xvf ./dgiot_dashboard.tar.gz  && \
    rm ./dgiot_dashboard.tar.gz -rf   && \
    rm -rf /data/dgiot/nginx/html && \
    mv ./html /data/dgiot/nginx/  && \

#   service
    mv /data/*.service  /lib/systemd/system/  && \

    yum clean all

#CMD ["docker run -itd --privileged -p 80:8080 --hostname dgiot dgiot/dgiot:4.7.0 init"]

ENTRYPOINT ["/usr/bin/entrypoint.sh"]

