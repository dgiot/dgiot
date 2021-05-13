FROM erlang:22.3

# RUN curl -L -o /tmp/openjdk-14.0.1_linux-x64_bin.tar.gz  https://download.java.net/java/GA/jdk14.0.1/664493ef4a6946b186ff29eb326336a2/7/GPL/openjdk-14.0.1_linux-x64_bin.tar.gz \
#     && tar xvf /tmp/openjdk-14.0.1_linux-x64_bin.tar.gz -C /usr/local

# ENV PATH=/usr/local/jdk-14.0.1/bin:$PATH

RUN wget --no-cookies \
    --no-check-certificate \
    --header "Cookie: oraclelicense=accept-securebackup-cookie" \
    https://download.oracle.com/otn-pub/java/jdk/8u251-b08/3d5a2bb8f8d4428bbe94aed7ec7ae784/jdk-8u251-linux-x64.tar.gz \
    -O /tmp/jdk-8u251-linux-x64.tar.gz \
    && tar xvf /tmp/jdk-8u251-linux-x64.tar.gz  -C /usr/local

ENV PATH=/usr/local/jdk1.8.0_251/bin:$PATH