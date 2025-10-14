FROM tomcat:10.0.27

ARG MAVEN_VERSION=3.9.11
ARG USER_HOME_DIR="/root"
ARG MAVEN_HOME=/usr/share/maven

RUN wget -q "https://dlcdn.apache.org/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz" -O /tmp/maven.tar.gz && \
    tar xzf /tmp/maven.tar.gz -C /usr/share/ && \
    ln -s /usr/share/apache-maven-${MAVEN_VERSION} $MAVEN_HOME && \
    rm /tmp/maven.tar.gz && \
    apt-get remove -y wget && \
    apt-get clean

ENV MAVEN_CONFIG "$USER_HOME_DIR/.m2"
ENV PATH=$MAVEN_HOME/bin:$PATH