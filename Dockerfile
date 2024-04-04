FROM phusion/baseimage:jammy-1.0.1
ARG VERSION
ARG HOME=/opt/backend/API

RUN apt-get update && apt-get install -y openjdk-21-jdk apt-utils unzip net-tools && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /opt/backend/API
COPY release/API-${VERSION}.zip /opt/backend/
WORKDIR /opt/backend/
RUN unzip API-${VERSION}.zip && rm API-${VERSION}.zip
RUN mv API-${VERSION}/* ${HOME}/ && rmdir API-${VERSION}
WORKDIR ${HOME}
RUN chmod +x  bin/start.sh
ENTRYPOINT bin/start.sh