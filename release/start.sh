#!/bin/bash

# For Docker
cd /opt/backend/API || exit 1
java -Dspring.config.location=../conf/application.properties -jar api-VERSION.jar --logging.config=../conf/logback.xml