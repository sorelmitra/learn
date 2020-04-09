FROM openjdk:11-jre-slim

LABEL vendor="Sorel Mitra"
LABEL maintainer="Sorel Mitra"

ADD maven/${project.artifactId}-${project.version}.jar ${project.artifactId}.jar

ENV JAVA_OPTS=""

EXPOSE 8080

ENTRYPOINT exec java $JAVA_OPTS -Djava.security.egd=file:/dev/./urandom \
    -Dfile.encoding=UTF-8 \
    -jar /${project.artifactId}.jar
