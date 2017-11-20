FROM frolvlad/alpine-oraclejdk8
VOLUME /tmp
ADD  target/bootadvanced-0.0.1-SNAPSHOT.jar bootadvanced.jar
EXPOSE 7070 8080 9090
ENTRYPOINT ["java","-jar","/bootadvanced.jar"]
