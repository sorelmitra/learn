FROM frolvlad/alpine-oraclejdk8
VOLUME /tmp
ADD  target/boatdirectory-client-0.0.1-SNAPSHOT.jar boatdirectory-client.jar
EXPOSE 8080
ENTRYPOINT ["java","-jar","/boatdirectory-client.jar"]
