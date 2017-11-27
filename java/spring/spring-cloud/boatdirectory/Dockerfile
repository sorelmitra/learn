FROM frolvlad/alpine-oraclejdk8
VOLUME /tmp
ADD  target/boatdirectory-0.0.1-SNAPSHOT.jar boatdirectory.jar
EXPOSE 8770
ENTRYPOINT ["java","-jar","/boatdirectory.jar"]
