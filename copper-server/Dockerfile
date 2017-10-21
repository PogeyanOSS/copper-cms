FROM openjdk:alpine

RUN mkdir -p /opt/cmis

ADD target/ /opt/cmis

WORKDIR /opt/cmis

EXPOSE 8080

CMD ["java","-jar","cmislab-1.0.0.jar","-httpPort=8080"]