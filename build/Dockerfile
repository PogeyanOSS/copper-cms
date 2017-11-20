FROM openjdk:8-alpine

ENV COPPER_CMS_VERSION=0.1.1
ENV COPPER_CMS_PATH=/opt/cmis

RUN mkdir -p ${COPPER_CMS_PATH}
ADD copper-server/target/copper-${COPPER_CMS_VERSION}.jar ${COPPER_CMS_PATH}

WORKDIR ${COPPER_CMS_PATH}

EXPOSE 8080

CMD java -jar copper-${COPPER_CMS_VERSION}.jar