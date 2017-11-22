FROM tomcat:8.0-jre8-alpine

ENV COPPER_CMS_VERSION=0.1.2

RUN rm -rf ${CATALINA_HOME}/webapps/ROOT
RUN rm -rf ${CATALINA_HOME}/webapps/examples
RUN rm -rf ${CATALINA_HOME}/webapps/docs

ADD copper-server/target/copper-server-${COPPER_CMS_VERSION}.war ${CATALINA_HOME}/webapps/ROOT.war
ADD build/repo.json ${CATALINA_HOME}/conf