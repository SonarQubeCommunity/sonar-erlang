FROM maven:3.9.3-amazoncorretto-17-debian AS build

# set up build environment
WORKDIR /opt/src
COPY . .

# run tests
RUN mvn test verify

# build sonar plugin
RUN mvn package -Dsource.skip=true -Denforcer.skip=true -Danimal.sniffer.skip=true -Dmaven.test.skip=true

# export target contains only the built JAR file
FROM scratch AS export
COPY --from=build /opt/src/sonar-erlang-plugin/target/sonar-erlang-plugin.jar /

# default target is the built image
FROM build AS result
