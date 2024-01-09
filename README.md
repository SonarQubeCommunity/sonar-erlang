![Build](https://github.com/evolution-gaming/sonar-erlang/workflows/Build/badge.svg?branch=master)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=evolution-gaming_sonar-erlang&metric=alert_status)](https://sonarcloud.io/dashboard?id=evolution-gaming_sonar-erlang)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=evolution-gaming_sonar-erlang&metric=sqale_rating)](https://sonarcloud.io/dashboard?id=evolution-gaming_sonar-erlang)

# About

SonarQube server Erlang language plugin.

## Description / Features

The plugin enables analysis of Erlang within SonarQube.

## Usage

### Run an Analysis with the SonarQube Scanner (recommended method)

To run an analysis of your Erlang project, use the SonarQube Scanner.
A sample project is available on GitHub: https://github.com/SonarSource/sonar-scanning-examples/tree/master/sonarqube-scanner

### Run an Analysis with other Analyzers

Maven and Ant can also be used to launch analysis on Erlang projects.

The plugin has been tested to work with SonarQube Community Version `9.9.1` .

### Configuration

All available keys are available in SonarQube  dashboard administration or in [ErlangPlugin.java](sonar-erlang-plugin/src/main/java/org/sonar/plugins/erlang/ErlangPlugin.java)

#### EUnit report import

`sonar.erlang.eunit.reportsfolder` - sets the Eunit reports folder, `.eunit` by default.

`sonar.erlang.eunit.coverdata.filename` - sets the Eunit coverage report file name, `eunit.coverdata` by default.

#### Common Test report import

`sonar.erlang.ct.coverdata.filename` - sets the Common Test coverage file name, `all.coverdata` by default in project root. The easiest way to export this file is to use a [cover spec file](https://erlang.org/doc/apps/common_test/cover_chapter.html#the-cover-specification-file).

You can see an example of such a file [HERE](sonar-erlang-plugin/src/test/resources/org/sonar/plugins/erlang/erlcount/config/test.cover.spec).

Check out how to configure Common Test for coverage in the [sample Makefile](sonar-erlang-plugin/src/test/resources/org/sonar/plugins/erlang/erlcount/Makefile).

#### Dialyzer

`sonar.erlang.dialyzer.filename` - sets Dialyzer report file name.

[See issue about Dialyzer](https://github.com/evolution-gaming/sonar-erlang/issues/24)

#### Xref

`sonar.erlang.xref.filename` - sets Xref report file name, located in Eunit report folder. `xref.log` by default.

#### Elvis

`sonar.erlang.elvis.filename` - sets [Elvis](https://github.com/inaka/elvis) report file name.

## Development

The project uses `Maven 3` and `JDK 11` and above.

### Build

Build the release plugin JAR with Maven

```shell script
mvn clean package
```

You will find the built JAR in `sonar-erlang-plugin/target/sonar-erlang-plugin.jar`

### Test

Run all unit tests with

```shell script
mvn test
```

### Integration testing

First, you can bring up a local SonarQube server instance using `docker` by running

```shell script
docker compose up -d
```

This will run a SonarQube instance in Docker and mount necessary volumes.

To install the built plugin jar, you must copy it to the following path like so after `mvn package`:

```shell script
cp ./sonar-erlang-plugin/target/sonar-erlang-plugin.jar ./sonar/extensions/plugins/sonar-erlang-plugin.jar
```

After that, access the server's dashboard (usually `localhost:9000`) with initial credentials `admin:admin`
Then, restart the server instance and accept the loaded plugin.

Now, create a new project with the project key `Erlang::erlcount` and save the access token.

Finally, navigate to the sample project in [sonar-erlang-plugin/src/test/resources/org/sonar/plugins/erlang/erlcount](./sonar-erlang-plugin/src/test/resources/org/sonar/plugins/erlang/erlcount) and run the `sonar-scanner` CLI:

```shell
sonar-scanner -Dsonar.login=<YOUR-TOKEN>
```

Once the scan is complete, you can view the results of the analysis in the dashboard, or fetch the metrics using the web API.

### Docker build

A basic `Dockerfile` is provided to build the plugin without installing JDK and Maven.  To make integration test processes easier it is directly written into `./sonar/extensions/plugins`:

```shell
docker build --target export -o- . | tar xv -C ./sonar/extensions/plugins/
```

You can easily debug and inspect the entire build process without using the final `export` target.  This command produces an image containing all the temporary build artifacts and outcomes of the build process:

```shell
docker build . -t sonar-erlang-build
```

### Contributing

Simply create a new GitHub Issue in 2 categories: suggestion, or a bug report.
If possible, include any useful data like logs, examples, code etc.
