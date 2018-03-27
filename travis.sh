#!/bin/bash

set -euox pipefail

function installTravisTools {
  mkdir -p ~/.local
  curl -sSL https://github.com/SonarSource/travis-utils/tarball/v47 | tar zx --strip-components 1 -C ~/.local
  source ~/.local/bin/install
}

case "$TARGET" in

CI)
  mvn verify -B -e -V
  ;;

IT)
  installTravisTools

  mvn package -Dsource.skip=true -Denforcer.skip=true -Danimal.sniffer.skip=true -Dmaven.test.skip=true \
    -Dorg.slf4j.simpleLogger.log.org.apache.maven.cli.transfer.Slf4jMavenTransferListener=warn

  if [ "$SQ_VERSION" == "DEV" ]; then
    build_snapshot "SonarSource/sonarqube"
  fi

  cd its/plugin
  mvn -Dsonar.runtimeVersion="$SQ_VERSION" -Dmaven.test.redirectTestOutputToFile=false \
    -Dorg.slf4j.simpleLogger.log.org.apache.maven.cli.transfer.Slf4jMavenTransferListener=warn install
  ;;

*)
  echo "Unexpected TARGET value: $TARGET"
  exit 1
  ;;

esac
