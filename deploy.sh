#!/bin/sh

curl -Lo sbt https://raw.githubusercontent.com/sbt/sbt/v1.6.1/sbt

chmod +x ./sbt

./sbt fullLinkJS

cp -r target/scala-3*/cloudflare-test-opt/ ./functions
