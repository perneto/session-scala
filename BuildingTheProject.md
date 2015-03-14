# Building the project #

session-scala uses [SBT](http://code.google.com/p/simple-build-tool/) for building. To install SBT,
follow the instructions here:
http://code.google.com/p/simple-build-tool/wiki/Setup

Basically, you just need to download the SBT jar file, and add a startup script anywhere in your path to launch it.

SBT will download Scala automatically, so you don't even need an existing Scala distribution.

Then, to build session-scala:
```
hg clone https://session-scala.googlecode.com/hg/ session-scala 
cd session-scala
sbt sbt update "project compilerplugin" proguard "project runtime" proguard
```

To run the tests (requires a running RabbitMQ server on localhost):
```
cd session-scala
sbt test
```