#!/bin/bash


ROOT=/home/omp08/session-scala
BENCHMARKS=$ROOT/examples/src/main/resources/benchmarks
RT_LIB_DIR=$ROOT/runtime/lib_managed/scala_2.8.1/compile
RUNTIME_LIB=$RT_LIB_DIR/amqp-client-2.1.1.jar:$RT_LIB_DIR/commons-cli-1.1.jar:$RT_LIB_DIR/commons-io-1.2.jar:$RT_LIB_DIR/scalaj-collection_2.8.0-1.0.jar
RUNTIME_JAR=$ROOT/runtime/target/scala_2.8.1/runtime_2.8.1-0.1.jar
EXAMPLES_JAR=$ROOT/examples/target/scala_2.8.1/examples_2.8.1-0.1.jar

# $1: role (main class name, first letter is capital)
# $2: IP address of monitor (or AMQP broker)
start_role() {
  cd session-scala
  hg up
  sbt "project runtime" update package "project examples" package
  echo scala -cp $RUNTIME_LIB:$RUNTIME_JAR:$EXAMPLES_JAR benchmark.buyerseller.$1 $2
  scala -cp $RUNTIME_LIB:$RUNTIME_JAR:$EXAMPLES_JAR benchmark.buyerseller.$1 $2
}
