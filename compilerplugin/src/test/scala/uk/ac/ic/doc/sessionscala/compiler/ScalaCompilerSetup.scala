package uk.ac.ic.doc.sessionscala.compiler

import tools.nsc.{Settings, Global}

/**
 * Created by: omp08
 */

trait ScalaCompilerSetup {
  val settings = new Settings
  val scalaVersion = "2.8.1"
  settings.classpath.tryToSet(List(
            "project/boot/scala-"+scalaVersion+"/lib/scala-compiler.jar" +
            ":project/boot/scala-"+scalaVersion+"/lib/scala-library.jar" +
            ":compilerplugin/target/scala_"+scalaVersion+"/test-classes"))
  val global = new Global(settings)
  new global.Run // to initialize standard definitions (basic types, etc)

}