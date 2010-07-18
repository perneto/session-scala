import org.scalatest.FunSuite

import scala.tools.nsc._
import reporters.ConsoleReporter

/**
 * Created by: omp08
 */

class RunCompiler extends FunSuite {
  val scalaVersion = "2.8.0.RC6"
  test("run compiler") {

    val settings = new Settings
    settings.classpath.tryToSet(List(
        "project/boot/scala-"+scalaVersion+"/lib/scala-compiler.jar" +
        ":project/boot/scala-"+scalaVersion+"/lib/scala-library.jar" +
        ":runtime/target/scala_"+scalaVersion+"/classes" +
        ":compilerplugin/target/scala_"+scalaVersion+"/classes" +
        ":examples/target/scala_"+scalaVersion+"/classes"
    ))
    //println("Classpath: " + settings.classpath)
    //settings.showPlugins only works if you're not compiling a file, same as -help
    settings.require.tryToSet(List("sessiontyping"))
    settings.plugin.tryToSet(List(
      "compilerplugin/target/scala_"+scalaVersion+"/compilerplugin_"+scalaVersion+"-0.1.jar"))
    settings.pluginOptions.tryToSet(List())
    //settings.print.tryToSet(List("all"))
    //settings.verbose.tryToSet(List())
    val tmpdir = System.getProperty("java.io.tmpdir")
    settings.d.tryToSet(List(tmpdir))
    val compiler = new Global(settings, new ConsoleReporter(settings))

    new compiler.Run().compile(List(
      "examples/src/main/scala/buyerseller/sessionactors/BuyerSeller.scala"))
  }
}