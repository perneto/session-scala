import org.scalatest.FunSuite

import scala.tools.nsc._
import reporters.ConsoleReporter

/**
 * Created by: omp08
 */

class RunCompiler extends FunSuite {
  test("run compiler") {

    val settings = new Settings
    settings.classpath.tryToSet(List(
        "runtime/target/scala_2.8.0.RC1/classes:compilerplugin/target/scala_2.8.0.RC1/classes" +
        ":examples/target/scala_2.8.0.RC1/classes"
    ))
    settings.showPlugins.tryToSet(List("true"))
    settings.plugin.tryToSet(List())
    settings.pluginOptions.tryToSet(List())
    val tmpdir = System.getProperty("java.io.tmpdir")
    settings.d.tryToSet(List(tmpdir))
    val compiler = new Global(settings, new ConsoleReporter(settings))

    new compiler.Run().compile(List(
      "examples/src/main/scala/buyerseller/sessionactors/BuyerSeller.scala"))
  }
}