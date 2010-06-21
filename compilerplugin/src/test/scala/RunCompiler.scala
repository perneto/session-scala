import org.scalatest.FunSuite

import scala.tools.nsc._
import reporters.ConsoleReporter

/**
 * Created by: omp08
 */

class RunCompiler extends FunSuite {
  test("run compiler") {
    val settings = new Settings
    val compiler = new Global(settings, new ConsoleReporter(settings))

    new compiler.Run().compile(List(
    "/Users/omp08/Code/scala-sessions/examples/src/main/scala/buyerseller/sessionactors/BuyerSeller.scala"))
  }
}