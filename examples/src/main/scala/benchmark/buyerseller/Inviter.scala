package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.Port
import Port._


/**
 * Created by: omp08
 */

object Inviter {
  def main(args: Array[String]) {
    val brokerHost = args(0)
    val shadeXX = AMQPPort("../../buyerseller/buyerseller.spr", 'Buyer, "shadeXX@"+brokerHost)
    val shade04 = AMQPPort("../../buyerseller/buyerseller.spr", 'Seller, "shade04@"+brokerHost)
    startSession(shade04, shadeXX)
    println("Inviter exiting")
  }
}