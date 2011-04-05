package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.Address
import Address._


/**
 * Created by: omp08
 */

object Inviter {
  def main(args: Array[String]) {
    val brokerHost = args(0)
    val shadeXX = AMQPAddress("../../buyerseller/buyerseller.spr", 'Buyer, "shadeXX@"+brokerHost)
    val shade04 = AMQPAddress("../../buyerseller/buyerseller.spr", 'Seller, "shade04@"+brokerHost)
    startSession(shade04, shadeXX)
    println("Inviter exiting")
  }
}