package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._


/**
 * Created by: omp08
 */

object Inviter {
  def main(args: Array[String]) {
    val brokerHost = args(0)
    withAMQPChannel("../../compileok/buyerseller/buyerseller.spr", brokerHost) { sharedChannel =>

      sharedChannel.invite("", 'Buyer -> "shadeXX", 'Seller -> "shade04")
    }
    println("Inviter exiting")
  }
}