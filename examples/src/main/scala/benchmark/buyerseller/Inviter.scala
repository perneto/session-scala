package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._


/**
 * Created by: omp08
 */

object Inviter {
  def main(args: Array[String]) {
    val brokerHost = args(0)
    withAMQPChannel(Set('Buyer, 'Seller), brokerHost) { sharedChannel =>

      sharedChannel.invite("", 'Buyer -> localhost, 'Seller -> localhost)
    }
    println("Inviter exiting")
  }
}