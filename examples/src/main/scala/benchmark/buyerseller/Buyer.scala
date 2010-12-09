package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._


/**
 * Created by: omp08
 */

object Buyer {
  def main(args: Array[String]) {
    val brokerHost = args(0)
    withAMQPChannel(Set('Buyer, 'Seller), brokerHost) { sharedChannel =>
      sharedChannel.accept('Buyer) { s =>
        println("Buyer started")
        s('Seller) ! Title("Widget A")
        val quote = s('Seller).?[Int]
        if (quote < 1000) {
          s('Seller) ! "123 Penny Lane"
          s('Seller) ! "4/6/2011 10:00 UTC-7"
        } else {
          s('Seller) ! Quit
        }
        println("*****************Buyer: finished")
      }
    }
  }
}