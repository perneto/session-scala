package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._


/**
 * Created by: omp08
 */

object Seller {

  def main(args: Array[String]) {
    val brokerHost = args(0)
    withAMQPChannel(Set('Buyer, 'Seller), brokerHost) { sharedChannel =>
      sharedChannel.accept('Seller) { s =>
        println("Seller started")
        val item = s('Buyer).?[Title]
        s('Buyer) ! 2000
        s('Buyer).receive {
          case address: String =>
            val deliveryDate = s('Buyer).?[String]
            println("placing order: " + item + " " + address + " " + deliveryDate)
          case Quit =>
        }
        println("*****************Seller: finished")
      }
    }
  }
}