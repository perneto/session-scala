package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._


/**
 * Created by: omp08
 */

object Seller {

  def main(args: Array[String]) {
    val brokerHost = args(0)
    withAMQPChannel("../../compileok/buyerseller/buyerseller.spr", brokerHost) { sharedChannel =>
      println("Seller: before accept")
      sharedChannel.accept('Seller) { s =>
        println("Seller accepted")
        val ('title, item: String) = s('Buyer).??
        s('Buyer) ! 2000
        s('Buyer).receive {
          case ('address, address: String) =>
            val deliveryDate = s('Buyer).?[String]
            println("placing order: " + item + " " + address + " " + deliveryDate)
          case 'quit => println("received 'quit")
        }
        println("*****************Seller: finished")
      }
    }
  }
}