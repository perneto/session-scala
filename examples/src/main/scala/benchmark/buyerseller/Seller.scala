package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.PublicPort
import PublicPort._


/**
 * Created by: omp08
 */

object Seller {

  def main(args: Array[String]) {
    val brokerHost = args(0)
    val seller = AMQPPort("../../buyerseller/buyerseller.spr", 'Seller, "seller", brokerHost)
    println("Seller: before bind")
    seller.bind { s =>
      println("Seller accepted")
      val item = s.?[String]('Buyer, 'title)
      s ! 'Buyer -> 2000
      s.receive('Buyer) {
        case ('address, address: String) =>
          val deliveryDate = s.?[String]('Buyer)
          println("placing order: " + item + " " + address + " " + deliveryDate)
        case 'quit => println("received 'quit")
      }
      println("*****************Seller: finished")
    }
  }
}