package benchmark.buyerseller

import uk.ac.ic.doc.sessionscala.Address
import Address._


/**
 * Created by: omp08
 */

object Buyer {
  def main(args: Array[String]) {
    val brokerHost = args(0)
    val buyer = AMQPPort("../../buyerseller/buyerseller.spr", 'Buyer, "buyer", brokerHost)
    buyer.bind { s =>
      //println("Buyer accepted")
      s ! 'Seller -> ('title, "Widget A")
      val quote = s.?[Int]('Seller)
      if (quote < 1000) {
        s ! 'Seller -> "123 Penny Lane"
        s ! 'Seller -> "4/6/2011 10:00 UTC-7"
      } else {
        s ! 'Seller -> 'quit
      }
      //println("*****************Buyer: finished")
    }
  }
}