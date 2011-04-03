package buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.Port._

object BuyerSellerSimpleAMQP {
  case class Title(title: String)
  case object Quit

  def main(args: Array[String]) {
    val proto = """protocol Test {
      role Buyer, Seller;
      Title(String) from Buyer to Seller;
      Quote(Int) from Seller to Buyer;
      choice from Buyer to Seller {
        String: Date(String) from Buyer to Seller;
        Quit():
      }
    }"""
    val buyer = AMQPPort(proto, 'Buyer)
    val seller = AMQPPort(proto, 'Seller) 

    actor { startSession(buyer, seller) }

    actor {
      seller.bind { s =>
        val item = s.?[String]('Buyer)
        s ! 'Buyer -> ('Quote, 2000)
        s.receive('Buyer) {
          case address: String =>
            val deliveryDate = s.?[String]('Buyer, 'Date)
          case 'Quit => println("received 'Quit")
        }
        println("*****************Seller: finished")
      }
    }

    buyer.bind { s =>
      s ! 'Seller -> ('Title, "Widget A")
      val quote = s.?[Int]('Seller)
      if (quote < 1000) {
        s ! 'Seller -> "123 Penny Lane"
        s ! 'Seller -> ('Date, "4/6/2011 10:00 UTC-7")
      } else {
        s ! 'Seller -> 'Quit
      }
      println("*****************Buyer: finished")
    }
  }
}