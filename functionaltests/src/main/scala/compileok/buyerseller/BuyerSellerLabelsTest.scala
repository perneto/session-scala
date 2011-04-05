package compileok.buyerseller

import scala.actors.Actor._
import uk.ac.ic.doc.sessionscala.Address._

object BuyerSellerLabelsTest {
  case class Title(title: String)
  case object Quit

  def main(args: Array[String]) {
    val buyerseller = """
      protocol BuyerSeller {
        role Buyer, Seller;
        String from Buyer to Seller;
        quote(Int) from Seller to Buyer;
        details() from Seller to Buyer;
        choice from Buyer to Seller {
          String:
            date(String) from Buyer to Seller;
          mylabel(String):
          quit():
        }
      }
    """
    val buyer = newLocalPort(buyerseller, 'Buyer)
    val seller = newLocalPort(buyerseller, 'Buyer)
    
    actor {
      seller.bind { s =>
        val item = s.?[String]('Buyer)
        s ! 'Buyer -> ('quote, 900)
        s ! 'Buyer -> 'details
        println("Seller about to receive")
        s.receive('Buyer) {
          case address: String =>
            val deliveryDate = s.?[String]('Buyer, 'date)
          case ('mylabel, s: String) =>
          case 'quit => println("received 'quit")
        }
      }
    }

    buyer.bind { s =>
      s ! 'Seller -> "Widget Foo"
      println("Buyer sent title")
      val quoteVal = s.?[Int]('Seller, 'quote) 
      println("Buyer got quote")
      s.?[Unit]('Seller, 'details)
      if (quoteVal < 1000) {
        s ! 'Seller -> "123 Penny Lane"
        println("Buyer sent String")
        s ! 'Seller -> ('date, "4/6/2011 10:00 UTC-7")
      } else {
        s ! 'Seller -> 'quit
        println("Buyer sent quit")
      }
    }
  }
}