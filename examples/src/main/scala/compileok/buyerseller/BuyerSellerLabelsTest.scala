package compileok.buyerseller

import scala.actors.Actor._
import uk.ac.ic.doc.sessionscala.{inlineprotocol, SharedChannel}

object BuyerSellerLabelsTest {
  case class Title(title: String)
  case object Quit

  def main(args: Array[String]) {
    @inlineprotocol("""
      protocol BuyerSeller {
        role Buyer, Seller;
        String from Buyer to Seller;
        quote(Int) from Seller to Buyer;
        details() from Seller to Buyer;
        choice from Buyer to Seller {
          String {
            date(String) from Buyer to Seller;
          }
          quit() {}
        }
      }
    """)
    val sharedChannel = SharedChannel.createLocalChannel(Set('Buyer, 'Seller))

    actor {
      sharedChannel.join('Seller) { s =>
        val item = s('Buyer).?[String]
        s('Buyer) ! ('quote, 900)
        s('Buyer) ! 'details
        println("Seller about to receive")
        s('Buyer).receive {
          case address: String =>
            val ('date, deliveryDate: String) = s('Buyer).??
          case 'quit => println("received 'quit")
        }
      }
    }

    sharedChannel.join('Buyer) { s =>
      s('Seller) ! "Widget Foo"
      println("Buyer sent title")
      val ('quote, quoteVal: Int) = s('Seller).??
      println("Buyer got quote")
      val ('details, _) = s('Seller).??
      if (quoteVal < 1000) {
        s('Seller) ! "123 Penny Lane"
        println("Buyer sent String")
        s('Seller) ! ('date, "4/6/2011 10:00 UTC-7")
      } else {
        s('Seller) ! 'quit
        println("Buyer sent quit")
      }
    }
  }
}