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
        title(String) from Buyer to Seller;
        quote(Int) from Seller to Buyer;
        choice from Seller to Buyer {
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
        val ('title, item: String) = s('Buyer).??
        s('Buyer) ! ('quote, 900)
        println("Seller about to receive")
        s('Buyer).receive {
          case address: String =>
            val ('date, deliveryDate: String) = s('Buyer).??
          case 'quit => println("received 'quit")
        }
      }
    }

    sharedChannel.join('Buyer) { s =>
      s('Seller) ! ('title, "Widget A")
      println("Buyer sent title")
      val ('quote, quote: Int) = s('Seller).??
      println("Buyer got quote")
      if (quote < 1000) {
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