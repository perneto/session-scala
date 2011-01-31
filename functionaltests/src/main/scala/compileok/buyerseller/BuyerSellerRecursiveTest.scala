package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala._


object BuyerSellerRecursiveTest {
  def main(args: Array[String]) {
    @protocol("buyersellerrec.scribble")
    val sharedChannel = SharedChannel.createLocalChannel(Set('Buyer, 'Seller))

    actor {
      sharedChannel.join('Seller) { s =>
        println("Seller: started")
        val o = s('Buyer).?[Order]
        def quoteRecursionSeller(s: SessionChannel, quote: Int) {
          s('Buyer) ! quote
          s('Buyer).receive {
            case OK =>
              s('Buyer) ! Invoice(quote)
              val payment = s('Buyer).?[Payment]
            //case s: String => todo: allow choice receive to have more branches than spec by subtyping
            case NotOK =>
              val reason = s('Buyer).?[String]
              quoteRecursionSeller(s, quote - 100)
          }
        }
        quoteRecursionSeller(s, 2000)
        println("Seller: finished")
      }
    }

    actor {
      sharedChannel.join('Buyer) { s =>
        println("Buyer: started")
        s('Seller) ! Order(100)
        def quoteRecursionBuyer(s: SessionChannel) {
          val price = s('Seller).?[Int]
          if (price < 1950) {
            s('Seller) ! OK
            val invoice = s('Seller).?[Invoice]
            s('Seller) ! Payment(price)
          } else {
            s('Seller) ! NotOK
            s('Seller) ! "Too expensive"
            quoteRecursionBuyer(s)
          }
        }
        quoteRecursionBuyer(s)
        println("Buyer: finished")
      }
    }
  }

}