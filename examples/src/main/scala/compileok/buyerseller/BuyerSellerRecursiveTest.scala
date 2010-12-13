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
        val (_,o) = s('Buyer).?
        def quoteRecursionSeller(s: SessionChannel, quote: Int) {
          s('Buyer) ! quote
          s('Buyer).receive {
            case OK =>
              s('Buyer) ! Invoice(quote)
              val (_,payment) = s('Buyer).?
            //case s: String => todo: this should be allowed,
            // a choice receive can have more branches than spec by subtyping
            case NotOK =>
              val (_,reason) = s('Buyer).?
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
          val (_,price: Int) = s('Seller).?
          if (price < 1950) {
            s('Seller) ! OK
            val (_,invoice: Invoice) = s('Seller).?
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