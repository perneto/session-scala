package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala._


object BuyerSellerRecursive {
  def main(args: Array[String]) {
    @protocol("buyersellerrec.scribble")
    val sharedChannel = SharedChannel.createLocalChannel(Set("Buyer", "Seller"))

    actor {
      sharedChannel.accept("Seller") { s =>
        println("Seller: started")
        val o = s("Buyer").?[Order]
        def quoteRecursion(s: SessionChannel, quote: Int) {
          s("Buyer") ! quote
          s("Buyer").receive {
            case OK =>
              s("Buyer") ! new Invoice(quote)
              val payment = s("Buyer").?[Payment]
            case NotOK =>
              val reason = s("Buyer").?[String]
              quoteRecursion(s, quote - 100)
          }
        }
        quoteRecursion(s, 2000)

        println("Seller: finished")
      }
    }

    actor {
      sharedChannel.accept("Buyer") { s =>
        println("Buyer: started")
        s("Seller") ! new Order(100)
        def quoteRecursion(s: SessionChannel) {
          val price = s("Seller").?[Int]
          if (price < 1950) {
            s("Seller") ! OK
            val invoice = s("Seller").?[Invoice]
            s("Seller") ! new Payment(price)
          } else {
            s("Seller") ! NotOK
            s("Seller") ! "Too expensive"
            quoteRecursion(s)
          }
        }
        quoteRecursion(s)
        println("Buyer: finished")
      }
    }
  }

}