package compileok.buyerseller

import scala.actors.Actor.actor
import uk.ac.ic.doc.sessionscala.PublicPort._
import uk.ac.ic.doc.sessionscala.SessionChannel


object BuyerSellerRecursiveTest {
  def main(args: Array[String]) {
    val buyer = newLocalPort("buyersellerrec.spr", 'Buyer)
    val seller = newLocalPort("buyersellerrec.spr", 'Seller)
    actor {
      seller.bind { s =>
        println("Seller: started")
        val o = s.?[Order]('Buyer)
        def quoteRecursionSeller(s: SessionChannel, quote: Int) {
          s ! 'Buyer -> quote
          s.receive('Buyer) {
            case ('Buyer, OK) =>
              s ! 'Buyer -> Invoice(quote)
              val payment = s.?[Payment]('Buyer)
            case ('Buyer, NotOK) =>
              val reason = s.?[String]('Buyer)
              quoteRecursionSeller(s, quote - 100)
          }
        }
        quoteRecursionSeller(s, 2000)
        println("Seller: finished")
      }
    }


    buyer.bind { s =>
      println("Buyer: started")
      s ! 'Seller -> Order(100)
      def quoteRecursionBuyer(s: SessionChannel) {
        val price = s.?[Int]('Seller)
        if (price < 1950) {
          s ! 'Seller -> OK
          val invoice = s.?[Invoice]('Seller)
          s ! 'Seller -> Payment(price)
        } else {
          s ! 'Seller -> NotOK
          s ! 'Seller -> "Too expensive"
          quoteRecursionBuyer(s)
        }
      }
      quoteRecursionBuyer(s)
      println("Buyer: finished")
    }
  }
}