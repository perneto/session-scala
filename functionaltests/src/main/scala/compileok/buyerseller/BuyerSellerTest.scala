package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.Port._


object BuyerSellerTest {
  def main(args: Array[String]) {
    val buyer = newLocalPort("buyerseller.spr", 'Buyer)
    val seller = newLocalPort("buyerseller.spr", 'Seller)
    
    println("running...")

    actor { startSession(buyer, seller) }
    
    // Starts Seller only once
    actor {
      seller.bind { s =>
        println("Seller: started")
        val o = s.?[Order]('Buyer)
        s ! 'Buyer -> 2000
        s.receive('Buyer) {
          case OK =>
            s ! 'Buyer -> new Invoice(2000)
            val payment = s.?[Payment]('Buyer)
          case NotOK =>
            val reason = s.?[String]('Buyer)
        }
        println("Seller: finished")
      }
    }

    buyer.bind { s =>
      println("Buyer: started")
      s ! 'Seller -> new Order(100)
      val price = s.?[Int]('Seller)
      if (price < 10000) {
        s ! 'Seller -> OK
        val invoice = s.?[Invoice]('Seller)
        s ! 'Seller -> new Payment(price)
      } else {
        s ! 'Seller -> NotOK
        s ! 'Seller -> "Too expensive"
      }
      println("Buyer: finished")
    }
  }
}