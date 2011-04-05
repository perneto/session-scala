package buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.{Address}
import Address._

object BuyerSellerAMQP {
  def main(args: Array[String]) {
    val buyer = AMQPPort("protocol Test { role Buyer, Seller; } ", 'Buyer)
    val seller = AMQPPort("protocol Test { role Buyer, Seller; } ", 'Seller)
    val otherbuyer = AMQPPort("protocol Test { role Buyer, Seller; } ", 'Buyer, "otherbuyer")
    
    actor { startSession(buyer, seller) }

    forwardInvite(buyer -> otherbuyer)

    println("running...")

    // To start Seller only once
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
        println("*****************Seller: finished")
      }
      println("############## Seller: sharedChannel.bind exited")
    }

    otherbuyer.bind { s =>
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
      println("*****************Buyer: finished")
    }
    println("############## Buyer: sharedChannel.bind exited")
  }
}
