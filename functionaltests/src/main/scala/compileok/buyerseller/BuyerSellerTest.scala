package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.{SharedChannel}


object BuyerSellerTest {
  def main(args: Array[String]) {
    SharedChannel.withLocalChannel("buyerseller.spr") { sharedChannel =>

      println("running...")

      // Starts Seller only once
      actor {
        sharedChannel.join('Seller) { s =>
          println("Seller: started")
          val o = s('Buyer).?[Order]
          s('Buyer) ! 2000
          s('Buyer).receive {
            case OK =>
              s('Buyer) ! new Invoice(2000)
              val payment = s('Buyer).?[Payment]
            case NotOK =>
              val reason = s('Buyer).?[String]
          }
          println("Seller: finished")
        }
      }

      sharedChannel.join('Buyer) { s =>
        println("Buyer: started")
        s('Seller) ! new Order(100)
        val price = s('Seller).?[Int]
        if (price < 10000) {
          s('Seller) ! OK
          val invoice = s('Seller).?[Invoice]
          s('Seller) ! new Payment(price)
        } else {
          s('Seller) ! NotOK
          s('Seller) ! "Too expensive"

        }
        println("Buyer: finished")
      }
    }
  }
}