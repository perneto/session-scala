package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.{protocol, SharedChannel}
import SharedChannel._

object BuyerSellerAMQP {
  def main(args: Array[String]) {
    //@protocol("buyerseller.scribble")
    // Scribble file also contains roles, could
    // generate set of roles automatically
    withAMQPChannel(Set('Buyer, 'Seller)) { sharedChannel =>

      sharedChannel.invite("", 'Buyer -> localhost, 'Seller -> localhost)

      sharedChannel.forwardInvite('Buyer -> localhost)

      println("running...")

      // To start Seller only once
      actor {
        sharedChannel.accept('Seller) { s =>
          println("Seller: started")
          val (_,o: Order) = s('Buyer).?
          s('Buyer) ! 2000
          s('Buyer).receive {
            case OK =>
              s('Buyer) ! new Invoice(2000)
              val (_,payment: Payment) = s('Buyer).?
            case NotOK =>
              val (_,reason: String) = s('Buyer).?
          }
          println("*****************Seller: finished")
        }
        println("############## Seller: sharedChannel.accept exited")
      }

      sharedChannel.accept('Buyer) { s =>
        println("Buyer: started")
        s('Seller) ! new Order(100)
        val (_,price: Int) = s('Seller).?
        if (price < 10000) {
          s('Seller) ! OK
          val (_,invoice) = s('Seller).?
          s('Seller) ! new Payment(price)
        } else {
          s('Seller) ! NotOK
          s('Seller) ! "Too expensive"

        }
        println("*****************Buyer: finished")
      }
      println("############## Buyer: sharedChannel.accept exited")
    }
    println("$$$$$$$$$$$$$$$$closed shared channel")
  }
}
