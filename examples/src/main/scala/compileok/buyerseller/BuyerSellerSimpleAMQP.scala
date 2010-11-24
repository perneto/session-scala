package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._

object BuyerSellerSimpleAMQP {
  def main(args: Array[String]) {
    withAMQPChannel(Set('Buyer, 'Seller)) { sharedChannel =>

      sharedChannel.invite("", 'Buyer -> localhost, 'Seller -> localhost)

      sharedChannel.forwardInvite('Buyer -> localhost)

      actor {
        sharedChannel.accept('Seller) { s =>
          val item = s('Buyer).?[String]
          s('Buyer) ! 2000
          s('Buyer).receive {
            case address: String =>
              val deliveryDate = s('Buyer).?[String]
              println("placing order: " + item + " " + address + " " + deliveryDate)
            case 'quit =>              
          }
          println("*****************Seller: finished")
        }
        println("############## Seller: sharedChannel.accept exited")
      }

      sharedChannel.accept('Buyer) { s =>        
        s('Seller) ! "Widget A"
        val quote = s('Seller).?[Int]
        if (quote < 1000) {
          s('Seller) ! "123 Penny Lane"
          s('Seller) ! "4/6/2011 10:00 UTC-7"
        } else {
          s('Seller) ! 'quit
        }
        println("*****************Buyer: finished")
      }
      println("############## Buyer: sharedChannel.accept exited")
    }
    println("$$$$$$$$$$$$$$$$closed shared channel")
  }
}