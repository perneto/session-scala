package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._

object BuyerSellerSimpleAMQP {
  case class Title(title: String)
  case object Quit

  def main(args: Array[String]) {
    withAMQPChannel(Set('Buyer, 'Seller)) { sharedChannel =>

      sharedChannel.invite("", 'Buyer -> localhost, 'Seller -> localhost)

      actor {
        sharedChannel.accept('Seller) { s =>
          val (_,item: Title) = s('Buyer).?
          s('Buyer) ! 2000
          s('Buyer).receive {
            case address: String =>
              val (_,deliveryDate: String) = s('Buyer).?
              println("placing order: " + item + " " + address + " " + deliveryDate)
            case Quit =>
          }
          println("*****************Seller: finished")
        }
      }

      sharedChannel.accept('Buyer) { s =>        
        s('Seller) ! Title("Widget A")
        val (_,quote: Int) = s('Seller).?
        if (quote < 1000) {
          s('Seller) ! "123 Penny Lane"
          s('Seller) ! "4/6/2011 10:00 UTC-7"
        } else {
          s('Seller) ! Quit
        }
        println("*****************Buyer: finished")
      }
    }    
  }
}