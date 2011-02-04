package buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel._

object BuyerSellerSimpleAMQP {
  case class Title(title: String)
  case object Quit

  def main(args: Array[String]) {
    withAMQPChannel("protocol Test { role Buyer, Seller; } ", port = 5672) { sharedChannel =>

      sharedChannel.invite("/Users/omp08/code/SessML/ex/BuyerSeller/BuyerSeller.session",
        'Buyer -> localhost, 'Seller -> localhost)

      actor {
        sharedChannel.accept('Seller) { s =>
          val ('Title, item: String) = s('Buyer).??
          s('Buyer) ! ('Quote, 2000)
          s('Buyer).receive {
            case address: String =>
              val ('Date, deliveryDate: String) = s('Buyer).??
            case 'Quit => println("received 'Quit")
          }
          println("*****************Seller: finished")
        }
      }

      sharedChannel.accept('Buyer) { s =>
        s('Seller) ! ('Title, "Widget A")
        val ('Quote, quote: Int) = s('Seller).??
        if (quote < 1000) {
          s('Seller) ! "123 Penny Lane"
          s('Seller) ! "4/6/2011 10:00 UTC-7"
        } else {
          s('Seller) ! 'Quit
        }
        println("*****************Buyer: finished")
      }
    }    
  }
}