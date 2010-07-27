package compileok.buyerseller

import scala.actors.Actor, Actor._
import uk.ac.ic.doc.sessionscala.{protocol, SharedChannel}

object BuyerSeller {
  def main(args: Array[String]) {
    @protocol("buyerseller.scribble") // Scribble file also contains roles, could
    // generate set of roles automatically
    val sharedChannel = SharedChannel.createLocalChannel(Set("Buyer", "Seller"))
    // Will restart an accepting Seller every time a session is started.
    // Similar in behaviour to a listening server socket.
    //sharedChannel.registerAccept("Seller") { ... }

    println("running...")

    // To start Seller only once
    actor {
      sharedChannel.accept("Seller") { s =>
        println("Seller: started")
        val o = s("Buyer").?[Order]
        s("Buyer") ! 2000
        s("Buyer").receive {
          case OK =>
            s("Buyer") ! new Invoice(2000)
            val payment = s("Buyer").?[Payment]
          case NotOK =>
            val reason = s("Buyer").?[String]
        }
        println("Seller: finished")
      }
    }

    actor {
      sharedChannel.accept("Buyer") { s =>
        println("Buyer: started")
        s("Seller") ! new Order(100)
        val price = s("Seller").?[Int]
        if (price < 10000) {
          s("Seller") ! OK
          val invoice = s("Seller").?[Invoice]
          s("Seller") ! new Payment(price)
        } else {
          s("Seller") ! NotOK
          s("Seller") ! "Too expensive"

        }
        println("Buyer: finished")
      }
    }
  }
}