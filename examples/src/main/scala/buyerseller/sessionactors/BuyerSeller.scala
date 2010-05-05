package buyerseller.sessionactors

import scala.actors.Actor, Actor._
import buyerseller._
import uk.ac.ic.doc.sessionscala.sessionops

object BuyerSeller {
  def main(args: Array[String]) {
    val sharedChannel = sessionops.createLocalChannel(Set("Buyer", "Seller"))
    // Will restart an accepting Seller every time a session is started.
    // Similar in behaviour to a listening server socket.
    //sharedChannel.registerAccept("Seller") { ... }

    println("running...")

    // To start Seller only once
    actor {
      sharedChannel.accept("Seller") { s =>
        println("Seller: started")
        val o : Order = s.?.asInstanceOf[Order]
        s("Buyer") ! 2000
        s.receive {
          case OK =>
            s("Buyer") ! new Invoice(5000)
            val payment = s.?
          case NotOK =>
            val reason = s.?
        }
        println("Seller: finished")
      }
    }

    actor {
      sharedChannel.accept("Buyer") { s =>
        println("Buyer: started")
        s("Seller") ! new Order(100)
        val price = s.?.asInstanceOf[Int]
        if (price < 10000) {
          s("Seller") ! OK
          val invoice = s.?
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