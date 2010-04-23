package buyerseller.sessionactors

import scala.actors.Actor, Actor._
import buyerseller._
import scala.concurrent.ops.spawn
import uk.ac.ic.doc.sessionscala.sessionops

object BuyerSeller extends Application {
  
  val sharedChannel = sessionops.createLocalChannel(Set("Buyer", "Seller"))
  // Will restart an accepting Seller every time a session is started.
  // Similar in behaviour to a listening server socket.
  //sharedChannel.registerAccept("Seller") { ... }

  println("running...")

  // To start Seller only once
  spawn {
    println("spawned")
    sharedChannel.accept("Seller") { s =>
      println("Seller: started")
      val o : Order = ?.asInstanceOf[Order]
      s("Buyer") ! 2000
      receive {
        case OK =>
          s("Buyer") ! new Invoice(5000)
          val payment = ?
        case NotOK =>
          val reason = ?
      }
      println("Seller: finished")
    }
  }
  println("after spawn")

  sharedChannel.accept("Buyer") { s =>
    println("Buyer: started")
    s("Seller") ! new Order(100)
    val price = ?.asInstanceOf[Int]
    if (price < 10000) {
      s("Seller") ! OK
      val invoice = ?
      s("Seller") ! new Payment(price)
    } else {
      s("Seller") ! NotOK
      s("Seller") ! "Too expensive"
    }
    println("Buyer: finished")
  }
}