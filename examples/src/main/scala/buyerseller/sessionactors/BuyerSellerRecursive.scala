package buyerseller.sessionactors

import scala.actors.Actor, Actor._
import buyerseller._
import uk.ac.ic.doc.sessionscala._
import scala.concurrent.ops.spawn


object BuyerSellerRecursive extends Application {
/*
  val sharedChannel = SharedChannel.createLocalChannel(Set("Buyer", "Seller"))
  // Will restart an accepting Seller every time a session is started.
  // Similar in behaviour to a listening server socket.
  //sharedChannel.registerAccept("Seller") { ... }


  def negotiatePrice(s: String => Actor, initPrice: Int): Unit = {
    s("Buyer") ! initPrice
    receive {
      case OK =>
        s("Buyer") ! new Invoice(5000)
        val payment = ?
      case NotOK =>
        negotiatePrice(initPrice - 100)
      case Stop =>
    }
  }
  // To start Seller only once
  spawn {
    sharedChannel.accept("Seller") { s =>
      println("Seller: started")
      val o : Order = ?.asInstanceOf[Order]
      negotiatePrice(s, 2000)

      println("Seller: finished")
    }
  }

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
*/
}