package buyerseller.sessionactors
import buyerseller._
import scala.actors.Actor
import uk.ac.ic.doc.sessionscala.{SharedChannel, sessionops}


class BuyerSellerProtocol(val buyer: Actor, val seller: Actor)
/*
object BuyerSellerLanguageChanges extends Application {
  protocol BuyerSellerProtocol {
    role Buyer
    role Seller

  }

  val sharedChannel: SharedChannel = sessionops.createLocalChannel(BuyerSellerProtocol)
  // Will restart an accepting Seller every time a session is started.
  // Similar in behaviour to a listening server socket.
  //sharedChannel.registerAccept("Seller") { ... }

  trait SharedChan[T] {
    def accept()
  }
  // To start Seller only once
  spawn {
    sharedChannel.accept(BuyerSellerProtocol.Seller) { s: BuyerSellerProtocol =>
      println("Seller: started")
      val o : Order = ?.asInstanceOf[Order]
      s.Buyer ! 2000
      receive {
        case OK =>
          s.Buyer ! new Invoice(5000)
          val payment = ?
        case NotOK =>
          val reason = ?
      }
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
}

*/