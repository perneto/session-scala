package compileok

import uk.ac.ic.doc.sessionscala.protocol

/**
 * Created by: omp08
 */

class DelegationTest {
  def main(args: Array[String]) {
    @protocol("delegation.scribble")
    // Scribble file also contains roles, could
    // generate set of roles automatically
    val sharedChannel = SharedChannel.createLocalChannel(Set('Buyer, 'Seller))

    println("running...")

    // To start Seller only once
    actor {
      sharedChannel.join('Seller) { s =>
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
        println("Seller: finished")
      }
    }

    actor {
      sharedChannel.join('Buyer) { s =>
        println("Buyer: started")
        s('Seller) ! new Order(100)
        val (_,price: Int) = s('Seller).?
        if (price < 10000) {
          s('Seller) ! OK
          val (_,invoice: Invoice) = s('Seller).?
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