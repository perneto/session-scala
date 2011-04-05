package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{SessionChannel, Address}, Address._

/**
 * Created by: omp08
 */

object SessionChannelAliasingTest {
  def m() {

    val buyer = newLocalAddress("../compileok/buyerseller/buyerseller.spr", 'Buyer)
    val seller = newLocalAddress("../compileok/buyerseller/buyerseller.spr", 'Seller)
    actor {
      buyer.bind { s =>
        val s1 = s // wrong
      }
    }

    seller.bind { s =>
      var s1: SessionChannel = null
      s1 = s // wrong
    }
  }
}