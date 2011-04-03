package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{SessionChannel, PublicPort}, PublicPort._

/**
 * Created by: omp08
 */

object SessionChannelAliasingTest {
  def m() {

    val buyer = newLocalPort("../compileok/buyerseller/buyerseller.spr", 'Buyer)
    val seller = newLocalPort("../compileok/buyerseller/buyerseller.spr", 'Seller)
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