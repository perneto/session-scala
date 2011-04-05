package compileerror

import uk.ac.ic.doc.sessionscala.{Address}
import compileok.buyerseller.{NotOK, OK}
import actors.Actor.actor

/**
 * Created by: omp08
 */

object BadBranchingTest {
  def m() {
    val sharedChan = Address.newLocalAddress("../compileok/buyerseller/buyerseller.spr", 'Buyer)
    actor {
      sharedChan.bind { s =>
        s.receive('Seller) {
          case OK if (42 == 43) => // bad: guards not supported
          case Some(NotOK) => // bad: complex patterns not supported
          case _ => // bad: complex patterns not supported
        }
      }
    }
  }
}