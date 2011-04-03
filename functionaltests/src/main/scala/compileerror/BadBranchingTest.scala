package compileerror

import uk.ac.ic.doc.sessionscala.{PublicPort}
import compileok.buyerseller.{NotOK, OK}
import actors.Actor.actor

/**
 * Created by: omp08
 */

object BadBranchingTest {
  def m() {
    val sharedChan = PublicPort.newLocalPort("../compileok/buyerseller/buyerseller.spr", 'Buyer)
    actor {
      sharedChan.bind { s =>
        s.receive('Seller) {
          case ('Seller, OK) if (42 == 43) => // bad: guards not supported
          case ('Seller, Some(NotOK)) => // bad: complex patterns not supported
          case _ => // bad: complex patterns not supported
        }
      }
    }
  }
}