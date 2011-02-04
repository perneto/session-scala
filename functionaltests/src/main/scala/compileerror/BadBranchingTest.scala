package compileerror

import uk.ac.ic.doc.sessionscala.{SharedChannel}
import compileok.buyerseller.{NotOK, OK}
import actors.Actor.actor

/**
 * Created by: omp08
 */

object BadBranchingTest {
  def m {
    SharedChannel.withLocalChannel("../compileok/buyerseller/buyerseller.scribble") {
      sharedChan =>
      actor {
        sharedChan.join('Buyer) { s =>
          s('Seller).receive {
            case OK if (42 == 43) => // bad: guards not supported
            case Some(NotOK) => // bad: complex patterns not supported
            case _ => // bad: complex patterns not supported
          }
        }
      }
    }
  }
}