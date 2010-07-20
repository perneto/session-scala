package compileerror

import uk.ac.ic.doc.sessionscala.{protocol, SharedChannel}
import compileok.buyerseller.{NotOK, OK}
import actors.Actor.actor

/**
 * Created by: omp08
 */

object BadBranching {
  def m {
    @protocol("buyerseller.scribble")
    val sharedChan = SharedChannel.createLocalChannel(Set("Buyer", "Seller"))

    actor {
      sharedChan.accept("Buyer") { s =>
        s.receive {
          case OK if (42 == 43) => // bad: guards not supported
          case Some(NotOK) => // bad: complex patterns not supported
          case _ => // bad: complex patterns not supported
        }
      }
    }

  }
}