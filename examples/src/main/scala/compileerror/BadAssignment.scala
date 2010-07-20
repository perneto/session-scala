package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{SessionChannel, protocol, SharedChannel}

/**
 * Created by: omp08
 */

object BadAssignment {
  def m {
    @protocol("../buyerseller.scribble")
    val sharedChan = SharedChannel.createLocalChannel(Set("Buyer", "Seller"))

    actor {
      sharedChan.accept("Buyer") { s =>
        val s1 = s // wrong
      }
    }

    sharedChan.accept("Seller") { s =>
      var s1: SessionChannel = null
      s1 = s // wrong
    }

  }
}