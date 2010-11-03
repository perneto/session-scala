package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{ParticipantChannel, protocol, SharedChannel}

/**
 * Created by: omp08
 */

object BadAssignment {
  def m {
    @protocol("../buyerseller.scribble")
    val sharedChan = SharedChannel.createLocalChannel(Set('Buyer, 'Seller))

    actor {
      sharedChan.join('Buyer) { s =>
        val s1 = s // wrong
      }
    }

    sharedChan.join('Seller) { s =>
      var s1: (Symbol => ParticipantChannel) = null
      s1 = s // wrong
    }

  }
}