package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{ParticipantChannel, SharedChannel}

/**
 * Created by: omp08
 */

object SessionChannelAliasingTest {
  def m {

    SharedChannel.withLocalChannel("../compileok/buyerseller/buyerseller.spr") {
      sharedChan =>

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
}