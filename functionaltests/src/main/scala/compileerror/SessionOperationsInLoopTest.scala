package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{inlineprotocol, SharedChannel, SessionChannel}

/**
 * Created by: omp08
 */

class SessionOperationsInLoopTest {
  def main(args: Array[String]) {
    @inlineprotocol("""
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """)
    val sharedChannel = SharedChannel.createLocalChannel(Set('Alice, 'Bob))

    actor {
      sharedChannel.join('Alice) { s =>
        var x = 42
        while (true) {
          s('Bob) ! "loop"
        }
        for (i <- 1 to 10) s('Bob) ! "loop"
        s('Bob).?[Int]
      }
    }
  }
}