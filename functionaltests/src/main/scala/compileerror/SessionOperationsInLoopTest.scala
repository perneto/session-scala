package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{SharedChannel}

/**
 * Created by: omp08
 */

class SessionOperationsInLoopTest {
  def main(args: Array[String]) {

    SharedChannel.withLocalChannel("""
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """) { sharedChannel =>
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