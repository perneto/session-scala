package compileerror

import uk.ac.ic.doc.sessionscala.{PublicPort}

/**
 * Created by: omp08
 */

object SessionOperationsInLoopTest {
  def main(args: Array[String]) {

    val sharedChannel = PublicPort.newLocalPort("""
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """, 'Alice)
    sharedChannel.bind { s =>
      var x = 42
      while (true) {
        s ! 'Bob -> "loop"
      }
      for (i <- 1 to 10) s ! 'Bob -> "loop"
      s.?[Int]('Bob)
    }
  }
}