package compileerror

import uk.ac.ic.doc.sessionscala.{Address}

object SessionOperationsInClosureTest {
  def main(args: Array[String]) {
    val sharedChannel = Address.newLocalPort(
    """
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """, 'Alice)

    sharedChannel.bind { s =>
      var x = 42
      for (i <- 1 to 10) s ! 'Bob -> "loop"
      s.?[Int]('Bob)
    }
  }
}