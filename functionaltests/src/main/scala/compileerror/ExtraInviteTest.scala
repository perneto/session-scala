package compileerror

import uk.ac.ic.doc.sessionscala.Address._

object ExtraInviteTest {
  def main(args: Array[String]) {
    val proto = """
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """
    val alice = newLocalPort(proto, 'Alice)
    val bob = newLocalPort(proto, 'Bob)
    startSession(alice, bob, bob)
  }
}