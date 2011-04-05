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
    val alice = newLocalAddress(proto, 'Alice)
    val bob = newLocalAddress(proto, 'Bob)
    startSession(alice, bob, bob)
  }
}