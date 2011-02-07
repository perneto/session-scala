package compileerror

import uk.ac.ic.doc.sessionscala.SharedChannel
import SharedChannel.localhost

object ExtraInviteTest {
  def main(args: Array[String]) {
    SharedChannel.withLocalChannel(
    """
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """
    ) { sharedChannel =>

      sharedChannel.invite('Alice -> localhost, 'Alice -> localhost)
    }
  }
}