package compileerror

import uk.ac.ic.doc.sessionscala.SharedChannel

object SharedChannelInvitesInClosureTest {
  def main(args: Array[String]) {
    SharedChannel.withLocalChannel(
    """
    protocol Foo {
      role Alice, Bob;
    }
    """
    ) { sharedChannel =>

      for (i <- 1 to 10) sharedChannel.invite('Alice -> SharedChannel.localhost)
    }
  }
}