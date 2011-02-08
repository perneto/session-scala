package compileerror

import uk.ac.ic.doc.sessionscala.SharedChannel

object SharedChannelAssignmentAliasingTest {
  def main(args: Array[String]) {
    SharedChannel.withLocalChannel(
    """
    protocol Foo {
      role Alice, Bob;
    }
    """
    ) { sharedChannel =>
      // Static checking of invites requires linearity.
      val shared2 = sharedChannel
    }
  }
}