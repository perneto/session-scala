package compileerror

import uk.ac.ic.doc.sessionscala.SharedChannel

object SharedChannelAsMethodParameterTest {
  /*
  def main(args: Array[String]) {
    SharedChannel.withLocalChannel(
    """
    protocol Foo {
      role Alice, Bob;
    }
    """
    ) { sharedChannel =>

      foo(sharedChannel)
    }
  }
  */

  def foo(forbidden: SharedChannel) {
    /*
    This is forbidden for now, as the compiler cannot know in advance which protocol
    is associated with 'forbidden'. It could work if all SharedChannel method parameters
    were required to have a @protocol annotation (to be able to typecheck the method body),
    and were treated as linear resources in the caller scope (because of invites).

    For now, we're not allowing anything of this form.
    */
  }
}