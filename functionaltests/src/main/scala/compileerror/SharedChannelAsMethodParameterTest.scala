package compileerror

import uk.ac.ic.doc.sessionscala.Port

object SharedChannelAsMethodParameterTest {
  /*
  def main(args: Array[String]) {
    Port.withLocalChannel(
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

  def foo(forbidden: Port) {
    /*
    This is forbidden for now, as the compiler cannot know in advance which protocol
    is associated with 'forbidden'. It could work if all Port method parameters
    were required to have a @protocol annotation (to be able to typecheck the method body),
    and were treated as linear resources in the caller scope (because of invites).

    We could also infer what capabilities each such method uses in DefDef pass,
    and consume them at once in JoinBlockPass (sounds better).

    For now, we're not allowing anything of this form.
    */
  }
}