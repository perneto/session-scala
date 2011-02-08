package compileerror

import uk.ac.ic.doc.sessionscala.{SessionChannel, SharedChannel}

object IncompleteProcessBlockInsideSessionMethodTest {
  def myMethod(s1: SessionChannel) = {
    SharedChannel.withLocalChannel("""
      protocol Foo {
        role Alice, Bob;
        String from Alice to Bob;
      } """) { shared =>

      shared.join('Alice) { s =>
        // missing send
      }
    }
  }
}