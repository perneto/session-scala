package compileerror

import uk.ac.ic.doc.sessionscala.{SessionChannel, Port}

object IncompleteProcessBlockInsideSessionMethodTest {
  def myMethod(s1: SessionChannel) = {
    val alice = Port.newLocalPort("""
      protocol Foo {
        role Alice, Bob;
        String from Alice to Bob;
      } """, 'Alice)

    alice.bind { s =>
      // missing send
    }
  }
}