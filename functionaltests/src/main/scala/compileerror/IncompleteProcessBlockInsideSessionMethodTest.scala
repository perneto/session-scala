package compileerror

import uk.ac.ic.doc.sessionscala.{SessionChannel, PublicPort}

object IncompleteProcessBlockInsideSessionMethodTest {
  def myMethod(s1: SessionChannel) = {
    val alice = PublicPort.newLocalPort("""
      protocol Foo {
        role Alice, Bob;
        String from Alice to Bob;
      } """, 'Alice)

    alice.bind { s =>
      // missing send
    }
  }
}