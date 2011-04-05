package compileerror

import uk.ac.ic.doc.sessionscala.{SessionChannel, Address}

object IncompleteProcessBlockInsideSessionMethodTest {
  def myMethod(s1: SessionChannel) = {
    val alice = Address.newLocalAddress("""
      protocol Foo {
        role Alice, Bob;
        String from Alice to Bob;
      } """, 'Alice)

    alice.bind { s =>
      // missing send
    }
  }
}