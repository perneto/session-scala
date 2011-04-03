package compileok

import uk.ac.ic.doc.sessionscala.{PublicPort, SessionChannel}, PublicPort._

/**
 * Created by: omp08
 */

object TwoChannelsAndNestedDelegationTest {
  def main(args: Array[String]) {
    val delegation1 = """
    protocol Delegation1 {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """

    val delegation2 = """
    protocol Delegation2 {
      role Foo, Bar;
      String from Foo to Bar;
      Boolean from Bar to Foo;
    }
    """

    val alice = newLocalPort(delegation1, 'Alice)
    val foo = newLocalPort(delegation2, 'Foo)
    
    def sendStrings(s1: SessionChannel, s2: SessionChannel) = {
      s1 ! 'Bob -> "quux"
      s2 ! 'Bar -> "quux"
      receiveAnswers(s2, s1)
    }

    def receiveAnswers(s2: SessionChannel, s1: SessionChannel) = {
      s2.?[Boolean]('Bar)
      s1.?[Int]('Bob)
    }

    alice.bind { s =>
      foo.bind { s2 =>
        sendStrings(s, s2)
      }
    }
  }
}