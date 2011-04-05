package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{Address, SessionChannel}
import uk.ac.ic.doc.sessionscala.Address._

/**
 * Created by: omp08
 */

object DelegationReturningTwoChannelsTest {
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

    val alice = newLocalAddress(delegation1, 'Alice)
    val bob = newLocalAddress(delegation1, 'Bob)
    val foo = newLocalAddress(delegation2, 'Foo)
    val bar = newLocalAddress(delegation2, 'Bar)

    def myMethod(s1: SessionChannel, s2: SessionChannel) = {
      s1 ! 'Bob -> "quux"
      s2 ! 'Bar -> "quux"
      (s1, s2)
    }

    actor {
      alice.bind { s =>
        foo.bind { s2 =>
        val (s_,s2_) = myMethod(s, s2)
        s_.?[Int]('Bob)
        s2_.?[Boolean]('Bar)
        }
      }
    }

    bob.bind { s =>
      bar.bind { s2 =>
        s.?[String]('Alice)
        s ! 'Alice -> 42
        s2.?[String]('Foo)
        s2 ! 'Foo -> true
      }
    }
  }
}