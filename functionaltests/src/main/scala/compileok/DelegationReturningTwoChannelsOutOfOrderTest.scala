package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{Address, SessionChannel}, Address._

/**
 * Created by: omp08
 */

object DelegationReturningTwoChannelsOutOfOrderTest {
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
      (s2, s1)
    }

    actor { 
      startSession(alice,bob)
      startSession(foo,bar)
    }
    
    actor {
      alice.bind { s1 =>
        foo.bind { s2 =>
        val (s2_,s1_) = myMethod(s1, s2)
        s1_.?[Int]('Bob)
        s2_.?[Boolean]('Bar)
        }
      }
    }

    bob.bind { s1 =>
      bar.bind { s2 =>
        s1.?[String]('Alice)
        s1 ! 'Alice -> 42
        s2.?[String]('Foo)
        s2 ! 'Foo -> true
      }
    }
 
  }
}