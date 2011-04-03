package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{PublicPort, SessionChannel}, PublicPort._

/**
 * Created by: omp08
 */

object DelegationReturningChannelsAndValuesTest {
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
    val bob = newLocalPort(delegation1, 'Bob)
    val foo = newLocalPort(delegation2, 'Foo)
    val bar = newLocalPort(delegation2, 'Bar)
    
    def myMethod(s1: SessionChannel, s2: SessionChannel) = {
      s1 ! 'Bob -> "quux"
      s2 ! 'Bar -> "quux"
      (s1, 42, s2)
    }

    actor { 
      startSession(alice,bob)
      startSession(foo,bar)
    }
    
    actor {
      alice.bind { s =>
        foo.bind { s2 =>
          val (s_, i, s2_) = myMethod(s, s2)
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