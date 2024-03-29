package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{Address, SessionChannel}, Address._

/**
 * Created by: omp08
 */

object DelegationReturningOneChannelTest {
  def main(args: Array[String]) {

    val delegation = """
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """ 
    val alice = newLocalAddress(delegation, 'Alice)
    val bob = newLocalAddress(delegation, 'Bob)

    def myMethod(s: SessionChannel): SessionChannel = {
      s ! 'Bob -> "foo"
      s
    }

    actor { startSession(alice, bob) }
    
    actor {
      alice.bind { s =>
        val s2 = myMethod(s)
        val i = s2.?[Int]('Bob)
        println("Alice received " + i)
      }
    }

    bob.bind { s =>
      s.?[String]('Alice)
      s ! ('Alice) -> 42
      println("Bob: finished")
    }
  }
}