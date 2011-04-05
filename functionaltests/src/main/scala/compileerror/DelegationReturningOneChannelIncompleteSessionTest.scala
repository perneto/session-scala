package compileerror

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{Address, SessionChannel}

/**
 * Created by: omp08
 */

object DelegationReturningOneChannelIncompleteSessionTest {
  def main(args: Array[String]) {
    val delegation = """
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """
    val alice = Address.newLocalAddress(delegation, 'Alice)
    val bob = Address.newLocalAddress(delegation, 'Bob)
                                      
    def myMethod(s: SessionChannel): SessionChannel = {
      s ! 'Bob -> "foo"
      s
    }

    actor {
      alice.bind { s =>
        val s2 = myMethod(s)
        // missing: s2('Bob).?[Int]
      }
    }

    actor {
      bob.bind { s =>
        s.?[String]('Alice)
        s ! 'Alice -> 42
        println("Bob: finished")
      }
    }
  }
}