package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{inlineprotocol, SharedChannel, SessionChannel}

/**
 * Created by: omp08
 */

class DelegationReturningOneChannelTest {
  def main(args: Array[String]) {
    @inlineprotocol("""
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """)
    val sharedChannel = SharedChannel.createLocalChannel(Set('Alice, 'Bob))

    def myMethod(s: SessionChannel): SessionChannel = {
      s('Bob) ! "foo"
      s
    }

    actor {
      sharedChannel.join('Alice) { s =>
        val s2 = myMethod(s)
        val i = s2('Bob).?[Int]
        println("Alice received " + i)
      }
    }

    actor {
      sharedChannel.join('Bob) { s =>
        s('Alice).?[String]
        s('Alice) ! 42
        println("Bob: finished")
      }
    }
  }
}