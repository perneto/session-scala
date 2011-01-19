package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{inlineprotocol, SharedChannel}

/**
 * Created by: omp08
 */

class DelegationTestDisabled {
  def main(args: Array[String]) {
    @inlineprotocol("""
    protocol Delegation {
      role Alice, Bob;
      String from Alice to Bob;
    }
    """)
    val sharedChannel = SharedChannel.createLocalChannel(Set('Alice, 'Bob))

    actor {
      sharedChannel.join('Alice) { s =>
        println("Alice: finished")
      }
    }

    actor {
      sharedChannel.join('Bob) { s =>
        println("Bob: finished")
      }
    }
  }
}