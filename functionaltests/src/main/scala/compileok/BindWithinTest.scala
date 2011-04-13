package compileok

import uk.ac.ic.doc.sessionscala.Address._
import actors.Actor.actor

object BindWithinTest {
  def main(args: Array[String]) {
    val alice = AMQPAddress("protocol Test { role Alice, Bob; String from Alice to Bob; }", 'Alice, "alice")
    val bob = AMQPAddress("protocol Test { role Alice, Bob; String from Alice to Bob; }", 'Bob, "bob")
    actor { startSession(alice, bob) }
    actor {
      alice.bindWithin(500) { s =>
        s ! 'Bob -> "foo"
      }
    }

    bob.bind { s =>
      s.?[String]('Alice)
    }
  }
}