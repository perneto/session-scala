package compileok

import uk.ac.ic.doc.sessionscala.PublicPort._
import actors.Actor.actor

object BasicAMQPChannelTest {
  def main(args: Array[String]) {
    val alice = AMQPPort("protocol Test { role Alice, Bob; String from Alice to Bob; }", 'Alice, "alice")
    val bob = AMQPPort("protocol Test { role Alice, Bob; String from Alice to Bob; }", 'Alice, "bob")
    actor { startSession(alice, bob) }
    actor {
      alice.bind { s =>
        s ! 'Bob -> "foo"
      }
    }

    bob.bind { s =>
      s.?[String]('Alice)
    }
  }
}