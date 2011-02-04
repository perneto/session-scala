package compileok

import uk.ac.ic.doc.sessionscala.SharedChannel._
import actors.Actor.actor

object BasicAMQPChannelTest {
  def main(args: Array[String]) {
    withAMQPChannel("protocol Test { role Alice, Bob; String from Alice to Bob; }") { sharedChan =>
      sharedChan.invite("", 'Alice -> localhost, 'Bob -> localhost)

      actor {
        sharedChan.accept('Alice) { s =>
          s('Bob) ! "foo"
        }
      }

      sharedChan.accept('Bob) { s =>
        s('Alice).?[String]
      }
    }
  }
}