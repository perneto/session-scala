package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{inlineprotocol, SharedChannel, SessionChannel}

/**
 * Created by: omp08
 */

class DelegationReturningChannelsAndValuesTest {
  def main(args: Array[String]) {
    @inlineprotocol("""
    protocol Delegation1 {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """)
    val sharedChannel = SharedChannel.createLocalChannel(Set('Alice, 'Bob))
    @inlineprotocol("""
    protocol Delegation2 {
      role Foo, Bar;
      String from Foo to Bar;
      Boolean from Bar to Foo;
    }
    """)
    val sharedChannel2 = SharedChannel.createLocalChannel(Set('Foo, 'Bar))

    def myMethod(s1: SessionChannel, s2: SessionChannel) = {
      s1('Bob) ! "quux"
      s2('Bar) ! "quux"
      (s1, 42, s2)
    }

    actor {
      sharedChannel.join('Alice) { s =>
        sharedChannel2.join('Foo) { s2 =>
          val (s_, i, s2_) = myMethod(s, s2)
          s_('Bob).?[Int]
          s2_('Bar).?[Boolean]
        }
      }
    }


    sharedChannel.join('Bob) { s =>
      sharedChannel2.join('Bar) { s2 =>
        s('Alice).?[String]
        s('Alice) ! 42
        s2('Foo).?[String]
        s2('Foo) ! true
      }
    }
  }
}