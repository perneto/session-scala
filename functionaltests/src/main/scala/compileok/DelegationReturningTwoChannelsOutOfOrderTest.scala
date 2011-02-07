package compileok

import actors.Actor.actor
import uk.ac.ic.doc.sessionscala.{SharedChannel, SessionChannel}

/**
 * Created by: omp08
 */

object DelegationReturningTwoChannelsOutOfOrderTest {
  def main(args: Array[String]) {
    SharedChannel.withLocalChannel("""
    protocol Delegation1 {
      role Alice, Bob;
      String from Alice to Bob;
      Int from Bob to Alice;
    }
    """) { sharedChannel =>
      SharedChannel.withLocalChannel("""
      protocol Delegation2 {
        role Foo, Bar;
        String from Foo to Bar;
        Boolean from Bar to Foo;
      }
      """) { sharedChannel2 =>

        def myMethod(s1: SessionChannel, s2: SessionChannel) = {
          s1('Bob) ! "quux"
          s2('Bar) ! "quux"
          (s2, s1)
        }

        actor {
          sharedChannel.join('Alice) { s1 =>
            sharedChannel2.join('Foo) { s2 =>
            val (s2_,s1_) = myMethod(s1, s2)
            s1_('Bob).?[Int]
            s2_('Bar).?[Boolean]
            }
          }
        }

        sharedChannel.join('Bob) { s1 =>
          sharedChannel2.join('Bar) { s2 =>
            s1('Alice).?[String]
            s1('Alice) ! 42
            s2('Foo).?[String]
            s2('Foo) ! true
          }
        }
      }
    }
  }
}