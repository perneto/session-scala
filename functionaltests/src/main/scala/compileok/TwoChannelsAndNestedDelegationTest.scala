package compileok

import uk.ac.ic.doc.sessionscala.{inlineprotocol, SharedChannel, SessionChannel}

/**
 * Created by: omp08
 */

class TwoChannelsAndNestedDelegationTest {
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

    def sendStrings(s1: SessionChannel, s2: SessionChannel) = {
      s1('Bob) ! "quux"
      s2('Bar) ! "quux"
      receiveAnswers(s2, s1)
    }

    def receiveAnswers(s2: SessionChannel, s1: SessionChannel) = {
      s2('Bar).?[Boolean]
      s1('Bob).?[Int]
    }

    sharedChannel.join('Alice) { s =>
      sharedChannel2.join('Foo) { s2 =>
        sendStrings(s, s2)
      }
    }

  }
}