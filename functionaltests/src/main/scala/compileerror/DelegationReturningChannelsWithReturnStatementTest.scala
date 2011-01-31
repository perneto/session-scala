package compileerror

import uk.ac.ic.doc.sessionscala.{inlineprotocol, SessionChannel, SharedChannel}

/**
 * Created by: omp08
 */

class DelegationReturningChannelsWithReturnStatementTest {
  def main(args: Array[String]) {
    def myMethod(s1: SessionChannel): SessionChannel = {
      if (42.hashCode == 42) return s1
      else s1
    }

    @inlineprotocol("""
    protocol Foo {
      role Alice, Bob;
      String from Alice to Bob;
    } """)
    val shared = SharedChannel.createLocalChannel(Set('Alice, 'Bob))
    shared.join('Alice) { s =>
      val s1 = myMethod(s)
      return
      s1('Bob) ! "foo"
    }
  }
}