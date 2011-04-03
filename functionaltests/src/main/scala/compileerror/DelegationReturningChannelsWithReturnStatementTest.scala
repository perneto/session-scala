package compileerror

import uk.ac.ic.doc.sessionscala.{SessionChannel, PublicPort}

/**
 * Created by: omp08
 */

object DelegationReturningChannelsWithReturnStatementTest {
  def main(args: Array[String]) {
    def myMethod(s1: SessionChannel): SessionChannel = {
      if (42.hashCode == 42) return s1
      else s1
    }

    val shared = PublicPort.newLocalPort("""
    protocol Foo {
      role Alice, Bob;
      String from Alice to Bob;
    } """, 'Alice) 

    shared.bind { s =>
      val s1 = myMethod(s)
      return
      s1 ! 'Bob -> "foo"
    }
  }
}