package compileerror

import uk.ac.ic.doc.sessionscala.{SessionChannel}

/**
 * Created by: omp08
 */

class DelegationReturningTwoChannelsInconsistentOrderTest {
  def main(args: Array[String]) {
    def myMethod(s1: SessionChannel, s2: SessionChannel): (SessionChannel, SessionChannel) = {
      if (42.hashCode == 42) (s1,s2)
      else (s2,s1)
    }
  }
}