package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.Address._
import actors.Actor
import Actor._

/**
 * Created by: omp08
 */

class DelegationSpec extends Timeouts {

  /* We might not want delegation at all with multiparty sessions?
  ignore("Simple delegation, ignoring lost messages") {
    val chanA = createLocalChannel(Set('Alice, 'Bob))
    val chanB = createLocalChannel(Set('Bob,"Carol"))
    var helloOK = false ; var fortyTwoOK = false ; var fortyThreeOK = false

    actor {
      chanA.join('Alice) { sA =>
        sA('Bob) ! "Hello"
        sA('Bob) ! 42 // Now magically redirected to Carol
        fortyThreeOK = sA('Bob).? == 43
      }
    }

    actor {
      chanA.join('Bob) { sA =>
        val hello = sA('Alice).?
        helloOK = hello == "Hello"
        chanB.join('Bob) { sB =>
          //sB("Carol").delegate(sA)
        }
      }
    }

    actor {
      chanB.join("Carol") { sB =>
        //val sA = sB.receiveDelegated
        println("Received session channel")
        //val msg = sA.?
        println("Received delegated msg")
        //fortyTwoOK = msg == 42
        //sA('Alice) ! 43
      }
    }

    sleep
    assert(helloOK, "Should receive non-delegated message")
    //assert(fortyTwoOK, "Should receive delegated message transparently")
    assert(fortyThreeOK)

  }
  */
}