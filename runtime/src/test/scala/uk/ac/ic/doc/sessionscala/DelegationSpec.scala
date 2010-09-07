package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.SharedChannel._
import actors.Actor
import Actor._

/**
 * Created by: omp08
 */

class DelegationSpec extends Timeouts {

  test("Session interleaving") {
    val chanA = createLocalChannel(Set("Alice", "Bob"))
    val chanB = createLocalChannel(Set("Bob","Carol"))
    var bothAccepted = false ; var aliceOK = false ; var carolOK = false
    var bobOK1 = false; var bobOK2 = false ; var bobOK3 = false 

    actor {
      chanA.join("Alice") { sA =>
        sA("Bob") ! "Hello from Alice"
        //println("Hello (1) from Alice sent to: " + sA("Bob"))
        aliceOK = sA("Bob").? == "Hello from Bob"
        sA("Bob") ! "Hello from Alice"
        //println("Hello (2) from Alice sent to: " + sA("Bob"))
      }
    }

    actor {
      chanA.join("Bob") { sA =>
        // println("before first receive on: " + sA)
        bobOK1 = sA("Alice").? == "Hello from Alice"

        chanB.join("Bob") { sB =>
          bothAccepted = true
          //println("before send to Alice")
          sA("Alice") ! "Hello from Bob"
          //println("sA: " + sA)
          //println("sB: " + sB)
          //println(this)
          //println("before second receive on: " + sA)
          bobOK2 = sA("Alice").? == "Hello from Alice"
          //println("before receive on: " + sB)
          bobOK3 = sB("Carol").? == "Hello from Carol"
          //println("after sB receive")
          sB("Carol") ! "Hello from Bob"

        }
      }
    }

    actor {
      chanB.join("Carol") { sB =>
        sB("Bob") ! "Hello from Carol"
        //println("Hello from Carol sent to: " + sB("Bob"))
        carolOK = sB("Bob").? == "Hello from Bob"
      }
    }

    sleep
    assert(bothAccepted, "Both sessions should be started")

    assert(aliceOK, "The message to Alice (sA) should be received")
    assert(bobOK1, "The message from Alice (sA) should be received in the outer scope")
    assert(bobOK2, "The message from Alice (sA) should be received in the inner scope")
    assert(bobOK3, "The message from Carol (sB) should be received")
    assert(carolOK, "The message to Carol (sB) should be received")
  }

  /* We might not want delegation at all with multiparty sessions?
  ignore("Simple delegation, ignoring lost messages") {
    val chanA = createLocalChannel(Set("Alice", "Bob"))
    val chanB = createLocalChannel(Set("Bob","Carol"))
    var helloOK = false ; var fortyTwoOK = false ; var fortyThreeOK = false

    actor {
      chanA.join("Alice") { sA =>
        sA("Bob") ! "Hello"
        sA("Bob") ! 42 // Now magically redirected to Carol
        fortyThreeOK = sA("Bob").? == 43
      }
    }

    actor {
      chanA.join("Bob") { sA =>
        val hello = sA("Alice").?
        helloOK = hello == "Hello"
        chanB.join("Bob") { sB =>
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
        //sA("Alice") ! 43
      }
    }

    sleep
    assert(helloOK, "Should receive non-delegated message")
    //assert(fortyTwoOK, "Should receive delegated message transparently")
    assert(fortyThreeOK)

  }
  */
}