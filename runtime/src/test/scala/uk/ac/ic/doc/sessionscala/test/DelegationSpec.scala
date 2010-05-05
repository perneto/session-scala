package uk.ac.ic.doc.sessionscala.test

import uk.ac.ic.doc.sessionscala.sessionops._
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
      chanA.accept("Alice") { sA =>
        sA("Bob") ! "Hello from Alice"
        //println("Hello (1) from Alice sent to: " + sA("Bob"))
        aliceOK = sA.? == "Hello from Bob"
        sA("Bob") ! "Hello from Alice"
        //println("Hello (2) from Alice sent to: " + sA("Bob"))
      }
    }

    actor {
      chanA.accept("Bob") { sA =>
        // println("before first receive on: " + sA)
        bobOK1 = sA.? == "Hello from Alice"

        chanB.accept("Bob") { sB =>
          bothAccepted = true
          //println("before send to Alice")
          sA("Alice") ! "Hello from Bob"
          //println("sA: " + sA)
          //println("sB: " + sB)
          //println(this)
          //println("before second receive on: " + sA)
          bobOK2 = sA.? == "Hello from Alice"
          //println("before receive on: " + sB)
          bobOK3 = sB.? == "Hello from Carol"
          //println("after sB receive")
          sB("Carol") ! "Hello from Bob"

        }
      }
    }

    actor {
      chanB.accept("Carol") { sB =>
        sB("Bob") ! "Hello from Carol"
        //println("Hello from Carol sent to: " + sB("Bob"))
        carolOK = sB.? == "Hello from Bob"
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


  ignore("Delegation") {
    val chanA = createLocalChannel(Set("Alice", "Bob"))
    val chanB = createLocalChannel(Set("Bob","Carol"))
    var helloOK = false ; var fortyTwoOk = false
    actor {
      chanA.accept("Alice") { sA =>
        sA("Bob") ! "Hello"
        sA("Bob") ! 42
      }
    }

    actor {
      chanA.accept("Bob") { sA =>
        val hello = sA.?
        helloOK = hello == "Hello"
        chanB.accept("Bob") { sB =>
          sB("Carol") !  sA("Alice")
        }
      }
    }

    actor {
      chanB.accept("Carol") { sB =>
        val alice = sB.?.asInstanceOf[Actor]
        val msg = ?
        fortyTwoOk = msg == 42
      }
    }

    sleep
    assert(helloOK, "Should receive non-delegated message")
    assert(fortyTwoOk, "Should receive delegated message transparently")

  }

}