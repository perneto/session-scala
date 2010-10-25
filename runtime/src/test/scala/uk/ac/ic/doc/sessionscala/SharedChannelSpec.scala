package uk.ac.ic.doc.sessionscala

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import uk.ac.ic.doc.sessionscala.SharedChannel._
import actors.Actor._

class SharedChannelSpec extends FunSuite with ShouldMatchers with Timeouts {
  def xor(a: Boolean, b: Boolean) = (a && !b)||(!a && b)
  
  val chan1 = createLocalChannel(Set("Foo"))
  val chan2 = createLocalChannel(Set("Foo","Bar"))

  test("complains if set of roles is empty") {
    intercept[IllegalArgumentException] {
      val chan = createLocalChannel(Set())
    }
  }

  test("complains if join called with undefined role") {
    intercept[IllegalArgumentException] {
      chan1.join("Quux") { _ => fail("Should not start Quux") }
    }
  }

  test("accepts Foo and Bar after two calls to join if expecting both") {
    var didRunFoo = false ; var didRunBar = false
    actor {
      chan2.join("Foo") { _ => didRunFoo = true }
    }
    actor {
      chan2.join("Bar") { _ => didRunBar = true}
    }
    sleep
    assert(didRunFoo)
    assert(didRunBar)
  }

  test("allows encoding of race condition with 2 join calls for same role") {
    val chan = createLocalChannel(Set("Foo","Bar"))
    var didRun1 = false ; var didRun2 = false ; var didRunBar = true
    withTimeoutAndWait {
      actor {
        chan.join("Bar") { _ => didRunBar = true}
      }
      actor {
        chan.join("Foo") { _ => didRun1 = true }
      }
      actor {
        chan.join("Foo") { _ => didRun2 = true }
      }
    }
    assert(didRunBar, "bar should have started")
    assert(xor(didRun1,didRun2), "should run either. ran 1: " + didRun1 + ", ran 2: " + didRun2)
  }

  test("sets up the session so actors can be messaged through session map") {
    var fooRecv = false ; var barRecv = false
    val chan = createLocalChannel(Set("Foo","Bar"))
    
      actor {
        chan.join("Foo") { s =>
          s("Bar") ! 42
          fooRecv = s("Bar").? == 43
        }
      }
      actor {
        chan.join("Bar") { s =>
          s("Foo") ! 43
          barRecv = s("Foo").? == 42
        }
      }
    sleep
    assert(fooRecv, "Foo should have received message from Bar")
    assert(barRecv, "Bar should have received message from Foo")
  }

  case object Foo      
  test("doesn't interfere with standard actor messaging") {
    var fooReceived = false
    val fooActor = actor {
      chan1.join("Foo") { s =>
        receive {
          case Foo => fooReceived = true
        }
      }
    }

    fooActor ! Foo
    sleep
    assert(fooReceived)
  }

  test("sequence of two message receives out of order works") {
    val chan = createLocalChannel(Set("Foo", "Bar", "Quux"))
    var barOk = false; var quuxOk = false
    actor {
      chan.join("Foo") { s =>
        barOk = s("Bar").?[Int] == 42
        quuxOk = s("Quux").?[Char] == 'a'
      }

    }

    actor { chan.join("Bar") { s =>
      Thread.sleep(100)
      s("Foo") ! 42
    }}

    actor { chan.join("Quux") { s =>
      s("Foo") ! 'a'
    }}

    Thread.sleep(500)
    assert(barOk)
    assert(quuxOk)
  }

  // Too long for routine testing
  ignore("shared channel doesn't blow the stack") {
    for (i <- List.range(1,1000000)) {
      actor { chan1.join("Foo") { _ => } }
    }
  }
}
