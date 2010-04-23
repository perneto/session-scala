package uk.ac.ic.doc.sessionscala.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import uk.ac.ic.doc.sessionscala.sessionops._
import scala.concurrent.ops.spawn
import actors.Actor.?

class SessionopsSpec extends FunSuite with ShouldMatchers with Timeouts {
  def timeout[T](b: => T) = withTimeoutAndWait(3000, 100)(b)
  def xor(a: Boolean, b: Boolean) = (a && !b)||(!a && b)

  val chan1 = createLocalChannel(Set("Foo"))
  val chan2= createLocalChannel(Set("Foo","Bar"))

  test("complains if set of roles is empty") {
    intercept[IllegalArgumentException] {
      val chan = createLocalChannel(Set())
    }
  }

  test("complains if accept called with undefined role") {
    var didRunBar = false
    intercept[IllegalArgumentException] {
      chan1.accept("Bar") { _ => didRunBar = true }
    }
    assert(!didRunBar)
  }

  test("starts actors Foo and Bar after two calls to accept if expecting both") {
    var didRunFoo = false ; var didRunBar = false
    timeout {
      spawn {
        chan2.accept("Foo") { _ => didRunFoo = true }
      }
      chan2.accept("Bar") { _ => didRunBar = true}

    }
    assert(didRunFoo) ; assert(didRunBar)
  }

  test("starts one actor after one accept if expecting one but two calls to accept") {
    val chan = createLocalChannel(Set("Foo","Bar"))
    var didRun1 = false ; var didRun2 = false ; var didRunBar = true
    timeout {
      spawn {
        chan.accept("Bar") { _ => didRunBar = true}
      }
      spawn {
        chan.accept("Foo") { _ => didRun1 = true }
      }
      chan.accept("Foo") { _ => didRun2 = true }
    }
    assert(didRunBar, "bar should have started")
    assert(xor(didRun1,didRun2), "should run either. ran 1: " + didRun1 + ", ran 2: " + didRun2)
  }

  test("sets up the session properly") {
    var fooRecv = false ; var barRecv = false
    val chan = createLocalChannel(Set("Foo","Bar"))
    
    timeout {
      spawn {
        chan.accept("Foo") { s =>
          s("Bar") ! 42
          println("sent to Bar")
          fooRecv = ? == 43
        }
      }
      chan.accept("Bar") { s =>
        s("Foo") ! 43
        println("sent to Foo")
        barRecv = ? == 42
      }
    }
    assert(fooRecv)
    assert(barRecv)
  }

  ignore("shared channel doesn't blow the stack") {
    for (i <- List.range(1,1000000)) chan1.accept("Foo") { _ => }
  }
}