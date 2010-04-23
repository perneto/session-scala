package uk.ac.ic.doc.sessionscala.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import uk.ac.ic.doc.sessionscala.sessionops._
import scala.concurrent.ops.spawn

class SessionopsSpec extends FunSuite with ShouldMatchers with Timeouts {
  def timeout[T](b: => T) = withTimeoutAndWait(2000, 100)(b)
  def xor(a: Boolean, b: Boolean) = (a && !b)||(!a && b)

  val chan = createLocalChannel(Set("Foo"))

  test("complains if set of roles is empty") {
    intercept[IllegalArgumentException] {
      val chan = createLocalChannel(Set())
    }
  }

  test("complains if accept called with undefined role") {
    var didRunBar = false
    intercept[IllegalArgumentException] {
      chan.accept("Bar") { _ => didRunBar = true }
    }
    assert(!didRunBar)
  }

  test("starts actors Foo and Bar after two calls to accept if expecting both") {
    var didRunFoo = false ; var didRunBar = false
    timeout {
      val chan = createLocalChannel(Set("Foo", "Bar"))
      spawn {
        chan.accept("Foo") { _ => didRunFoo = true }
      }
      chan.accept("Bar") { _ => didRunBar = true}

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

  ignored("shared channel doesn't blow the stack") {
    for (i <- List.range(1,1000000)) chan.accept("Foo") { _ => }
  }
}