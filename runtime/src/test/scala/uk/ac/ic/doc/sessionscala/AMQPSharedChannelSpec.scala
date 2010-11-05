package uk.ac.ic.doc.sessionscala

import org.scalatest.matchers.ShouldMatchers
import uk.ac.ic.doc.sessionscala.SharedChannel._
import actors.Actor._
import org.scalatest.{BeforeAndAfterEach, FunSuite, BeforeAndAfterAll}

class SharedChannelInviteSpec extends FunSuite with Timeouts with ShouldMatchers with BeforeAndAfterEach {
  test("invite not exhaustive: error") {
    val shared = createAMQPChannel(Set('Alice, 'Bob))
    intercept[IllegalArgumentException] {
      shared.invite('Alice -> localhost) // missing Bob
    }
  }

  test("invite of unexpected role: error") {
    val shared = createAMQPChannel(Set('Alice))
    intercept[IllegalArgumentException] {
      shared.invite('Alice -> localhost, 'Foo -> localhost)
    }
  }

  test("accept of unexpected role: error") {
    println("create channel")
    val shared = createAMQPChannel(Set('Alice))
    println("created")
    withTimeout(1000) {
      println("in timeout")
      intercept[IllegalArgumentException] {
        println("calling accept")
        shared.accept('Foo) { s => }
        println("called accept")
      }
    }
  }

  def invite() = {
    val shared = createAMQPChannel(Set('Alice, 'Bob))
    shared.invite('Alice -> localhost, 'Bob -> localhost)
    shared
  }

  test("invite/accept init") {
    var aliceStarted = false; var bobStarted = false;
    withTimeoutAndWait {
      val shared = invite()
      actor { shared.accept('Alice) { s =>
        aliceStarted = true
      }}

      actor { shared.accept('Bob) { s =>
        bobStarted = true
      }}
    }

    assert(aliceStarted, "Alice did not start")
    assert(bobStarted, "Bob did not start")
    //todo: assert exchanges/queues have correct names
  }

  test("accept when invited for another role: blocks") {
    var didRun = false
    expectTimeout(1000) {
      val shared = createAMQPChannel(Set('Alice, 'Bob))
      shared.invite('Alice -> localhost, 'Bob -> "foohost")
      shared.accept('Bob) { s =>
        didRun = true
      }
    }
    assert(!didRun, "Bob should not have started as there was no invite for Bob on localhost")
  }

  ignore("session channel has references for all roles") {
    withTimeout(1000) {
      val shared = invite()
      shared.accept('Alice) { s =>
        s('Bob) // just ensures that the mapping is defined for 'Bob
        s('Alice)
      }
    }
  }

  ignore("invited participants can talk") {
    var aliceOk = false; var bobOk = false
    withTimeoutAndWait {
      val shared = invite()
      actor { shared.accept('Alice) { s =>
        s('Bob) ! 42
        aliceOk = s('Bob).? == "foo"
      }}

      actor { shared.accept('Bob) { s =>
        s('Alice) ! "foo"
        bobOk = s('Alice).? == 42
      }}
    }
    assert(aliceOk, "Alice was not able to communicate")
    assert(bobOk, "Bob was not able to communicate")
  }

  override def afterEach() {
    val chan = AMQPUtils.connectDefaults()
    try {
      chan.exchangeDelete(AMQPUtils.INIT_EXCHANGE)
      chan.queueDelete(localhost)
      chan.exchangeDelete("s1")
      chan.queueDelete("s1Alice")
      chan.queueDelete("s1Bob")
    } catch {
      case _ =>
    }
  }
}
