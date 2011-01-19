package uk.ac.ic.doc.sessionscala

import org.scalatest.matchers.ShouldMatchers
import uk.ac.ic.doc.sessionscala.SharedChannel._
import actors.Actor._
import org.scalatest.{BeforeAndAfterEach, FunSuite, BeforeAndAfterAll}

class SharedChannelInviteSpec extends FunSuite with Timeouts with ShouldMatchers with BeforeAndAfterEach {

  test("invite not exhaustive: error") {
    withAMQPChannel(Set('Alice, 'Bob)) { shared =>
      intercept[IllegalArgumentException] {
        shared.invite("", 'Alice -> localhost) // missing Bob
      }
    }
    Thread.sleep(500) // race condition between afterEach and invitation receiver, afterEach deletes queue after receiver created it
  }

  test("invite of unexpected role: error") {
    withAMQPChannel(Set('Alice)) { shared =>
      intercept[IllegalArgumentException] {
        shared.invite("", 'Alice -> localhost, 'Foo -> localhost)
      }
    }
    Thread.sleep(500) // race condition between afterEach and invitation receiver, afterEach deletes queue after receiver created it
  }

  test("accept of unexpected role: error") {
    withAMQPChannel(Set('Alice)) { shared =>
      withTimeout(1000) {
        intercept[IllegalArgumentException] {
          shared.accept('Foo) { s => }
        }
      }
    }
  }

  test("invite/accept init") {
    var aliceStarted = false; var bobStarted = false;
    withShared { shared =>
      withTimeoutAndWait(2000,500) {
        actor { shared.accept('Alice) { s =>
          aliceStarted = true
        }}

        shared.accept('Bob) { s =>
          bobStarted = true
        }
      }
    }
    assert(aliceStarted, "Alice did not start")
    assert(bobStarted, "Bob did not start")
  }

  ignore("accept when invited for another role: blocks") {
    var didRun = false
    withAMQPChannel(Set('Alice, 'Bob)) { shared =>
      expectTimeout(1000) {
        shared.invite("", 'Alice -> localhost, 'Bob -> "foohost")
        shared.accept('Bob) { s =>
          didRun = true
        }
      }
    }
    assert(!didRun, "Bob should not have started as there was no invite for Bob on localhost")
  }

  def withShared(block: SharedChannel => Unit) {
    withAMQPChannel(Set('Alice, 'Bob)) { shared =>
      shared.invite("", 'Alice -> localhost, 'Bob -> localhost)
      block(shared)
    }
  }
  test("session channel has references for all roles apart from own role") {
    withShared { shared =>
      withTimeout(1000) {
        shared.accept('Alice) { s =>
          s('Bob) // just ensures that the mapping is defined for 'Bob
        }
      }
    }
  }

  test("invited participants can talk") {
    var aliceOk = false; var bobOk = false
    withShared { shared =>
      withTimeoutAndWait {
        actor { shared.accept('Alice) { s =>
          println("ALICE STARTED")
          s('Bob) ! 4242
          println("ALICE SENT 4242 TO BOB")
          val (_label,recv) = s('Bob).??
          println("ALICE RECEIVED: " + recv)
          aliceOk = recv == "foo"
        }}

        shared.accept('Bob) { s =>
          println("BOB STARTED")
          s('Alice) ! "foo"
          println("BOB SENT foo TO ALICE")
          val (_label,recv) = s('Alice).??
          
          println("BOB RECEIVED: " + recv)
          bobOk = recv == 4242
        }
      }
    }
    assert(aliceOk, "Alice was not able to communicate")
    assert(bobOk, "Bob was not able to communicate")
  }

  override def afterEach() {
    val chan = AMQPUtils.connectDefaults()
    try {
      //chan.exchangeDelete(AMQPUtils.INIT_EXCHANGE) using standard amq.direct now, so no delete
      chan.queueDelete(localhost)
      chan.exchangeDelete("s1")
      chan.queueDelete("s1Alice")
      chan.queueDelete("s1Bob")
      chan.queueDelete("foohost")
    } catch {
      case _ =>
    } finally {
      chan.getConnection.close()
    }
  }
}
