package uk.ac.ic.doc.sessionscala

import org.scalatest.matchers.ShouldMatchers
import uk.ac.ic.doc.sessionscala.SharedChannel._
import actors.Actor._
import org.scalatest.{BeforeAndAfterEach, FunSuite}

class SharedChannelInviteSpec extends FunSuite with Timeouts with ShouldMatchers with BeforeAndAfterEach {
  test("invite not exhaustive: error") {
    val shared = createAMQPChannel(Set('Alice, 'Bob))

    intercept[IllegalArgumentException] {
      shared.invite('Alice -> localhost) // missing Bob
    }
  }

  ignore("invite of unexpected role: error") {
    val shared = createAMQPChannel(Set('Alice))
    intercept[IllegalArgumentException] {
      shared.invite('Alice -> localhost, 'Foo -> localhost)
    }
  }

  ignore("accept of unexpected role: error") {
    val shared = createAMQPChannel(Set('Alice))
    intercept[IllegalArgumentException] {
      shared.accept('Foo) { s => }
    }
  }

  def invite() = {
    val shared = createAMQPChannel(Set('Alice, 'Bob))
    shared.invite('Alice -> localhost, 'Bob -> localhost)
    shared
  }

  test("invite/accept init") {
    var aliceStarted = false; var bobStarted = false;
    val shared = invite()
    withTimeoutAndWait {
      actor { shared.accept('Alice) { s =>
        aliceStarted = true
      }}

      actor { shared.accept('Bob) { s =>
        bobStarted = true
      }}
    }

    assert(aliceStarted, "Alice did not start")
    assert(bobStarted, "Bob did not start")
  }

  ignore("invited participants can talk") {
    val shared = invite()
    withTimeoutAndWait {
      actor { shared.accept('Alice) { s =>
      }}

      actor { shared.accept('Bob) { s =>
      }}
    }
  }

  override def afterEach() {
    val chan = AMQPUtils.connectDefaults()
    try {
      chan.exchangeDelete(AMQPUtils.INIT_EXCHANGE)
      chan.queueDelete(localhost)
    } catch {
      case _ =>
    }
  }
}