package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.PublicPort._
import org.scalatest.{BeforeAndAfterEach, FunSuite, Tag}
import concurrent.ops.spawn
import actors.Actor, Actor._

class PublicPortSpec extends FunSuite with Timeouts {

  override def nestedSuites = List(
    //new SessionPortSpecImpl("AMQP", AMQPPort(_, _, "myqueue"), AMQPPort(_, _, "otherqueue"))//,
    new SessionPortSpecImpl("Shared Mem", newLocalPort, newLocalPort)
  )

  class SessionPortSpecImpl(name: String,
                            createDefaultPort: (String, Symbol) => PublicPort,
                            createOtherPort: (String, Symbol) => PublicPort)
          extends FunSuite with BeforeAndAfterEach {
    var alice: PublicPort = null
    var bob: PublicPort = null


    override def suiteName = "SessionPortSpec: "+name+" implementation"

    override def beforeEach() {
      alice = createDefaultPort("protocol Test { role Alice, Bob; }", 'Alice)
      bob = createDefaultPort("protocol Test { role Alice, Bob; }", 'Bob)
    }

    test("non-exhaustive invite: no runtime error (should be checked by compiler). Timeouts since no bound process", Tag("timeouts")) {
      expectTimeout(500) {
        startSession(alice)
      }
      println("finished testcase")
    }

    test("role not in protocol: error") {
      println("started next")
      intercept[IllegalArgumentException] {
        createDefaultPort("protocol Test { role Alice, Bob; }", 'Foo)
      }
    }

    test("complains if protocol is not valid Scribble") {
      intercept[IllegalArgumentException] {
        createDefaultPort("foobar", 'Foo)
      }
    }

    test("complains if protocol does not have at least 1 role") {
      intercept[IllegalArgumentException] {
        createDefaultPort("protocol P {}", 'Foo)
      }
    }

    test("bind when not invited: timeouts", Tag("timeouts")) {
      var didRun = false
      val otherAlice = createOtherPort("protocol Test { role Alice, Bob; }", 'Alice)
      val otherBob = createOtherPort("protocol Test { role Alice, Bob; }", 'Bob)
      expectTimeout(1000) {
        spawn { startSession(otherAlice, otherBob) }

        alice.bind { s => didRun = true
        println("didRun!!")
        }
      }

      assert(!didRun, "Alice should not have started as there was no invite for Alice on default test port")
    }

    test("startSession/bind correctly starts session", Tag("init")) {
      var aliceStarted = false; var bobStarted = false;
      println("starting test")
      withTimeoutAndWait {
        spawn {
          println("calling startSession")
          startSession(alice, bob)
        }
        println("before alice spawn")
        spawn {
          println("calling bind on alice")
          alice.bind { s =>
            println("in alice")
            aliceStarted = true
          }
        }

        println("calling bind on bob")
        bob.bind { s =>
          println("in bob")
          bobStarted = true
        }
        println("bob exited")
      }
      assert(aliceStarted, "Alice did not start")
      assert(bobStarted, "Bob did not start")
    }

    test("invited participants can talk", Tag("talk")) {
      var aliceOk = false; var bobOk = false
      withTimeoutAndWait {
        spawn { startSession(alice, bob) }

        spawn { alice.bind { s =>
          s ! 'Bob -> 42
          val recv = s.?[String]('Bob)
          aliceOk = recv == "foo"
        }}

        bob.bind { s =>
          val recv = s.?[Int]('Alice)
          bobOk = recv == 42
          s ! 'Alice -> "foo"
        }
      }
      assert(aliceOk, "Alice was not able to communicate")
      assert(bobOk, "Bob was not able to communicate")
    }

    test("syntax for labels using implicit conversion", Tag("labels")) {
      var aliceOk = false; var bobOk = false ; var bobOk2 = false
      withTimeoutAndWait {
        spawn { startSession(alice, bob) }

        spawn { alice.bind { s =>
          s ! 'Bob -> 'foo(42)
          val recv = s.?[(String,Int)]('Bob,'bar)
          aliceOk = recv == ("foo",43)
          println("Alice got foo, 43")

          s ! 'Bob -> 'foo
        }}

        bob.bind { s =>
          val recv = s.?[Int]('Alice,'foo)
          bobOk = recv == 42
          println("Bob got 42")
          s ! 'Alice -> 'bar("foo",43)
          
          s.receive('Alice) {
            case ('Alice, 'foo) => bobOk2 = true
          }
        }
      }
      assert(aliceOk, "Alice was not able to communicate")
      assert(bobOk, "Bob was not able to communicate")
      assert(bobOk2, "Bob was not able to receive label")
    }

    def xor(a: Boolean, b: Boolean) = (a && !b)||(!a && b)

    test("allows encoding of race condition with 2 binds on same port", Tag("race")) {
      var didRun1 = false ; var didRun2 = false ; var didRunBar = true
      withTimeoutAndWait {
        spawn { startSession(alice, bob) }
        spawn {
          alice.bind { _ => didRunBar = true}
        }
        spawn {
          bob.bind { _ => didRun1 = true }
        }
        spawn {
          bob.bind { _ => didRun2 = true }
        }
      }
      sleep()
      assert(didRunBar, "bar should have started")
      assert(xor(didRun1,didRun2), "should run either. ran 1: " + didRun1 + ", ran 2: " + didRun2)
    }

    case object Foo ; case object Bar
    test("doesn't interfere with standard actor messaging") {
      var fooReceived = false ; var barReceived = false
      val single = createDefaultPort("protocol P { role Single; }", 'Single)
      withTimeout(1000) {
        spawn { startSession(single) }
        val fooActor = actor {
          receive {
            case Foo => fooReceived = true
          }
          single.bind { s =>
            receive {
              case Bar => barReceived = true
            }
          }
        }

        fooActor ! Foo
        fooActor ! Bar
      }
      sleep()
      assert(fooReceived && barReceived)
    }

    test("sequence of two message receives out of order works") {
      val foo = newLocalPort("protocol P { role Foo, Bar, Quux; }", 'Foo)
      val bar = newLocalPort("protocol P { role Foo, Bar, Quux; }", 'Bar)
      val quux = newLocalPort("protocol P { role Foo, Bar, Quux; }", 'Quux)
      var barOk = false; var quuxOk = false
      withTimeout(1000) {
        spawn { startSession(foo, bar, quux) }
        spawn {
          foo.bind { s =>
            barOk = (s ? 'Bar) == 42
            quuxOk = (s ? 'Quux) == 'a'
          }
        }

        spawn { bar.bind { s =>
          sleep()
          s ! 'Foo -> 42
        }}

        spawn { quux.bind { s =>
          s ! 'Foo -> 'a'
        }}
      }

      Thread.sleep(800) // needs to be longer than the sleep in Bar otherwise message will not have arrived yet
      assert(barOk)
      assert(quuxOk)
    }

    // Too long for routine testing
    test("many waiting binds don't blow the stack", Tag("slow")) {
      val manyWaiting = createDefaultPort("protocol P { role One, Other; }", 'One)
      for (i <- List.range(1,1000000)) {
        actor { manyWaiting.bind { _ => } }
      }
    }
  }
}
