package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.PublicPort._
import org.scalatest.{BeforeAndAfterEach, FunSuite, Tag}
import actors.Actor, Actor._

class PublicPortSpec extends FunSuite with Timeouts {

  val rng = new java.util.Random
  def randomName(): String = {
    val rand = rng.nextInt(1000000000)
    var name = rand.toString
    while (name.length < 9) name = "0" + name
    "q"+name
  }

  override def nestedSuites = List(
    new SessionPortSpecImpl("AMQP", {(proto,role) => AMQPPort(proto, role, randomName())}),
    new SessionPortSpecImpl("Shared Mem", newLocalPort)
  )

  class SessionPortSpecImpl(name: String,
                            freshPort: (String, Symbol) => PublicPort)
          extends FunSuite with BeforeAndAfterEach {
    var alice: PublicPort = null
    var bob: PublicPort = null


    override def suiteName = "SessionPortSpec: "+name+" implementation"

    override def beforeEach() {
      alice = freshPort("protocol Test { role Alice, Bob; }", 'Alice)
      bob = freshPort("protocol Test { role Alice, Bob; }", 'Bob)
    }

    test("non-exhaustive invite: no runtime error (should be checked by compiler). Timeouts since no bound process", Tag("timeouts")) {
      expectTimeout(500) {
        startSession(alice)
      }
    }

    test("role not in protocol: error") {
      intercept[IllegalArgumentException] {
        freshPort("protocol Test { role Alice, Bob; }", 'Foo)
      }
    }

    test("complains if protocol is not valid Scribble") {
      intercept[IllegalArgumentException] {
        freshPort("foobar", 'Foo)
      }
    }

    test("complains if protocol does not have at least 1 role") {
      intercept[IllegalArgumentException] {
        freshPort("protocol P {}", 'Foo)
      }
    }

    test("bind when not invited: timeouts", Tag("timeouts")) {
      var didRun = false
      val otherAlice = freshPort("protocol Test { role Alice, Bob; }", 'Alice)
      val otherBob = freshPort("protocol Test { role Alice, Bob; }", 'Bob)
      expectTimeout(1000) {
        actor { startSession(otherAlice, otherBob) }

        alice.bind { s => didRun = true }
      }

      assert(!didRun, "Alice should not have started as there was no invite for Alice on default test port")
    }

    test("startSession/bind correctly starts session", Tag("init")) {
      var aliceStarted = false; var bobStarted = false;
      //println("starting test")
      withTimeoutAndWait {
        actor {
          //println("calling startSession")
          startSession(alice, bob)
        }
        //println("before alice spawn")
        actor {
          //println("calling bind on alice")
          alice.bind { s =>
            //println("in alice")
            aliceStarted = true
          }
        }

        //println("calling bind on bob")
        bob.bind { s =>
          //println("in bob")
          bobStarted = true
        }
        //println("bob exited")
      }
      assert(aliceStarted, "Alice did not start")
      assert(bobStarted, "Bob did not start")
    }

    test("invited participants can talk", Tag("talk")) {
      var aliceOk = false; var bobOk = false
      withTimeoutAndWait {
        actor { startSession(alice, bob) 
          //println("done starting session")
        }

        actor { alice.bind { s =>
          s ! 'Bob -> 42
          //println("Alice sent 42 to Bob")
          val recv = s.?[String]('Bob)
          //println("Alice received "+recv+" from Bob")
          aliceOk = recv == "foo"
        }}

        bob.bind { s =>
          val recv = s.?[Int]('Alice)
          //println("Bob received "+recv+" from Alice")
          bobOk = recv == 42
          s ! 'Alice -> "foo"
          //println("Bob sent foo to Alice")
        }
      }
      assert(aliceOk, "Alice was not able to communicate")
      assert(bobOk, "Bob was not able to communicate")
    }

    test("syntax for labels using implicit conversion", Tag("labels")) {
      var aliceOk = false; var bobOk = false ; var bobOk2 = false
      withTimeoutAndWait {
        actor { startSession(alice, bob) }

        actor { alice.bind { s =>
          s ! 'Bob -> 'foo(42)
          val recv = s.?[(String,Int)]('Bob,'bar)
          aliceOk = recv == ("foo",43)
          //println("Alice got foo, 43")

          s ! 'Bob -> 'foo
        }}

        bob.bind { s =>
          val recv = s.?[Int]('Alice,'foo)
          bobOk = recv == 42
          //println("Bob got 42")
          s ! 'Alice -> 'bar("foo",43)
          
          s.receive('Alice) {
            // TODO: Static checking for errors like ('Alice, ...)
            case 'foo => bobOk2 = true
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
        actor { startSession(alice, bob) }
        actor {
          alice.bind { _ => didRunBar = true}
        }
        actor {
          bob.bind { _ => didRun1 = true }
        }
        actor {
          bob.bind { _ => didRun2 = true }
        }
      }
      Thread.sleep(1000)
      assert(didRunBar, "bar should have started")
      assert(xor(didRun1,didRun2), "should run either. ran 1: " + didRun1 + ", ran 2: " + didRun2)
    }

    case object Foo ; case object Bar
    test("doesn't interfere with standard actor messaging", Tag("actors")) {
      var fooReceived = false ; var barReceived = false
      val single = freshPort("protocol P { role Single; }", 'Single)
      withTimeout(1000) {
        actor { startSession(single) }
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
      var fromBar: Any = null ; var fromQuux: Any = null
      withTimeout(1000) {
        actor { startSession(foo, bar, quux) }
        actor {
          foo.bind { s =>
            fromBar = s ? 'Bar
            println("got from bar")
            fromQuux = s ? 'Quux
          }
        }

        actor { bar.bind { s =>
          sleep()
          s ! 'Foo -> 42
        }}

        actor { quux.bind { s =>
          s ! 'Foo -> 'a'
        }}
      }

      sleep(2000) // needs to be longer than the sleep in Bar otherwise message will not have arrived yet
      assert(fromBar == 42, "Foo should have received 42 from Bar, got: "+fromBar)
      assert(fromQuux == 'a', "Foo should have received 'a' from Quux, got:"+fromQuux)
    }

    // Too long/resource-intensive for routine testing
    ignore("many waiting binds don't blow the stack", Tag("slow")) {
      val manyWaiting = freshPort("protocol P { role One, Other; }", 'One)
      for (i <- List.range(1,1000000)) {
        actor { manyWaiting.bind { _ => } }
      }
    }
  }
}
