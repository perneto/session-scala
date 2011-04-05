package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.Address._
import org.scalatest.{BeforeAndAfterEach, BeforeAndAfterAll, FunSuite, Tag}
import actors.Actor, Actor._
import org.scalatest.matchers.ShouldMatchers._

class PublicPortSpec extends FunSuite with Timeouts with BeforeAndAfterAll {

  val rng = new java.util.Random
  def randomName(): String = {
    val rand = rng.nextInt(1000000000)
    var name = rand.toString
    while (name.length < 9) name = "0" + name
    "q"+name
  }
  
  val queuesInTest = collection.mutable.Set[String]()
  def createQueue(proto: String, role: Symbol) = {
    val name = randomName()
    queuesInTest += name
    AMQPPort(proto, role, name)
  }
  
  override def afterAll() {
    val chan = AMQPUtils.connectDefaults()
    for (q <- queuesInTest) {
      // need to ensure it's there first, as some tests don't really create the queue
      // (randomName is called for every call to AMQPPort, but not all ports get bound/invited)
      chan.queueDeclare(q,false,false,false,null)
      chan.queueDelete(q)
    }
    AMQPUtils.close(chan)
  }

  override def nestedSuites = List(
    new SessionPortSpecImpl("AMQP", createQueue),
    new SessionPortSpecImpl("Shared Mem", newLocalPort)
  )

  class SessionPortSpecImpl(name: String,
                            freshPort: (String, Symbol) => Address)
          extends FunSuite with BeforeAndAfterEach {
    var alice: Address = null
    var bob: Address = null

    override def suiteName = "SessionPortSpec: "+name+" implementation"

    override def beforeEach() {
      alice = freshPort("protocol Test { role Alice, Bob; }", 'Alice)
      bob = freshPort("protocol Test { role Alice, Bob; }", 'Bob)
    }

    test("non-exhaustive invite: no runtime error (should be checked by compiler). Timeouts since no bound process", Tag("timeouts"), Tag("leftoverq")) {
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

    test("bind when not invited: timeouts", Tag("timeouts"), Tag("leftoverq")) {
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
        actor { 
          startSession(alice, bob) 
          println("done starting session")
        }

        actor { alice.bind { s =>
          s ! 'Bob -> 42
          println("Alice sent 42 to Bob")
          val recv = s.?[String]('Bob)
          //println("Alice received "+recv+" from Bob")
          aliceOk = recv == "foo"
        }}

        bob.bind { s =>
          val recv = s.?[Int]('Alice)
          //println("Bob received "+recv+" from Alice")
          bobOk = recv == 42
          s ! 'Alice -> "foo"
          println("Bob sent foo to Alice")
        }
      }
      assert(aliceOk, "Alice was not able to communicate")
      assert(bobOk, "Bob was not able to communicate")
    }

    test("syntax for labels using implicit conversion", Tag("labels")) {
      var aliceOk = false; var aliceOk2 = false
      var bobOk = false ; var bobOk2 = false ; var bobOk3 = false
      withTimeoutAndWait {
        actor { startSession(alice, bob) }

        actor { alice.bind { s =>
          s ! 'Bob -> ('foo, 42)
          s ! 'Bob -> 'foo
          s ! 'Bob -> 'label
          
          val recv = s.?[(String,Int)]('Bob,'bar)
          aliceOk = recv == ("foo", 43)
          //println("Alice got foo, 43")

          s.receive('Bob) {
            case ('bar, s:String, i:Int) => aliceOk2 = true
          }          
        }}

        bob.bind { s =>
          s ! 'Alice -> ('bar, "foo", 43)
          s ! 'Alice -> ('bar, "foo", 43)
                    
          val recv = s.?[Int]('Alice,'foo)
          bobOk = recv == 42
          //println("Bob got 42")
                    
          s.receive('Alice) {
            // TODO: Static checking for errors like ('Alice, ...)
            case 'foo => bobOk2 = true
          }
          
          s.?[Unit]('Alice, 'label)
          bobOk3 = true
        }
      }
      assert(aliceOk, "Alice was not able to receive labelled msg with 2 values using ?")
      assert(aliceOk2, "Alice was not able to receive labelled msg with 2 values using receive")
      assert(bobOk, "Bob was not able to receive labelled msg with 1 value using ?")
      assert(bobOk2, "Bob was not able to receive label with no value using receive")
      assert(bobOk3, "Bob was not able to receive label with no value using ?")    
    }

    test("Receive from multiple roles", Tag("rcvmulti")) {
      var rcvOk = false ; var rcvOk2 = false
      val proto = """
        protocol P { role A, B; }
      """
      val a = freshPort(proto, 'A)
      val b = freshPort(proto, 'B)
      
      withTimeoutAndWait {
        actor { startSession(a,b) }
        actor {
          a.bind { s => 
            s ! 'B -> ('label, 42, "foo")
            s ! 'B -> 'label2
          }
        }
        b.bind { s =>
          s.mreceive {
            case 'A -> (('label, i, str)) => rcvOk = true
            case 'B -> (i:Int) =>
          }
          s.mreceive {
            case 'A -> 'label2 => rcvOk2 = true
          }
        } 
      }
      
      assert(rcvOk, "B should have received labelled message using multi-source receive")
      assert(rcvOk2, "B should have received label using multi-source receive")    
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
      sleep(600)
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

    test("sequence of two message receives out of order works", Tag("ooo")) {
      val foo = freshPort("protocol P { role Foo, Bar, Quux; }", 'Foo)
      val bar = freshPort("protocol P { role Foo, Bar, Quux; }", 'Bar)
      val quux = freshPort("protocol P { role Foo, Bar, Quux; }", 'Quux)
      var fromBar: Any = null ; var fromQuux: Any = null
      withTimeout(1000) {
        actor { 
          startSession(foo, bar, quux)
          //println("finished starting session")
        }
        actor {
          foo.bind { s =>
            fromBar = s ? 'Bar
            //println("got from bar")
            fromQuux = s ? 'Quux
          }
        }

        actor { 
          bar.bind { s =>
            sleep()
            s ! 'Foo -> 42
          }
        }

        actor { 
          quux.bind { s =>
            s ! 'Foo -> 'a'
          }
        }
      }

      sleep(600) // needs to be longer than the sleep in Bar otherwise message will not have arrived yet
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
