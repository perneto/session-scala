package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.Address._
import org.scalatest.{BeforeAndAfterEach, BeforeAndAfterAll, FunSuite, Tag}
import actors.Actor, Actor._
import org.scalatest.matchers.ShouldMatchers._
import java.util.concurrent.TimeoutException

class AddressSpec extends FunSuite with Timeouts with BeforeAndAfterAll {

  override def nestedSuites = List(
    new SessionPortSpecImpl("AMQP", createQueue),
    new SessionPortSpecImpl("Shared Mem", newLocalAddress)
  )

  class SessionPortSpecImpl(name: String,
                            freshAddr: (String, Symbol) => Address)
          extends FunSuite with BeforeAndAfterEach {
    var alice: Address = null
    var bob: Address = null

    override def suiteName = "SessionPortSpec: "+name+" implementation"

    override def beforeEach() {
      alice = freshAddr("protocol Test { role Alice, Bob; }", 'Alice)
      bob = freshAddr("protocol Test { role Alice, Bob; }", 'Bob)
    }

    test("non-exhaustive invite: no runtime error (should be checked by compiler). " +
            "Timeouts since no bound process", Tag("timeouts"), Tag("startTimeout")) {
      intercept[TimeoutException] {
        startSessionWithin(500, alice)
      }
    }

    test("bind when not invited: timeouts", Tag("timeouts")) {
      var didRun = false
      val otherAlice = freshAddr("protocol Test { role Alice, Bob; }", 'Alice)
      val otherBob = freshAddr("protocol Test { role Alice, Bob; }", 'Bob)
      actor { 
        intercept[TimeoutException] {
          startSessionWithin(300, otherAlice, otherBob) 
        }
      }

      intercept[TimeoutException] {
        alice.bindWithin(300) { s => didRun = true }
      }
      assert(!didRun, "Alice should not have started as there was no invite for Alice on default test port")
    }
    
    test("role not in protocol: error") {
      intercept[IllegalArgumentException] {
        freshAddr("protocol Test { role Alice, Bob; }", 'Foo)
      }
    }

    test("complains if protocol is not valid Scribble") {
      intercept[IllegalArgumentException] {
        freshAddr("foobar", 'Foo)
      }
    }

    test("complains if protocol does not have at least 1 role") {
      intercept[IllegalArgumentException] {
        freshAddr("protocol P {}", 'Foo)
      }
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
      val a = freshAddr(proto, 'A)
      val b = freshAddr(proto, 'B)
      
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
          try {
            bob.bindWithin(300) { _ => didRun1 = true }
          } catch {
            case _:TimeoutException =>
          }
        }
        actor {
          try {
            bob.bindWithin(300) { _ => didRun2 = true }
          } catch {
            case _:TimeoutException =>
          }
        }
      }
      sleep(500)
      assert(didRunBar, "bar should have started")
      assert(xor(didRun1,didRun2), "should run either. ran 1: " + didRun1 + ", ran 2: " + didRun2)
    }

    case object Foo ; case object Bar
    test("doesn't interfere with standard actor messaging", Tag("actors")) {
      var fooReceived = false ; var barReceived = false
      val single = freshAddr("protocol P { role Single; }", 'Single)
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
      val foo = freshAddr("protocol P { role Foo, Bar, Quux; }", 'Foo)
      val bar = freshAddr("protocol P { role Foo, Bar, Quux; }", 'Bar)
      val quux = freshAddr("protocol P { role Foo, Bar, Quux; }", 'Quux)
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

      sleep(1000) // needs to be longer than the sleep in Bar otherwise message will not have arrived yet
      assert(fromBar == 42, "Foo should have received 42 from Bar, got: "+fromBar)
      assert(fromQuux == 'a', "Foo should have received 'a' from Quux, got:"+fromQuux)
    }

    // Too long/resource-intensive for routine testing
    ignore("many waiting binds don't blow the stack", Tag("slow")) {
      val manyWaiting = freshAddr("protocol P { role One, Other; }", 'One)
      for (i <- List.range(1,1000000)) {
        actor { manyWaiting.bind { _ => } }
      }
    }
    
  test("Session interleaving") {
      val alice = freshAddr("protocol P { role Alice, Bob; }", 'Alice)
      val bobQ = freshAddr("protocol Q { role Bob, Carol; }", 'Bob)
      val bobP = freshAddr("protocol P { role Alice, Bob; }", 'Bob)
      val carol = freshAddr("protocol Q { role Bob, Carol; }", 'Carol)
      var bothAccepted = false ; var aliceOK = false ; var carolOK = false
      var bobOK1 = false; var bobOK2 = false ; var bobOK3 = false 
  
      actor { startSession(alice, bobP) }
      actor { startSession(bobQ, carol) }
  
      actor {
        alice.bind { sA =>
          sA ! 'Bob -> "Hello from Alice"
          //println("Hello (1) from Alice sent to: " + sA('Bob))
          aliceOK = sA.?('Bob) == "Hello from Bob"
          sA ! 'Bob -> "Hello from Alice"
          //println("Hello (2) from Alice sent to: " + sA('Bob))
        }
      }
  
      actor {
        bobP.bind { sA =>
          // println("before first receive on: " + sA)
          bobOK1 = sA.?('Alice) == "Hello from Alice"
  
          bobQ.bind { sB =>
            bothAccepted = true
            //println("before send to Alice")
            sA ! 'Alice -> "Hello from Bob"
            //println("sA: " + sA)
            //println("sB: " + sB)
            //println(this)
            //println("before second receive on: " + sA)
            bobOK2 = sA.?('Alice) == "Hello from Alice"
            //println("before receive on: " + sB)
            bobOK3 = sB.?('Carol) == "Hello from Carol"
            //println("after sB receive")
            sB ! 'Carol -> "Hello from Bob"
  
          }
        }
      }
  
      actor {
        carol.bind { sB =>
          sB ! 'Bob -> "Hello from Carol"
          //println("Hello from Carol sent to: " + sB('Bob))
          carolOK = sB.?('Bob) == "Hello from Bob"
        }
      }
  
      sleep(500)
      assert(bothAccepted, "Both sessions should be started")
  
      assert(aliceOK, "The message to Alice (sA) should be received")
      assert(bobOK1, "The message from Alice (sA) should be received in the outer scope")
      assert(bobOK2, "The message from Alice (sA) should be received in the inner scope")
      assert(bobOK3, "The message from Carol (sB) should be received")
      assert(carolOK, "The message to Carol (sB) should be received")
    }    
  }
  
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
    AMQPAddress(proto, role, name)
  }
  
  override def afterAll() {
    val chan = AMQPUtils.connectDefaults()
    for (q <- queuesInTest) {
      // need to ensure it's there first, as some tests don't really create the queue
      // (randomName is called for every call to AMQPAddress, but not all addresses get bound/invited)
      chan.queueDeclare(q,false,false,false,null)
      chan.queueDelete(q)
    }
    AMQPUtils.close(chan)
  }  
}
