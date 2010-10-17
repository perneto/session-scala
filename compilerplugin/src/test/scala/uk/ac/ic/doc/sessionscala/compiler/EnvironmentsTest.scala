package uk.ac.ic.doc.sessionscala.compiler

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scribble.protocol.model._
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by: omp08
 */

@RunWith(classOf[JUnitRunner])
class EnvironmentsTest extends FunSuite with SessionTypingEnvironments
                                        with ScalaCompilerSetup
                                        with ScribbleParsing
                                        with ShouldMatchers {

  import global._

  val topEnv = new JoinBlockTopLevelEnv
  val sharedChan = newTermName("sharedChannel")
  val sharedChan2 = newTermName("sharedChan2")
  val sessChan = newTermName("sessionChannel")
  val sessChan2 = newTermName("sessChan2")
  val stringT = definitions.StringClass.tpe
  val intT = definitions.IntClass.tpe
  val objectT = definitions.ObjectClass.tpe
  val anyT = definitions.AnyClass.tpe
  val charSequenceT = definitions.getClass("java.lang.CharSequence").tpe
  
  val stringTRef = new TypeReference("String")
  val intTRef = new TypeReference("Int")

  def join(model: ProtocolModel, joinAs: String): SessionTypingEnvironment = {
    val env = topEnv.registerSharedChannel(sharedChan, model)
    env.enterJoin(sharedChan, joinAs, sessChan)
  }

  test("top-level enter join, unregistered channel") {
    intercept[SessionTypeCheckingException] {
      topEnv.enterJoin(sharedChan, "A", sessChan)
    }
  }

  test("top-level leave (no session to be left)") {
    intercept[SessionTypeCheckingException] {
      topEnv.leaveJoin
    }
  }

  val emptyProtoModel = parse(
  """protocol Foo { role Alice; }
  """)

  test("enter and leave join, empty protocol") {
    var env = join(emptyProtoModel, "Alice")
    env = env.leaveJoin
  }

  val sendStringModel = parse(
    """protocol Foo {
         role Alice, Bob;
         String from Alice to Bob;
       }
    """)

  test("basic protocol, complete") {
    var env = join(sendStringModel, "Alice")
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveJoin
  }

  test("basic protocol, wrong message type") {
    var env = join(sendStringModel, "Alice")
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", objectT) // wrong message type
    }
  }

  test("basic protocol, missing interaction") {
    var env = join(sendStringModel, "Alice")
    // missing send
    intercept[SessionTypeCheckingException] {
      env = env.leaveJoin
    }
  }

  test("basic protocol, receive side") {
    var env = join(sendStringModel, "Bob")
    env = env.receive(sessChan, "Alice", stringT)
    env = env.leaveJoin
  }

  val choiceProtoModel = parse(
    """protocol Foo {
         role Alice, Bob;
         choice from Alice to Bob {
           String {}
           Int {}
         }
       }
    """)

  test("protocol with choice, chooser side, complete") {
    var env = join(choiceProtoModel, "Alice")
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveJoin
  }

  test("protocol with choice, receiver side, complete") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    env = env.leaveJoin
  }


  test("choice, if branches on chooser side, complete") {
    var env = join(choiceProtoModel, "Alice")
    env = env.enterThen
    env = env.send(sessChan, "Bob", stringT)
    env = env.enterElse
    env = env.send(sessChan, "Bob", intT)
    env = env.leaveIf
    env = env.leaveJoin
  }

  test("choice, supertype on receive covers 2 branches")  {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(anyT)
    env = env.leaveChoiceReceiveBranch
    intercept[SessionTypeCheckingException] {
      env = env.leaveChoiceReceiveBlock
    }
  }

  test("choice, if branches on receive side, branch label is supertype but covers single branch, complete") {
    var env = join(choiceProtoModel, "Bob")
    env = env.enterThen
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock

    env = env.enterElse
    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(charSequenceT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
        
    env = env.leaveIf
    env = env.leaveJoin
  }

  val twoMsgProto = parse(
  """protocol Foo {
       role Alice, Bob;
       String from Alice to Bob;
       Int from Bob to Alice;
     }
  """)

  test("interleaved sessions") {
    var env = join(twoMsgProto, "Alice")
    env = env.registerSharedChannel(sharedChan2, twoMsgProto)
    env = env.enterJoin(sharedChan2, "Bob", sessChan2)
    env = env.send(sessChan, "Bob", stringT)
    env = env.receive(sessChan2, "Alice", stringT)
    env = env.send(sessChan2, "Alice", intT)
    env = env.receive(sessChan, "Bob", intT)
    env = env.leaveJoin
    env = env.leaveJoin
  }

  test("interleaved sessions, branches receive side") {
    var env = join(choiceProtoModel, "Bob")

    env = env.registerSharedChannel(sharedChan2, twoMsgProto)
    env = env.enterJoin(sharedChan2, "Alice", sessChan2)
    env = env.send(sessChan2, "Bob", stringT)

    env = env.enterChoiceReceiveBlock(sessChan, "Alice")
    env = env.enterChoiceReceiveBranch(stringT)

    env = env.receive(sessChan2, "Bob", intT)

    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)

    env = env.receive(sessChan2, "Bob", intT)

    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock

    env = env.leaveJoin
    env = env.leaveJoin
  }
  
  import scalaj.collection.Imports._

  def sessionMethod(method: Symbol, chan: Name): SessionTypingEnvironment = {
    var env: SessionTypingEnvironment = new MethodSessionTypeInferenceTopLevelEnv
    env.enterSessionMethod(fooMethod, List(chan))
  }
  
  def inferred(env: SessionTypingEnvironment, method: Symbol, chan: Name): List[Activity] = 
    env.asInstanceOf[MethodSessionTypeInferenceTopLevelEnv]
                                    .inferredSessionType(method, chan)
  
  val empty = definitions.EmptyPackage
  val fooMethod = empty.newMethod(mkTermName("foo"))
  test("method inference, one send and receive") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", stringT)
    env = env.receive(sessChan, "Bob", stringT)
    env = env.leaveSessionMethod
    
    val inf = inferred(env, fooMethod, sessChan) 
    inf should have length (2)
    inf(0) should be (createInteraction(null, new Role("Bob"), stringTRef))
    inf(1) should be (createInteraction(new Role("Bob"), null, stringTRef))
  }
  
  test("method inference, branching") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.enterChoiceReceiveBlock(sessChan, "Bob")
    env = env.enterChoiceReceiveBranch(stringT)
    env = env.leaveChoiceReceiveBranch
    env = env.enterChoiceReceiveBranch(intT)
    env = env.leaveChoiceReceiveBranch
    env = env.leaveChoiceReceiveBlock
    
    val inf = inferred(env, fooMethod, sessChan) 
    inf should have length (1)
    inf(0) should be (createChoice(new Role("Bob"), null, List(stringTRef, intTRef)))
  }
  
  test("method inference, if branches should be merged into choice") {
    var env = sessionMethod(fooMethod, sessChan)
    
    env = env.enterThen
    env = env.send(sessChan, "Alice", stringT)
    env = env.enterElse
    env = env.send(sessChan, "Alice", intT)
    env = env.leaveIf
    
    val inf = inferred(env, fooMethod, sessChan)
    inf should have length (1)
    inf(0) should be (createChoice(null, new Role("Alice"), List(stringTRef, intTRef)))
  }
  
  test("inferred method call") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Bob", stringT)
    
    val topEnv = new JoinBlockTopLevelEnv(env)
    env = topEnv.registerSharedChannel(sharedChan, sendStringModel)
    env.enterJoin(sharedChan, "Alice", sessChan)
    env.delegation(fooMethod, List(sessChan))
    env.leaveJoin
  }
  
  test("method inference, recursion") {
    var env = sessionMethod(fooMethod, sessChan)
    
    env = env.send(sessChan, "Alice", stringT)
    env = env.delegation(fooMethod, List(sessChan)) // recursion detected: gather inferred and wrap inside recur
    
    val inf = inferred(env, fooMethod, sessChan)
    inf should have length (1)
    // To solve the unknown label problem, extra annotations might be required
    inf(0) should be (createRecur(null, 
          List(createInteraction(null, new Role("Alice"), stringTRef), createRecursion(null))
        )
    )
  }
  
  val recurModel = parse(
  """ protocol Foo {
        role Alice, Bob;
        X: {
          String from Alice to Bob;
          #X;
        }
      }
  """)
  
  test("inferred, recursive method call - unroll recursion first") {
    var env = sessionMethod(fooMethod, sessChan)
    env = env.send(sessChan, "Alice", stringT)
    env = env.delegation(fooMethod, List(sessChan))
    
    val topEnv = new JoinBlockTopLevelEnv(env)
    env = topEnv.registerSharedChannel(sharedChan, recurModel)
    env.enterJoin(sharedChan, "Alice", sessChan)
    env.delegation(fooMethod, List(sessChan))
    env.leaveJoin
  }
  	
  test("scoping of inferred methods") {
    // will require keeping even the join block visitor informed of method definitions,
    // so that it can bring previously inferred methods in and out of scope
  }
}