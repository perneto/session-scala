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

  val basicProtoModel = parse(
    """protocol Foo {
         role Alice, Bob;
         String from Alice to Bob;
       }
    """)

  test("basic protocol, complete") {
    var env = join(basicProtoModel, "Alice")
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveJoin
  }

  test("basic protocol, wrong message type") {
    var env = join(basicProtoModel, "Alice")
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", objectT) // wrong message type
    }
  }

  test("basic protocol, missing interaction") {
    var env = join(basicProtoModel, "Alice")
    // missing send
    intercept[SessionTypeCheckingException] {
      env = env.leaveJoin
    }
  }

  test("basic protocol, receive side") {
    var env = join(basicProtoModel, "Bob")
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

  val empty = definitions.EmptyPackage
  val fooMethod = empty.newMethod(mkTermName("foo"))
  ignore("method inference - needs Interaction.equals implemented in Scribble") {
    var env: SessionTypingEnvironment = new MethodSessionTypeInferenceTopLevelEnv
    env = env.enterSessionMethod(fooMethod, List(sessChan))
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveSessionMethod
    val inferred = env.asInstanceOf[MethodSessionTypeInferenceTopLevelEnv]
                                    .inferredSessionType(fooMethod, sessChan) 
    inferred should have length (1)
    inferred(0) should be (new Interaction(null, List(new Role("Bob")) asJava, 
        new MessageSignature(new TypeReference("String"))))
  }
}