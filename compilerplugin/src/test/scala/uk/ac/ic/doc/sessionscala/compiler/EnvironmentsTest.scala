package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scribble.protocol.model._
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by: omp08
 */

class EnvironmentsTest extends FunSuite with SessionTypingEnvironments
                                        with ScalaCompilerSetup
                                        with ScribbleParsing
                                        with ShouldMatchers {

  import global._

  val topEnv = new TopLevelEnv
  val sharedChan = newTermName("sharedChannel")
  val sessChan = newTermName("sessionChannel")
  val stringT = definitions.StringClass.tpe
  val intT = definitions.IntClass.tpe
  val objectT = definitions.ObjectClass.tpe

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

}