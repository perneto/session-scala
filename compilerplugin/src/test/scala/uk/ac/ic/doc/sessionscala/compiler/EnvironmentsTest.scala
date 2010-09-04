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

  val topEnv = new TopLevelSessionTypingEnvironment
  val sharedChan = newTermName("foo")
  val sessChan = newTermName("s")
  val stringT = definitions.StringClass.tpe
  val intT = definitions.IntClass.tpe
  val objectT = definitions.ObjectClass.tpe
    
  test("top-level enter join, unregistered channel") {
    intercept[SessionTypeCheckingException] {
      topEnv.enterJoin(sharedChan, "A", sessChan)
    }
  }

  test("top-level leave") {
    intercept[SessionTypeCheckingException] {
      topEnv.leaveJoin
    }
  }
                                    
  test("enter and leave join, empty protocol") {
    val emptyProtocol = new Protocol
    val emptyModel = new ProtocolModel()
    emptyProtocol.setRole(new Role("A"))
    emptyModel.setProtocol(emptyProtocol)

    var env = topEnv.registerSharedChannel(sharedChan, emptyModel)
    env = env.enterJoin(sharedChan, "A", sessChan)
    env = env.leaveJoin
  }

  val basicProtoModel = parse(
    """protocol Foo {
         role Alice, Bob;
         String from Alice to Bob;
       }
    """)

  test("basic protocol, complete") {
    var env = topEnv.registerSharedChannel(sharedChan, basicProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveJoin
  }

  test("basic protocol, wrong message type") {
    var env = topEnv.registerSharedChannel(sharedChan, basicProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", objectT) // wrong message type
    }
  }

  test("basic protocol, missing interaction") {
    var env = topEnv.registerSharedChannel(sharedChan, basicProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    // missing send
    intercept[SessionTypeCheckingException] {
      env = env.leaveJoin
    }
  }

  test("basic protocol, receive side") {
    var env = topEnv.registerSharedChannel(sharedChan, basicProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
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
    var env = topEnv.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.send(sessChan, "Bob", stringT)
    env = env.leaveJoin
  }

  test("protocol with choice, receiver side, complete") {
    var env = topEnv.registerSharedChannel(sharedChan, choiceProtoModel)
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    env = env.enterBranchReceiveBlock(sessChan, "Alice")
    env = env.enterIndividualBranchReceive(stringT)
    env = env.leaveIndividualBranchReceive
    env = env.enterIndividualBranchReceive(intT)
    env = env.leaveIndividualBranchReceive
    env = env.leaveBranchReceiveBlock
    env = env.leaveJoin
  }
}