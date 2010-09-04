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
  val string = definitions.StringClass.tpe
    
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

    val envWithSharedChan = topEnv.registerSharedChannel(sharedChan, emptyModel)
    var env = envWithSharedChan.enterJoin(sharedChan, "A", sessChan)
    env = env.leaveJoin
    env should be theSameInstanceAs (envWithSharedChan)
  }

  val basicProtoModel = parse(
    """protocol Foo {
         role Alice, Bob;
         String from Alice to Bob;
       }
    """)

  test("basic protocol, complete") {
    var env = topEnv.registerSharedChannel(sharedChan, basicProtoModel)
    val envInit = env
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.send(sessChan, "Bob", string)
    env = env.leaveJoin
    env should be === (envInit)
  }

  test("basic protocol, wrong message type") {
    var env = topEnv.registerSharedChannel(sharedChan, basicProtoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    intercept[SessionTypeCheckingException] {
      env = env.send(sessChan, "Bob", definitions.ObjectClass.tpe) // wrong message type
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
    val envInit = env
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    env = env.receive(sessChan, "Alice", string)
    env = env.leaveJoin
    env should be theSameInstanceAs (envInit)
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
    val envInit = env
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.send(sessChan, "Bob", string)
    env = env.leaveJoin
    env should be theSameInstanceAs (envInit)
  }

  test("protocol with choice, receiver side, complete") {
    var env = topEnv.registerSharedChannel(sharedChan, choiceProtoModel)
    val envInit = env
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    env = env.enterBranchReceiveBlock(sessChan, "Alice")
    env = env.enterIndividualBranchReceive(string)
    env = env.leaveIndividualBranchReceive
    env = env.enterIndividualBranchReceive(definitions.IntClass.tpe)
    env = env.leaveIndividualBranchReceive
    env = env.leaveJoin
    env should be theSameInstanceAs (envInit)
  }
}