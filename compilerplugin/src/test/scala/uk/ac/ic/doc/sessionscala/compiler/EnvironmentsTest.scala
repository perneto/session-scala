package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scribble.common.logging.{ConsoleJournal, Journal}
import tools.nsc.{Settings, Global}
import org.scribble.protocol.model._

/**
 * Created by: omp08
 */

class EnvironmentsTest extends FunSuite with SessionTypingEnvironments {
  val scribbleJournal: Journal = new ConsoleJournal
  val settings = new Settings
  val global = new Global(settings)
  import global.{Block => _, _}

  val topEnv = new TopLevelSessionTypingEnvironment
  val sharedChan = newTermName("foo")
  val sessChan = newTermName("s")

  test("top-level enter join, unregistered channel") {
    intercept[SessionEnvironmentException] {
      topEnv.enterJoin(sharedChan, "A", sessChan)
    }
  }

  test("top-level leave") {
    intercept[SessionEnvironmentException] {
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
    assert(env == envWithSharedChan)
  }

  test("basic protocol") {
    val block = new Block

    val roleA = new Role("Alice")
    val roleB = new Role("Bob")
    val roles = new RoleList
    roles.getRoles.add(roleA)
    roles.getRoles.add(roleB)
    block.add(roles)

    val msg = new Interaction
    val sig = new MessageSignature
    sig.getTypeReferences.add(new TypeReference("String"))
    msg.setMessageSignature(sig)
    msg.setFromRole(roleA)
    msg.getToRoles.add(roleB)
    block.add(msg)

    val proto = new Protocol
    proto.setBlock(block)
    val protoModel = new ProtocolModel
    protoModel.setProtocol(proto)

    var env = topEnv.registerSharedChannel(sharedChan, protoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    //env = env.send(sessChan, "Bob", )
  }
}