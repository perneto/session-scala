package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import org.scribble.common.logging.{ConsoleJournal, Journal}
import org.scribble.protocol.model._
import java.io.ByteArrayInputStream
import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import tools.nsc.{NoPhase, Settings, Global}

/**
 * Created by: omp08
 */

class EnvironmentsTest extends FunSuite with SessionTypingEnvironments {
  val scribbleJournal: Journal = new ConsoleJournal
  val scribbleParser = new ANTLRProtocolParser

  val settings = new Settings
  val scalaVersion = "2.8.0"
  settings.classpath.tryToSet(List(
            "project/boot/scala-"+scalaVersion+"/lib/scala-compiler.jar" +
            ":project/boot/scala-"+scalaVersion+"/lib/scala-library.jar"))

  val global = new Global(settings)

  import global.{Block => _, _}
  new Run // to initialize standard definitions (basic types, etc)
  /*  Possibly more lightweight, but crashes
  val phase1 = syntaxAnalyzer.newPhase(NoPhase)
  phase = phase1
  definitions.init
  */
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

  val protoModel = {
    val p = """protocol Foo {
                 role Alice, Bob;"
                 String from Alice to Bob; }"""
    scribbleParser.parse(new ByteArrayInputStream(p.getBytes), scribbleJournal)
  }

  test("basic protocol properly implemented") {
    var env = topEnv.registerSharedChannel(sharedChan, protoModel)
    val envInit = env
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.send(sessChan, "Bob", definitions.StringClass.tpe)
    env = env.leaveJoin
    assert(env == envInit)
  }

  test("basic protocol, wrong message type") {
    var env = topEnv.registerSharedChannel(sharedChan, protoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    env = env.send(sessChan, "Bob", definitions.ObjectClass.tpe) // wrong message type
    intercept[SessionEnvironmentException] {
      env = env.leaveJoin // checks are delayed until whole session type should have been implemented
    }
  }

  test("basic protocol, missing interaction") {
    var env = topEnv.registerSharedChannel(sharedChan, protoModel)
    env = env.enterJoin(sharedChan, "Alice", sessChan)
    // missing send
    intercept[SessionEnvironmentException] {
      env = env.leaveJoin // checks are delayed until whole session type should have been implemented
    }
  }

  test("basic protocol, receive side") {
    var env = topEnv.registerSharedChannel(sharedChan, protoModel)
    val envInit = env
    env = env.enterJoin(sharedChan, "Bob", sessChan)
    env = env.receive(sessChan, "Alice", definitions.StringClass.tpe)
    env = env.leaveJoin
    assert(env == envInit)
  }
}