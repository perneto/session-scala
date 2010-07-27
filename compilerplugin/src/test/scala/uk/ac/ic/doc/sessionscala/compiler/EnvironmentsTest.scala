package uk.ac.ic.doc.sessionscala.compiler

import org.scalatest.FunSuite
import uk.ac.ic.doc.sessionscala.SharedChannel
import org.scribble.common.logging.{ConsoleJournal, Journal}
import tools.nsc.{Settings, Global}
import org.scribble.protocol.model.{Role, ProtocolModel, Protocol}

/**
 * Created by: omp08
 */

class EnvironmentsTest extends FunSuite with SessionTypingEnvironments {
  val scribbleJournal: Journal = new ConsoleJournal
  val settings = new Settings
  val global = new Global(settings)

  test("top-level leave") {

  }

  test("top-level enter accept") {

  }

  test("enter and leave accept, empty protocol") {
    val emptyProtocol = new Protocol
    val localModel = new ProtocolModel()
    emptyProtocol.setRole(new Role("A"))

    var env: SessionTypingEnvironment = new TopLevelSessionTypingEnvironment
  }
}