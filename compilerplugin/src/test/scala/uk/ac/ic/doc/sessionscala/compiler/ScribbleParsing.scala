package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.common.logging.{Journal, ConsoleJournal}
import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import org.scribble.protocol.model.ProtocolModel
import java.util.Map
import java.io.{Serializable, ByteArrayInputStream}

/**
 * Created by: omp08
 */

trait ScribbleParsing {
  class ExceptionsJournal extends Journal {
    val console = new ConsoleJournal
    var errors: Vector[(String, Map[String, Object])] = Vector()
    def info(issue: String, prop: Map[String, Object]) = console.info(issue,prop)

    def warning(issue: String, prop: Map[String, Object]) = console.warning(issue,prop)

    def error(issue: String, prop: Map[String, Object]) = {
      errors = (issue,prop) +: errors
      console.error(issue,prop)
    }
    def hasError = !errors.isEmpty
    def getErrors = errors
  }
  val scribbleJournal = new ExceptionsJournal
  val scribbleParser = new ANTLRProtocolParser

  def parse(s: String): ProtocolModel = {
    scribbleJournal.errors = Vector()
    val model = scribbleParser.parse(new ByteArrayInputStream(s.getBytes), scribbleJournal, null)
    if (scribbleJournal.hasError) throw new SessionTypeCheckingException(
      "Error parsing Scribble description: " + scribbleJournal.getErrors
      )
    model
  }

}