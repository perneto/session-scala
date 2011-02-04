package uk.ac.ic.doc.sessionscala.compiler

import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import org.scribble.protocol.model.ProtocolModel
import java.io.ByteArrayInputStream
import uk.ac.ic.doc.sessionscala.ExceptionsJournal

/**
 * Created by: omp08
 */

trait ScribbleParsing {
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