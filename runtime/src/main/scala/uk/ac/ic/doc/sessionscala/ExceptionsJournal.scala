package uk.ac.ic.doc.sessionscala

import org.scribble.common.logging.{Journal, ConsoleJournal}
import java.util.Map

/**
 * Created by: omp08
 */

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
