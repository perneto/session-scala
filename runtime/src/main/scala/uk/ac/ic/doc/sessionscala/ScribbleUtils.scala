package uk.ac.ic.doc.sessionscala

import org.scribble.protocol.model.ProtocolModel
import scalaj.collection.Imports._

object ScribbleUtils {
  def roleSymbols(model: ProtocolModel): Set[Symbol] = {
    roles(model).map(Symbol(_))
  }

  def roles(model: ProtocolModel): Set[String] =
    Set(model.getRoles.asScala.map(r => r.getName): _*)
}