package uk.ac.ic.doc.sessionscala

import collection.JavaConverters._
import org.scribble.protocol.model.{Role, ProtocolModel}

object ScribbleRuntimeUtils {
  def roleSymbols(model: ProtocolModel): Set[Symbol] = {
    roleNames(model).map(Symbol(_))
  }

  def roleNames(model: ProtocolModel): Set[String] =
    roles(model).map(r => r.getName)
  
  def roles(model: ProtocolModel): Set[Role] =
    Set(model.getRoles.asScala:_*)
}