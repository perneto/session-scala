package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc
import nsc.Global

trait SessionDefinitions {
  val global: Global
  import global._

  // Lazy because otherwise global is not set yet, so NPE trying to access definitions
  lazy val addressTrait = definitions.getClass("uk.ac.ic.doc.sessionscala.Address")
  lazy val addressObject = definitions.getModule("uk.ac.ic.doc.sessionscala.Address")
  lazy val sessionChannelClass = definitions.getClass("uk.ac.ic.doc.sessionscala.SessionChannel")
  lazy val bindMethod = definitions.getMember(addressTrait, "bind")
  lazy val startSessionMethod = definitions.getMember(addressObject, "startSession")
  lazy val qmarkMethods = definitions.getMember(sessionChannelClass, "$qmark")
  lazy val bangMethod = definitions.getMember(sessionChannelClass, "$bang")
  lazy val receiveMethod = definitions.getMember(sessionChannelClass, "receive")
  //lazy val receiveWithinMethod = definitions.getMember(sessionChannelClass, "receiveWithin")
  lazy val reactMethod = definitions.getMember(sessionChannelClass, "react")
  //lazy val reactWithinMethod = definitions.getMember(sessionChannelClass, "reactWithin")
  lazy val mreceiveMethod = definitions.getMember(sessionChannelClass, "mreceive")
  lazy val mreactMethod = definitions.getMember(sessionChannelClass, "mreact")
  
  lazy val symbolObject = definitions.getModule("scala.Symbol")
  lazy val symbolApplyMethod = definitions.getMember(symbolObject, "apply")
  lazy val symbolUnapplyMethod = definitions.getMember(symbolObject, "unapply")
  
  lazy val arrowObject = definitions.getModule("uk.ac.ic.doc.sessionscala.Address.->")
  lazy val arrowUnapplyMethod = definitions.getMember(arrowObject, "unapply")
}


