package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc
import nsc.Global

trait SessionDefinitions {
  val global: Global
  import global._

  // Lazy because otherwise global is not set yet, so NPE trying to access definitions
  lazy val protocolAnnotation = definitions.getClass("uk.ac.ic.doc.sessionscala.protocol")
  lazy val sharedChannelTrait = definitions.getClass("uk.ac.ic.doc.sessionscala.SharedChannel")
  lazy val participantChannelClass = definitions.getClass("uk.ac.ic.doc.sessionscala.ParticipantChannel")
  lazy val joinMethod = definitions.getMember(sharedChannelTrait, "join")
  lazy val qmarkMethod = definitions.getMember(participantChannelClass, "$qmark")
  lazy val bangMethod = definitions.getMember(participantChannelClass, "$bang")
  lazy val receiveMethod = definitions.getMember(participantChannelClass, "receive")
}


