package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc
import nsc.Global

trait SessionDefinitions {
  val global: Global
  import global._

  // Lazy because otherwise global is not set yet, so NPE trying to access definitions
  lazy val sharedChannelTrait = definitions.getClass("uk.ac.ic.doc.sessionscala.Address")
  lazy val participantChannelClass = definitions.getClass("uk.ac.ic.doc.sessionscala.ParticipantChannel")
  lazy val joinMethod = definitions.getMember(sharedChannelTrait, "join")
  lazy val acceptMethod = definitions.getMember(sharedChannelTrait, "bind")
  lazy val inviteMethod = definitions.getMember(sharedChannelTrait, "invite")
  lazy val qmarkMethod = definitions.getMember(participantChannelClass, "$qmark")
  lazy val qqMarkMethod = definitions.getMember(participantChannelClass, "$qmark$qmark")
  lazy val bangMethod = definitions.getMember(participantChannelClass, "$bang")
  lazy val receiveMethod = definitions.getMember(participantChannelClass, "receive")
  lazy val reactMethod = definitions.getMember(participantChannelClass, "receive")

  lazy val symbolObject = definitions.getModule("scala.Symbol")
  lazy val symbolApplyMethod = definitions.getMember(symbolObject, "apply")
  lazy val symbolUnapplyMethod = definitions.getMember(symbolObject, "unapply")
}


