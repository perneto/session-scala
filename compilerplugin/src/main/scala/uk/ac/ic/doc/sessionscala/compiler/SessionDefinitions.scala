package uk.ac.ic.doc.sessionscala.compiler

import scala.tools.nsc
import nsc.Global

trait SessionDefinitions {
  val global: Global
  import global._

  // Lazy because otherwise global is not set yet, so NPE trying to access definitions
  lazy val protocolAnnotation = definitions.getClass("uk.ac.ic.doc.sessionscala.protocol")
  lazy val sharedChannelTrait = definitions.getClass("uk.ac.ic.doc.sessionscala.SharedChannel")
  lazy val inputChannelTrait = definitions.getClass("scala.actors.InputChannel")
  lazy val channelTrait = definitions.getClass("scala.actors.Channel")
  lazy val acceptMethod = definitions.getMember(sharedChannelTrait, "accept")
  lazy val instanceOfMethod = definitions.getMember(definitions.getClass("scala.Any"), "asInstanceOf")
  lazy val qmarkMethod = definitions.getMember(inputChannelTrait, "$qmark")
  lazy val bangMethod = definitions.getMember(channelTrait, "$bang")
  lazy val receiveMethod = definitions.getMember(inputChannelTrait, "receive")
}


