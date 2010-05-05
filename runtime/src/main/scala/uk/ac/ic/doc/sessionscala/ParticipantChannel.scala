package uk.ac.ic.doc.sessionscala

import actors.{Actor, Channel}

class ParticipantChannel(receiver: Actor) extends Channel[Any](receiver) {
  def delegate(s: SessionChannel) = {}
}