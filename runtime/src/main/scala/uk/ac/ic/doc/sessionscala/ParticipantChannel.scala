package uk.ac.ic.doc.sessionscala

import actors._

class ParticipantChannel(val chanFrom: Channel[Any], val chanTo: Channel[Any])  {
  def delegate(s: SessionChannel) = {}


  def !(msg: Any) = chanTo ! msg

  def ?[R] = chanFrom.receive {
      case x: R => x
  }

  def react(f: PartialFunction[Any, Unit]) = chanFrom.receive(f)

  def receive[R](f: PartialFunction[Any, R]) = chanFrom.receive(f)
}