package uk.ac.ic.doc.sessionscala

import actors._

class ParticipantChannel(val chanFrom: InputChannel[Any], val chanTo: OutputChannel[Any])  {
  def delegate(s: SessionChannel) = {}


  def !(msg: Any) = chanTo ! msg

  def ?[R] = chanFrom.receive {
      case x: R => x
  }

  def react(f: PartialFunction[Any, Unit]) = chanFrom.receive(f)

  def receive[R](f: PartialFunction[Any, R]) = chanFrom.receive(f)
}
