package uk.ac.ic.doc.sessionscala

import actors._

// chanFrom needs to be a Channel because receive is called from the outside
// code would compile with chanFrom: InputChannel[Any] but fail at runtime if an Actor is passed as parameter
class ParticipantChannel(val chanFrom: Channel[Any], val chanTo: OutputChannel[Any])  {
  def delegate(s: SessionChannel) = {}

  def !(msg: Any) = chanTo ! msg

  def ?[R] = chanFrom.receive {
      case x: R => x
  }

  def react(f: PartialFunction[Any, Unit]) = chanFrom.receive(f)

  def receive[R](f: PartialFunction[Any, R]) = chanFrom.receive(f)
}
