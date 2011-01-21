package uk.ac.ic.doc.sessionscala

import actors._

// chanFrom needs to be a Channel because receive is called from the outside
// code would compile with chanFrom: InputChannel[Any] but fail at runtime if an Actor is passed as parameter
class ParticipantChannel(val chanFrom: Channel[Any], val chanTo: OutputChannel[Any])  {
  def delegate(s: SessionChannel) = {}

  def !(msg: Any) = chanTo ! msg

  // Needs a return type different from AnyRef, otherwise this
  // and the other ? method have the same return type after erasure,
  // thus the class won't compile.
  def ??(): Product = {
    chanFrom.? match {
      case p: Product => p
      case s: Symbol =>
        (s,null)
        // This is meant to be pattern-matched like so:
        // val ('mylabel, _) = s('Foo).??
        // It's not a common case (most label-only messages should be used for branching)
        // so I consider the less-than-pretty syntax ok.
    }
  }

  def ?[R] = chanFrom.receive {
      case x: R => x
  }

  def react(f: PartialFunction[Any, Unit]) = chanFrom.react(f)

  def receive[R](f: PartialFunction[Any, R]) = chanFrom.receive(f)
}
