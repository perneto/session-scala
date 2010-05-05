package uk.ac.ic.doc.sessionscala

import actors.{Channel, InputChannel}

trait SessionChannel extends (String => ParticipantChannel) with InputChannel[Any] {
  def receiveDelegated: SessionChannel = null
}

private[sessionscala] class SessionChannelImpl(map: (String => ParticipantChannel), var channel: Channel[Any])
        extends SessionChannel
{
  def apply(role: String) = map.apply(role)

  def ? = channel.?

  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]) = channel.reactWithin(msec)(f)

  def react(f: PartialFunction[Any, Unit]) = channel.react(f)

  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]) = channel.receiveWithin(msec)(f)

  def receive[R](f: PartialFunction[Any, R]) = channel.receive(f)

  override def toString = "SessionChannelImpl{map:"+map+", channel: "+channel+"}"
}
