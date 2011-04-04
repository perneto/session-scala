package uk.ac.ic.doc.sessionscala

import actors.Channel

case class ChannelPair(toOther: Channel[Any], fromOther: Channel[Any])
case class Invite(replyPort: Port, protocol: String, role: Symbol)
case class AcceptedInvite(role: Symbol, replyPort: PrivatePort, port: PrivatePort)
trait PrivatePort {
  def send(msg: Any)
}