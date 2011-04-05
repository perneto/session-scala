package uk.ac.ic.doc.sessionscala

import actors.Channel

case class ChannelPair(toOther: Channel[Any], fromOther: Channel[Any])
case class Invite(replyPort: Address, protocol: String, role: Symbol)
case class AcceptedInvite(role: Symbol, replyPort: PrivateAddress, port: PrivateAddress)
trait PrivateAddress {
  def send(msg: Any)
}