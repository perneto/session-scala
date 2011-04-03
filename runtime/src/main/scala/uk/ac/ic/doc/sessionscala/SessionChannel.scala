package uk.ac.ic.doc.sessionscala

import actors.Actor.self
import actors.{Channel, !}

case class ChannelPair(toOther: Channel[Any], fromOther: Channel[Any])

class SessionChannel(ourRole: Symbol, map: Map[Symbol, ChannelPair]) {
  //println("Creating SessionChannel: "+this+", mapping: "+map)
  def !(msg: (Symbol, Any)) = msg match {
    case (role: Symbol, msg: Any) => 
      //println("send to role: "+role+" at: "+map(role).toOther)
      map(role).toOther ! msg
  }

  def ?[T](role: Symbol): T = {
    map(role).fromOther.receive {
      case Labelled(label: Symbol, msg: Any) =>
        throw new IllegalStateException("Trying to receive an unlabelled message, but got labelled message")
      case label: Symbol =>
        throw new IllegalStateException("Trying to receive an unlabelled message, but got label")
      case msg: Any => msg.asInstanceOf[T]
    }
  }

  def ?[T](role: Symbol, label: Symbol): T = {
    map(role).fromOther.receive {
      case Labelled(`label`, msg: Any) =>
        val seq = msg.asInstanceOf[Seq[Any]]
        extractMsg(seq).asInstanceOf[T]
      case label: Symbol =>
        ().asInstanceOf[T]
      case msg: Any =>
        throw new IllegalStateException("Trying to receive a labelled message, but got unlabelled message")
    }
  }

  def react(role: Symbol, otherRoles: Symbol*)(f: PartialFunction[(Symbol,Any), Unit]): Nothing = {
    self.react(buildBody(role, otherRoles, f))
  }

  def receive[T](role: Symbol, otherRoles: Symbol*)(f: PartialFunction[(Symbol,Any), T]): T = {
    self.receive(buildBody(role, otherRoles, f))
  }

  def buildBody[T](role: Symbol, otherRoles: Seq[Symbol], f: PartialFunction[(Symbol,Any), T]) = {
    var body: PartialFunction[Any, T] = Map()
    for (srcRole <- role +: otherRoles)
      body = body orElse {
        case chan ! msg if chan == map(srcRole).fromOther => f((srcRole, msg))
      }
    body
  }

  def extractMsg(seq: Seq[Any]) = {
    seq match {
      case Seq(a) => a
      case Seq(a,b) => (a,b)
      case Seq(a,b,c) => (a,b,c)
      case Seq(a,b,c,d) => (a,b,c,d)
      // TODO extend to Tuple22
    }
  }
}