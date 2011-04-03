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

  def react(role1: Symbol, role2: Symbol, otherRoles: Symbol*)
           (f: PartialFunction[(Symbol,Any), Unit]): Nothing = {
    self.react(buildBody(Set(role1,role2) ++ otherRoles, f))
  }
  
  def react(role: Symbol)(f: PartialFunction[Any,Unit]): Nothing = {
    self.react(buildBody(Set(role),addRole(role, f)))
  }
    
  def receive[T](role: Symbol)(f: PartialFunction[Any,T]): T = {
    self.receive(buildBody(Set(role),addRole(role, f)))
  }
  
  def receive[T](role1: Symbol, role2: Symbol, otherRoles: Symbol*)
                (f: PartialFunction[(Symbol,Any), T]): T = {
    self.receive(buildBody(Set(role1,role2) ++ otherRoles, f))
  }

  def addRole[T](role: Symbol, f: PartialFunction[Any,T]): PartialFunction[(Symbol,Any),T] = 
    new PartialFunction[(Symbol,Any),T] {
      def isDefinedAt(x: (Symbol, Any)) = x._1 == role && f.isDefinedAt(x._2)
      def apply(x: (Symbol, Any)) = f.apply(x._2)
    }
  
  def buildBody[T](roles: Set[Symbol], f: PartialFunction[(Symbol,Any), T]): PartialFunction[Any,T] = {
    val filtMap = map filterKeys(roles.contains _) 
    val srcRoles = filtMap map { case (role,cp) => (cp.fromOther -> role) }
    {
      case (chan: Channel[Any]) ! msg if srcRoles.contains(chan) => 
        f((srcRoles(chan), msg))
    }
  }

  def extractMsg(seq: Seq[Any]) = {
    seq match {
      case Seq(a) => a
      case Seq(a,b) => (a,b)
      case Seq(a,b,c) => (a,b,c)
      case Seq(a,b,c,d) => (a,b,c,d)
      case Seq(a,b,c,d,e) => (a,b,c,d,e)  
      case Seq(a,b,c,d,e,f) => (a,b,c,d,e,f)  
      case Seq(a,b,c,d,e,f,g) => (a,b,c,d,e,f,g)  
      // TODO extend to Tuple22
    }
  }
}