package uk.ac.ic.doc.sessionscala

import actors.Actor.self
import actors.{Channel, !}
import uk.ac.ic.doc.sessionscala.Port.->

class SessionChannel(ourRole: Symbol, map: Map[Symbol, ChannelPair]) {
  //println("Creating SessionChannel: "+this+", mapping: "+map)
  def !(msg: (Symbol,Any)) =  
      //println("send to role: "+role+" at: "+map(role).toOther)
      map(msg._1).toOther ! msg._2
  
  def ?[T](role: Symbol): T = {
    map(role).fromOther.receive {
      case label: Symbol =>
        throw new IllegalStateException("Trying to receive an unlabelled message, but got label")
      case msg: Any => msg.asInstanceOf[T]
    }
  }

  def ?[T](role: Symbol, label: Symbol): T = {
    map(role).fromOther.receive (dropLabel(label) orElse {
      case label: Symbol =>
        ().asInstanceOf[T]
      case msg: Any =>
        throw new IllegalStateException("Trying to receive a labelled message, but got unlabelled message")
    })
  }

  def react(role1: Symbol, role2: Symbol, otherRoles: Symbol*)
           (f: PartialFunction[->, Unit]): Nothing = {
    self.react(buildBody(Set(role1,role2) ++ otherRoles, f))
  }
  
  def react(role: Symbol)(f: PartialFunction[Any,Unit]): Nothing = {
    self.react(buildBody(Set(role),addRole(role, f)))
  }
    
  def receive[T](role: Symbol)(f: PartialFunction[Any,T]): T = {
    self.receive(buildBody(Set(role),addRole(role, f)))
  }
  
  def receive[T](role1: Symbol, role2: Symbol, otherRoles: Symbol*)
                (f: PartialFunction[->, T]): T = {
    self.receive(buildBody(Set(role1,role2) ++ otherRoles, f))
  }

  def addRole[T](role: Symbol, f: PartialFunction[Any,T]): PartialFunction[->,T] = 
    new PartialFunction[->,T] {
      def isDefinedAt(x: ->) = x.role == role && f.isDefinedAt(x.contents)
      def apply(x: ->) = f.apply(x.contents)
    }
  
  def buildBody[T](roles: Set[Symbol], f: PartialFunction[->, T]): PartialFunction[Any,T] = {
    val filtMap = map filterKeys(roles.contains _) 
    val srcRoles = for ((role,cp) <- filtMap) yield cp.fromOther -> role
    return {
      case (chan: Channel[Any]) ! msg if srcRoles.contains(chan) =>
        f(->(srcRoles(chan),msg))
    }
  }
  
  def dropLabel[T](label: Symbol): PartialFunction[Any,T] = {
    val f: PartialFunction[Any, Any] = {
      case (`label`,a) => a
      case (`label`,a,b) => (a,b)
      case (`label`,a,b,c) => (a,b,c)
      case (`label`,a,b,c,d) => (a,b,c,d)
      case (`label`,a,b,c,d,e) => (a,b,c,d,e)
      case (`label`,a,b,c,d,e,f) => (a,b,c,d,e,f)
      case (`label`,a,b,c,d,e,f,g) => (a,b,c,d,e,f,g)
      case (`label`,a,b,c,d,e,f,g,h) => (a,b,c,d,e,f,g,h)
      case (`label`,a,b,c,d,e,f,g,h,i) => (a,b,c,d,e,f,g,h,i)
      case (`label`,a,b,c,d,e,f,g,h,i,j) => (a,b,c,d,e,f,g,h,i,j)
      case (`label`,a,b,c,d,e,f,g,h,i,j,k) => (a,b,c,d,e,f,g,h,i,j,k)             
      // TODO extend to Tuple22
    }
    f.andThen(_.asInstanceOf[T])
  }
}