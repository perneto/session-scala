package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.Address.->
import actors.Actor, Actor._
import PartialFunction.cond

class SessionChannel(ourRole: Symbol, map: Map[Symbol, Actor]) {
  println("Creating SessionChannel: "+this+", map: "+map)
  def !(msg: (Symbol,Any)) = { 
    println("!: send to role: "+msg._1+" at: "+map(msg._1)+", msg:"+msg._2)
    map(msg._1) ! ->(ourRole, msg._2)
  }
  
  def ?[T](role: Symbol): T = {
    self.receive {
      case `role` -> (label: Symbol) => throw new IllegalStateException(
        "Trying to receive an unlabelled message, but got label")
      case `role` -> msg => 
        println(ourRole+" received msg:"+msg+" from:"+role)
        msg.asInstanceOf[T]
    }
  }

  def ?[T](role: Symbol, label: Symbol): T = {
    self.receive {
      case `role` -> (label: Symbol) => 
        println(ourRole+" received label:"+label+" from:"+role)
        ().asInstanceOf[T]
      case `role` -> msg => 
        println(ourRole+" received msg:"+msg+" from:"+role)
        dropLabel(label, msg)  
    }
  }

  def mreact(f: PartialFunction[Any, Unit]): Nothing = self.react(f)
  def mreceive[R](f: PartialFunction[Any, R]): R = self.receive(f)
  
  def react(role: Symbol)(f: PartialFunction[Any,Unit]): Nothing = {
    self.react(addRole(role, f))
  }
    
  def receive[R](role: Symbol)(f: PartialFunction[Any,R]): R = {
    self.receive(addRole(role, f))
  }
  
  def addRole[R](role: Symbol, f: PartialFunction[Any,R]) = 
    new PartialFunction[Any,R] {
      def isDefinedAt(x: Any) = cond(x) {
        case `role` -> contents => f.isDefinedAt(contents)
      }
      def apply(x: Any) = x match {
        case _ -> contents => f(contents)
      }
    }
  
  def dropLabel[R](label: Symbol, msg: Any): R = {
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
      case _ =>  throw new IllegalStateException(
        "Trying to receive a labelled message, but got unlabelled message")
    }
    f(msg).asInstanceOf[R]
  }
}