package uk.ac.ic.doc.sessionscala

import actors._
import Actor._
import AMQPUtils._
import collection.mutable
import messageformats.AMQPMessageFormats

/**
 * Created by: omp08
 */

trait AMQPActorProxyComponent {
  thisObject: AMQPMessageFormats
         with AMQPConnectionComponent
         with CoordinationActorsComponent => // funny name because self is used up by Actor.self

  case class NewDestinationRole(role: Symbol, chan: actors.Channel[Any])
  case class NewSourceRole(role: Symbol, chan: actors.Channel[Any])
  case class DeserializedMsgReceive(fromRole: Symbol, label: Symbol, body: Any)

  class AMQPActorProxy(sessExchangeName: String, role: Symbol) extends Actor {
    var mapProxies = Map.empty[Symbol, Actor] // always empty at the moment, will implement optimized local communication later

    def publish(dstRole: Symbol, label: Symbol, msg: Any) {
      mapProxies.get(dstRole) match {
        case Some(localProxy: Actor) =>
          println("Direct message send to local proxy: " + localProxy + ", msg: " + msg)
          localProxy ! DeserializedMsgReceive(role, label, msg)
        case None => // always this case at the moment, will implement optimized local communication later
          println("Proxy for "+role+" is sending message " + msg + " to exchange " + sessExchangeName + " with routingKey: " + dstRole.name)
          chan.basicPublish(sessExchangeName, dstRole.name, null, 
            serialize(sessExchangeName, role, dstRole, label, msg))
      }
    }

    val chan = connect()
    val roleQueue = sessExchangeName + role.name
    // self instead of this gives the actor for the thread creating this object
    // self is only valid in the act method
    val consumerTag = chan.basicConsume(roleQueue, true, new SendMsgConsumer(chan, this))
    // noAck = true, automatically sends acks todo: probably should be false here
    println("Proxy for role "+role+" is consuming messages on queue: "+roleQueue+"...")

    val srcRoleChans = mutable.Map[Symbol, actors.Channel[Any]]()
    val exitSignals = mutable.Set[Symbol]()

    var reactBody: PartialFunction[Any, Unit] = {
      case NewDestinationRole(dstRole, actorChan) =>
        reactBody = reactBody orElse {
          case actorChan ! msg =>
            val (label, contents) = extractLabel(msg)
            publish(dstRole, label, contents)
        }
      case rawMsg: Array[Byte] =>
        val (srcRole, label, msg) = deserialize(rawMsg)
        println("Proxy for " + role + " received from: "
                + srcRole + ", label: " + label + ", msg: " + msg)
        self ! DeserializedMsgReceive(srcRole, label, msg)
      case NewSourceRole(role, actorChan) =>
        srcRoleChans += (role -> actorChan)
        println("In proxy for "+role+", expanded srcRoleChans: " + srcRoleChans)
      case DeserializedMsgReceive(role, label, msg) if srcRoleChans.isDefinedAt(role) =>
        // in case the mapping is not defined yet, the message will wait in the mailbox until it eventually is
        println("sending (" + label + ", "+ msg + ")"+" to channel " + srcRoleChans(role))
        srcRoleChans(role) ! buildTuple(label,msg)
      case Exit =>
        println("#############################Proxy for role "+role+" exiting")
        close(chan, consumerTag)
        exit()
    }

    def act = {
      proxyRegistryActor ! (role, self)
      loop { receive(reactBody) }
    }

    override def toString = "AMQPActorProxy(sessExchangeName=" + sessExchangeName + ", role=" + role + ")"

    def buildTuple(label: Symbol, msg: Any): Product = msg match {
      case Tuple1(msg) => (label, msg)
      case (a,b) => (label, a,b)
      case (a,b,c) => (label, a,b,c)
      case (a,b,c,d) => (label, a,b,c,d)
      case (a,b,c,d,e) => (label, a,b,c,d,e)
      case (a,b,c,d,e,f) => (label, a,b,c,d,e,f)
      case (a,b,c,d,e,f,g) => (label, a,b,c,d,e,f,g)
      case (a,b,c,d,e,f,g,h) => (label, a,b,c,d,e,f,g,h)
      case (a,b,c,d,e,f,g,h,i) => (label, a,b,c,d,e,f,g,h,i)
      case (a,b,c,d,e,f,g,h,i,j) => (label, a,b,c,d,e,f,g,h,i,j)
      case (a,b,c,d,e,f,g,h,i,j,k) => (label, a,b,c,d,e,f,g,h,i,j,k)
      case (a,b,c,d,e,f,g,h,i,j,k,l) => (label, a,b,c,d,e,f,g,h,i,j,k,l)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
      case (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) => (label, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
      // Scala tuples are defined up to Tuple22, so we covered everything.
      case x => (label, x)
    }

    def extractLabel(msg: Any): (Symbol, Any) = msg match {
      case label: Symbol => (label, "")
      case (label: Symbol, x) => (label, x)
      case (label: Symbol, a,b) => (label, (a,b))
      case (label: Symbol, a,b,c) => (label, (a,b,c))
      case (label: Symbol, a,b,c,d) => (label, (a,b,c,d))
      case (label: Symbol, a,b,c,d,e) => (label, (a,b,c,d,e))
      case (label: Symbol, a,b,c,d,e,f) => (label, (a,b,c,d,e,f))
      case (label: Symbol, a,b,c,d,e,f,g) => (label, (a,b,c,d,e,f,g))
      case (label: Symbol, a,b,c,d,e,f,g,h) => (label, (a,b,c,d,e,f,g,h))
      case (label: Symbol, a,b,c,d,e,f,g,h,i) => (label, (a,b,c,d,e,f,g,h,i))
      case (label: Symbol, a,b,c,d,e,f,g,h,i,j) => (label, (a,b,c,d,e,f,g,h,i,j))
      case (label: Symbol, a,b,c,d,e,f,g,h,i,j,k) => (label, (a,b,c,d,e,f,g,h,i,j,k))

      case x => (Symbol(""), x)
    }
  }
}