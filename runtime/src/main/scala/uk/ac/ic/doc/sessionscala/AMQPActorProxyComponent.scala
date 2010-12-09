package uk.ac.ic.doc.sessionscala

import actors._
import Actor._
import AMQPUtils._
import collection.mutable

/**
 * Created by: omp08
 */

trait AMQPActorProxyComponent {
  thisObject: AMQPSharedChannel=> //MatchmakerActorComponent with AMQPMessageFormats => // funny name because self is used up by Actor.self

  case class NewDestinationRole(role: Symbol, chan: actors.Channel[Any])
  case class NewSourceRole(role: Symbol, chan: actors.Channel[Any])
  case class DeserializedMsgReceive(fromRole: Symbol, body: Any)

  class AMQPActorProxy(exchange: String, role: Symbol) extends Actor {
    var mapProxies = Map.empty[Symbol, Actor] // always empty at the moment, will implement optimized local communication later

    def publish(dstRole: Symbol, msg: Any) {
      mapProxies.get(dstRole) match {
        case Some(localProxy: Actor) =>
          println("Direct message send to local proxy: " + localProxy + ", msg: " + msg)
          localProxy ! DeserializedMsgReceive(role, msg)
        case None => // always this case at the moment, will implement optimized local communication later
          println("Proxy for "+role+" is sending message " + msg + " to exchange " + exchange + " with routingKey: " + dstRole.name)
          chan.basicPublish(exchange, dstRole.name, null, serialize(role, msg))
      }
    }

    val chan = connect(factory)
    val roleQueue = exchange + role.name
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
          case actorChan ! msg => publish(dstRole, msg)
        }
      case rawMsg: Array[Byte] =>
        val (srcRole, msg) = deserialize(rawMsg)
        println("Proxy for " +role+ " received from: " + srcRole + ", msg: " + msg)
        self ! DeserializedMsgReceive(srcRole, msg)
      case NewSourceRole(role, actorChan) =>
        srcRoleChans += (role -> actorChan)
        println("In proxy for "+role+", expanded srcRoleChans: " + srcRoleChans)
      case DeserializedMsgReceive(role, msg) if srcRoleChans.isDefinedAt(role) =>
        // in case the mapping is not defined yet, the message will wait in the mailbox until it eventually is
        println("sending " + msg + " to channel " + srcRoleChans(role))
        srcRoleChans(role) ! msg
      case Exit =>
        println("Proxy for role "+role+" exiting")
        close(chan, consumerTag)
        exit()
    }

    def act = {
      proxyRegistryActor ! (role, self)
      loop {
        receive(reactBody)
      }
    }

    override def toString = "AMQPActorProxy(exchange=" + exchange + ", role=" + role + ")"
  }
}