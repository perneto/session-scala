package uk.ac.ic.doc.sessionscala

import actors.Actor
import Actor._
import messageformats.AMQPMessageFormats
import SharedChannel.localhost
import actors.Actor

/**
 * Created by: omp08
 */

trait CoordinationActorsComponent {
  thisObject: MatchmakerActorComponent
          with AMQPMessageFormats
          with AMQPActorProxyComponent
          with AMQPConnectionComponent =>
  
  val invitationReceiverActor = actor {
    println("Starting invitation receiver actor...")
    connectionManagerActor ! (('queueDeclare, localhost))
    // noAck = true, automatically sends acks
    connectionManagerActor ! (('consume, localhost, self))
    println("Invitation receiver is consuming messages...")

    loop {
      react {
        case body: Array[Byte] =>
          val (invitedRole,sessExchange,protocol) = openInvite(body)
          // todo: check protocol is compatible with the local protocol
          matchMakerActor ! Invite(invitedRole, sessExchange, protocol)
          println("sent invitation for " + invitedRole + " to matchmaker")
        case Quit =>
          println("Invitation receiver exiting")
          exit()
      }
    }
  }

  val proxyRegistryActor = actor {
    // This actor is useless at the moment; it was added to support optimization of
    // local messages (to avoid sending them out to the broker and back), but there's a race condition
    // at startup so I left it aside for now.
    var mapProxies = Map.empty[Symbol, Actor]
    def sendAll(msg: Any) = {
      println("proxy registry sending: " + msg + " to proxies: " + mapProxies)
      mapProxies.values foreach (_ ! msg)
    }
    loop {
      react {
        case (role: Symbol, proxy: Actor) =>
          mapProxies += (role -> proxy)
          //sendAll(mapProxies)
        case Quit =>
          println("Proxy registry exiting")
          // no need to send Quit to proxies here, this is done at the end of the accept
          // method in AMQPSharedChannel
          exit()
      }
    }
  }


}