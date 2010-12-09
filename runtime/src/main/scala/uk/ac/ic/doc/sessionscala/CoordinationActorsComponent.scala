package uk.ac.ic.doc.sessionscala

import actors.Actor
import Actor._
import SharedChannel.localhost
/**
 * Created by: omp08
 */

trait CoordinationActorsComponent {
  thisObject: MatchmakerActorComponent with AMQPActorProxyComponent with AMQPSharedChannel =>
  
  val invitationReceiverActor = actor {
    println("Starting invitation receiver actor...")
    val chan = connectAndInitExchange()
    chan.queueDeclare(localhost, false, false, false, null)
    // noAck = true, automatically sends acks
    val consumerTag = chan.basicConsume(localhost, true, new SendMsgConsumer(chan, self))
    println("Invitation receiver is consuming messages...")

    loop {
      react {
        case body: Array[Byte] =>
          val (invitedRole,sessExchange,protocol) = openInvite(body)
          matchMakerActor ! Invite(invitedRole, sessExchange, protocol)
          println("sent invitation for " + invitedRole + " to matchmaker")
        case Exit =>
          println("Invitation receiver exiting")
          close(chan, consumerTag)
          exit()
      }
    }
  }

  val proxyRegistryActor = actor {
    var mapProxies = Map.empty[Symbol, Actor]
    def sendAll(msg: Any) = mapProxies.values foreach (_ ! msg)
    loop {
      react {
        case (role: Symbol, proxy: Actor) =>
          mapProxies += (role -> proxy)
          sendAll(mapProxies)
        case Exit =>
          println("Proxy registry exiting")
          exit()
      }
    }
  }


}