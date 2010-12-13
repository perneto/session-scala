package uk.ac.ic.doc.sessionscala

import actors.Actor
import Actor._
import messageformats.AMQPMessageFormats
import SharedChannel.localhost
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
    val chan = connectAndInitExchange()
    chan.queueDeclare(localhost, false, false, false, null)
    // noAck = true, automatically sends acks
    val consumerTag = chan.basicConsume(localhost, true, new SendMsgConsumer(chan, self))
    println("Invitation receiver is consuming messages...")

    loop {
      react {
        case body: Array[Byte] =>
          val (invitedRole,sessExchange,protocol) = openInvite(body)
          // todo: check protocol is compatible with the local protocol
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
    def sendAll(msg: Any) = {
      println("proxy registry sending: " + msg + " to proxies: " + mapProxies)
      mapProxies.values foreach (_ ! msg)
    }
    loop {
      react {
        case (role: Symbol, proxy: Actor) =>
          mapProxies += (role -> proxy)
          sendAll(mapProxies)
        case Exit =>
          println("Proxy registry exiting")
          // no need to send Exit to proxies here, this is done at the end of the accept
          // method in AMQPSharedChannel
          exit()
      }
    }
  }


}