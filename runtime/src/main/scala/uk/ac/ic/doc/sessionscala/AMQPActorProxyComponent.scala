package uk.ac.ic.doc.sessionscala

import actors.{Actor, !}, Actor._
import collection.mutable
import messageformats.AMQPMessageFormats
import com.rabbitmq.client.Channel
import AMQPUtils._

/**
 * Created by: omp08
 */

trait AMQPActorProxyComponent {
  this: AMQPPublicPort => 

  case class NewDestinationRole(role: Symbol, chan: actors.Channel[Any], 
                                address: (String, String, Int, String, String))
  case class NewSourceRole(role: Symbol, chan: actors.Channel[Any])
  case class DeserializedMsgReceive(fromRole: Symbol, label: Symbol, body: Option[Any])

  class AMQPActorProxy(amqpChan: Channel, roleQueue: String, ourRole: Symbol) extends Actor {
    // self instead of this would give the actor for the thread creating this object.
    // self is only valid in the act method
    amqpChan.basicConsume(roleQueue, true, new SendMsgConsumer(amqpChan, this))
    // noAck = true, automatically sends acks todo: probably should be false here
    println("Proxy for role "+ourRole+" is consuming messages on queue: "+roleQueue+"...")

    val srcRoleChans = mutable.Map[Symbol, actors.Channel[Any]]()
    var chansOtherBrokers = Set[Channel]()
    
    def sameBroker(broker: String, port: Int): Boolean =
      broker != amqpChan.getConnection.getHost || 
      port != amqpChan.getConnection.getPort
    
    var reactBody: PartialFunction[Any, Unit] = {
      case NewDestinationRole(dstRole, actorChan, address) =>
        val (otherQueue:String, otherBroker:String, otherPort:Int, otherUser:String, otherPwd:String) = address
        println("NewDestinationRole: ourRole: "+ourRole+", dstRole: "+dstRole+", queue: "+otherQueue+", actorChan: "+actorChan)
        val chanForRole = 
          if (!sameBroker(otherBroker, otherPort)) amqpChan
          else {
            val c = connect(createFactory(otherBroker, otherPort, otherUser, otherPwd))
            chansOtherBrokers += c
            c
          }
        reactBody = reactBody orElse {
          case actorChan ! msg =>
            println("Got message for "+dstRole+", sending to: "+otherQueue+", msg: "+msg+", on chan: "+actorChan)
            publish(chanForRole, otherQueue, (ourRole,msg))
        }
      case rawMsg: Array[Byte] =>
        val pair = deserialize(rawMsg)
        println(pair)
        val (senderRole:Symbol, msg) = pair
        println("Proxy for "+ourRole+" received: "+(senderRole,msg))
        srcRoleChans(senderRole) ! msg
      case NewSourceRole(srcRole, actorChan) =>
        srcRoleChans += (srcRole -> actorChan)
        //println("In proxy for "+ourRole+", expanded srcRoleChans: " + srcRoleChans)
      case Quit =>
        for (c <- chansOtherBrokers) close(c)
        //println("#############################Proxy for ourRole "+ourRole+" exiting")
        exit()
    }

    def act() = loop { react(reactBody) }


    override def !(msg: Any) {
      println("! on "+this+", msg:"+msg)
      super.!(msg)
    }

    override def toString = "AMQPActorProxy(roleQueue=" + roleQueue + ", ourRole=" + ourRole + ")"
  }
}