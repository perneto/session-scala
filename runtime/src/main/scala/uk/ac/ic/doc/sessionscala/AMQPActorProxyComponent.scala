package uk.ac.ic.doc.sessionscala

import actors.{Actor, Channel, !}
import com.rabbitmq.client.{Channel => AMQPChannel}
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

  class AMQPActorProxy(sessMap: Map[Symbol,AMQPPrivatePort],
                       amqpChan: AMQPChannel, roleQueue: String, ourRole: Symbol) extends Actor {
    // self instead of this would give the actor for the thread creating this object.
    // self is only valid in the act method
    var consumerTag: String = null 
    
    var chanMap: Map[Symbol, ChannelPair] = null
    var outputChans: Map[Channel[Any], (Symbol, String, AMQPChannel)] = null
    var srcRoleChans: Map[Symbol, Channel[Any]] = null
    var set = false
    def setChanMap(_chanMap: Map[Symbol,ChannelPair]) {
      assert(!set); set = true
      chanMap = _chanMap
      srcRoleChans = for ((role, ChannelPair(toRole, fromRole)) <- chanMap)
                     yield (role -> fromRole)  
            
      outputChans = chanMap map { case (role, ChannelPair(toRole, fromRole)) =>
        val (queueToRole: String, otherBroker: String, otherPort: Int, 
             otherUser: String, otherPwd: String) = sessMap(role).address
        val chanForRole = 
          if (!sameBroker(otherBroker, otherPort)) amqpChan
          else {
            val c = connect(createFactory(otherBroker, otherPort, otherUser, otherPwd))
            openedChans += c
            c              
          }
        (toRole -> (role, queueToRole, chanForRole))
      }          
    }
    var openedChans = Set[AMQPChannel]()


    override def start() = {
      // noAck = true, automatically sends acks todo: probably should be false here
      consumerTag = amqpChan.basicConsume(roleQueue, true, new SendMsgConsumer(amqpChan, this))
      //println("Proxy for role "+ourRole+" is consuming messages on queue: "+roleQueue+"...")      
      super.start()
    }

    def act() = loop { 
      react {
        case (chan: Channel[Any]) ! msg if outputChans.contains(chan) =>
          val dstRole = outputChans(chan)._1
          val otherQueue = outputChans(chan)._2
          val chanForRole = outputChans(chan)._3
          //println("Got message for "+dstRole+", sending to: "+otherQueue+", msg: "+msg+", on chan: "+chan)
          publish(chanForRole, otherQueue, (ourRole,msg))
          
        case rawMsg: Array[Byte] =>
          val pair = deserialize(rawMsg)
          val (senderRole:Symbol, msg) = pair
          //println("Proxy for "+ourRole+" received: "+pair)
          srcRoleChans(senderRole) ! msg
        
        case Quit =>
          amqpChan.basicCancel(consumerTag)
          for (c <- openedChans) AMQPUtils.close(c)
          //println("Proxy for role "+ourRole+" exiting")
          reply(())
          exit()
      }
    }

    def sameBroker(broker: String, port: Int): Boolean =
          broker != amqpChan.getConnection.getHost || 
          port != amqpChan.getConnection.getPort
        
    override def toString = "AMQPActorProxy(roleQueue=" + roleQueue + ", ourRole=" + ourRole + ")"
  }
}