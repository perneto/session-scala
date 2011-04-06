package uk.ac.ic.doc.sessionscala

import com.rabbitmq.client.Channel
import AMQPUtils._, Address.->
import actors.Actor

/**
 * Created by: omp08
 */

trait AMQPActorProxyComponent {
  this: AMQPAddressImpl => 

  class AMQPActorReceiverForRole(roleActor: Actor, amqpChan: Channel, queueForRole: String) extends Actor {
    var consumerTag: String = null 
        
    override def start() = {
      // noAck = true, automatically sends acks todo: probably should be false here
      consumerTag = amqpChan.basicConsume(queueForRole, true, new SendMsgConsumer(amqpChan, this))
      //println("Receiver is consuming messages for "+roleActor+" on queue: "+queueForRole+"...")      
      super.start()
    }

    def act() = loop {
      react {
        case rawMsg: Array[Byte] =>
          val msgWithSender = deserialize(rawMsg).asInstanceOf[->]
          //println("AMQP receiver for "+roleActor+" received: "+msgWithSender)
          roleActor ! msgWithSender
      
        case Quit =>
          amqpChan.basicCancel(consumerTag)
          //println("Proxy for role "+ourRole+" exiting")
          reply(())
          exit()
      }
    }

    override def toString = "AMQPActorReceiverForRole(roleActor:"+roleActor+
        " amqpChan:"+amqpChan+" queueForRole:"+queueForRole+")"
  }
  
  class AMQPActorProxyToRole(dstRole: Symbol,
                             dstAddr: AMQPPrivateAddress,
                             existingAmqpChan: Channel) extends Actor {
    
    val AMQPPrivateAddress(
        queueToRole: String, otherBroker: String, otherPort: Int, 
        otherUser: String, otherPwd: String) = dstAddr
          
    val (chanToRole, chanCreated) = {
      if (!sameBroker(otherBroker, otherPort)) 
        (existingAmqpChan, false)
      else
        (connect(createFactory(otherBroker, otherPort, otherUser, otherPwd)), true)
    }

    def act() = loop { 
      react {
        case m@(srcRole -> msg) =>
          //println("Got message for "+dstRole+", sending to: "+queueToRole+", msg: "+msg)
          publish(chanToRole, queueToRole, m)
        case Quit =>
          if (chanCreated) AMQPUtils.close(chanToRole)
          reply(())
          exit()
      }
    }

    def sameBroker(broker: String, port: Int): Boolean =
        broker != existingAmqpChan.getConnection.getHost || 
        port != existingAmqpChan.getConnection.getPort
        
    override def toString = "AMQPActorProxyToRole(dstRole="+dstRole+
        ", dstAddr:"+dstAddr+", existingAmqpChan:"+existingAmqpChan+")"
  }
}