package uk.ac.ic.doc.sessionscala

import scala.actors.{Channel => _, _}, Actor._
import uk.ac.ic.doc.sessionscala.AMQPUtils._
import com.rabbitmq.client.{Channel, MessageProperties, ConnectionFactory}

case class AMQPPublicPort(protocol: String, role: Symbol,
                     queueName: String,
                     brokerHost: String, port: Int,
                     user: String, pwd: String)
        extends PublicPort
        with AMQPSimpleMessageFormats
        with AMQPActorProxyComponent {

  //not val to avoid breaking serialize
  def fact = createFactory(brokerHost, port, user, pwd)  
  
  def send(msg: Any) = withChan(fact) { chan =>
    //println("Port for "+role+": send msg: "+msg+" to queue: "+queueName)
    ensureQueueExists(chan)
    //println("declared queue "+queueName)
    publish(chan, queueName, msg)
  }

  def receive(): Any = withChan(fact) { chan =>
    //println("Port for "+role+": receive from: "+queueName)
    ensureQueueExists(chan)
    consumeOne(chan, queueName)
  }

  def derived(name: String) = copy(queueName = name)

  def bind[T](act: SessionChannel => T): T = {
    //println("bind: " + role + ", waiting for invite on: " + queueName)
    withChan(fact) { chan =>
      ensureQueueExists(chan)
      //println("bind "+role+" consuming invite on: "+queueName)
      val Invite(sessIDPort, invProtocol, invRole) = consumeOne(chan, queueName)
      //println("got invite: " + invRole)
      
      if (role != invRole) {
        println("WARNING: Got invite for another role: "+invRole+
                ", expecting "+role+". Discarding and waiting again...")
        bind(act)
      } else if (!protocolsCompatible(protocol, invProtocol)) {
        println("WARNING: Got invite for correct role: "+invRole+
                ", but incompatible protocols. Local: "+protocol+", invitation: "+
                invProtocol+". Discarding and waiting again...")
        bind(act)
      } else {  
        // server-named, non-durable, exclusive, non-autodelete
        // non-autodelete because there is a short consume interruption between mapping receive and ActorProxy start
        val declareOK = chan.queueDeclare("",false,true,false,null)
        val sessionQueue = declareOK.getQueue
        
        val declareOk2 = chan.queueDeclare()
        val replyQueue = declareOk2.getQueue
        sessIDPort.send(
          AcceptedInvite(invRole,
                         AMQPPrivatePort(replyQueue, brokerHost, port, user, pwd),
                         AMQPPrivatePort(sessionQueue, brokerHost, port, user, pwd))
        )
        val mapping = consumeOne(chan, replyQueue).asInstanceOf[Map[Symbol,AMQPPrivatePort]]
        // autodelete replyQueue is deleted here (after consumer exits)
        
        //println("Got mapping from inviter: "+mapping)
        
        val proxy = new AMQPActorProxy(mapping, chan, sessionQueue, role)
        val chanMap = (protocolRoles foldLeft Map[Symbol, ChannelPair]()) { case (result, otherRole) =>
          // binds the channel to the current actor (Actor.self). 
          // self is the only actor that can receive on the channel
          val chanFrom = new actors.Channel[Any]() 
          val chanTo = new actors.Channel[Any](proxy)
          result + (otherRole -> new ChannelPair(chanTo, chanFrom))
        }
            
        proxy.setChanMap(chanMap)      
        //println(role+": Mapping: "+chanMap)
        
        proxy.start()      
        val res = act(new SessionChannel(role, chanMap))
        //println("!!!!!!!!!!!!!!!!bind: call to act finished")
        proxy !? Quit
        res
      }
    }
  }

  def publish(chan: Channel, queue: String, msg: Any) {
    //println("publishing: "+msg+" to queue: "+queue)
    chan.basicPublish("", queue, MessageProperties.BASIC, serialize(msg))
    //println("done: "+msg+" to "+queue)
  }

  // Only use when there's a single message in the queue max guaranteed
  def consumeOne(chan: Channel, queue: String): Any = {
    // auto-ack: true
    val consumerTag = chan.basicConsume(queue, true, new SendMsgConsumer(chan, Actor.self))
    val msg = Actor.self.receive {
      case bytes: Array[Byte] => deserialize(bytes)
    }
    chan.basicCancel(consumerTag)
    msg
  }
  
  def ensureQueueExists(chan: Channel) {
    // durable: false, exclusive: true, autodelete: false, arguments: none (null)
    //chan.queueDeclare(queueName, false, true, false, null)
    // temp: exclusive: false - seems there's a race somewhere, or someone isn't closing
    chan.queueDeclare(queueName, false, false, false, null)
  }
  
  case class AMQPPrivatePort(privateQueue: String, brokerHost: String, port: Int,
                             user: String, pwd: String) extends PrivatePort {
    // not serializable, so recreate always
    def fact = createFactory(brokerHost, port, user, pwd)  
    def send(msg: Any) = withChan(fact) { chan =>      
      publish(chan, privateQueue, msg)
    }
    
    def address = (privateQueue, brokerHost, port, user, pwd)
  }
  
}
