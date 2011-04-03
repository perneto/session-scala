package uk.ac.ic.doc.sessionscala

import scala.actors.{Channel => _, _}, Actor._
import uk.ac.ic.doc.sessionscala.AMQPUtils._
import com.rabbitmq.client.{Channel, MessageProperties}

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
        val declareOK = chan.queueDeclare()
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
    self.receive {
      case bytes: Array[Byte] => 
        val msg = deserialize(bytes)
        chan.basicCancel(consumerTag)
        msg
    }
  }
  
  def ensureQueueExists(chan: Channel) {
    // args: durable, exclusive, autodelete, arguments (null if no extra args)
    
    // exclusive: false to allow more than 1 process to bind to this port,
    // creating a race for invites (a good thing, follows theory).
    // Would seem not needed in the simple case of 1 invite - 1 process,
    // but the ensureQueueExists done in send() then breaks. Exclusive really
    // means exclusive-everything, not just consume-exclusive.
    
    // auto-delete: false, as when several procs bind on this channel we don't want to lose any invites
    // it would seem to work as we call ensureQueueExists every time we bind(),
    // but an invite message could get lost in the following scenario:
    // 1. inviter creates queue
    // 2. inviter sends two invites
    // 3. process 1 binds, declares queue but does not create it as the inviter did,
    // consumes one invite
    // 4. process 1 finishes reading invite, stops listening to queue
    // 5. queue is auto-deleted, losing the second invite
    // 6. process 2 binds later, re-creates the queue, but the invite is no longer there
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

  override def close() = withChan(fact) { chan =>
    chan.queueDelete(queueName)
    //println("deleted: "+queueName)
  }
}
