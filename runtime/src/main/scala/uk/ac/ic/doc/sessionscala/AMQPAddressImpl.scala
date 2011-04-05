package uk.ac.ic.doc.sessionscala

import scala.actors.{Channel => _, _}, Actor._
import uk.ac.ic.doc.sessionscala.AMQPUtils._
import com.rabbitmq.client.{Channel, MessageProperties}

case class AMQPAddressImpl(protocol: String, role: Symbol,
                     queueName: String,
                     brokerHost: String, port: Int,
                     user: String, pwd: String)
        extends Address
        with AMQPSimpleMessageFormats
        with AMQPActorProxyComponent {

  //not val to avoid breaking serialize
  def fact = createFactory(brokerHost, port, user, pwd)  
  
  def send(msg: Any) = withChan(fact) { chan =>
    //println("Address for "+role+": send msg: "+msg+" to queue: "+queueName)
    ensureQueueExists(chan)
    //println("declared queue "+queueName)
    publish(chan, queueName, msg)
  }

  def receive(): Any = withChan(fact) { chan =>
    //println("Address for "+role+": receive from: "+queueName)
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
                         AMQPPrivateAddress(replyQueue, this),
                         AMQPPrivateAddress(sessionQueue, this))
        )
        val roleAddresses = consumeOne(chan, replyQueue).asInstanceOf[Map[Symbol,AMQPPrivateAddress]]
        // autodelete replyQueue is deleted here (after consumer exits)
        // autodelete sessionQueue has not been consumed from yet, so still valid
        
        val participantsMap = protocolRoles map { otherRole =>
            val proxy = 
              new AMQPActorProxyToRole(otherRole, roleAddresses(otherRole), chan)
            proxy.start()          
            (otherRole -> proxy)          
        } toMap
        
        // receiver actor starts consuming on sessionQueue
        val receiverActor = new AMQPActorReceiverForRole(self, chan, sessionQueue)
        receiverActor.start()
        
        try {
          act(new SessionChannel(role, participantsMap))
        //println("!!!!!!!!!!!!!!!!bind: call to act finished")
        } finally {
          receiverActor !? Quit
          // sessionQueue autodeleted here, after receiverActor stops consuming.
          for (a <- participantsMap.values) a !? Quit
          println("Sent Quit and got replies from: "+receiverActor+", "+participantsMap.values)
        }
      }
    }
  }

  def publish(chan: Channel, queue: String, msg: Any) {
    println("publishing: "+msg+" to queue: "+queue)
    chan.basicPublish("", queue, MessageProperties.BASIC, serialize(msg))
    //println("done: "+msg+" to "+queue)
  }

  // Only use when there's a single message in the queue max guaranteed
  def consumeOne(chan: Channel, queue: String): Any = {
    // auto-ack: true
    println("consumeOne")
    val consumerTag = chan.basicConsume(queue, true, new SendMsgConsumer(chan, Actor.self))
    self.receive {
      case bytes: Array[Byte] => 
        val msg = deserialize(bytes)
        chan.basicCancel(consumerTag)
        println("consumeOne cancelling consume")
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
  
  object AMQPPrivateAddress {
    def apply(privQ: String, parent: AMQPAddressImpl): AMQPPrivateAddress =
      AMQPPrivateAddress(privQ, parent.brokerHost, parent.port, parent.user, parent.pwd)
  }
  case class AMQPPrivateAddress(privateQueue: String, brokerHost: String, port: Int,
                             user: String, pwd: String) extends PrivateAddress {
    // not serializable, so recreate always
    def fact = createFactory(brokerHost, port, user, pwd)  
    def send(msg: Any) = withChan(fact) { chan =>      
      publish(chan, privateQueue, msg)
    }
  }

  override def close() = withChan(fact) { chan =>
    chan.queueDelete(queueName)
    //println("deleted: "+queueName)
  }
}
