package uk.ac.ic.doc.sessionscala

import uk.ac.ic.doc.sessionscala.AMQPUtils._
import com.rabbitmq.client.{Channel, MessageProperties}
import actors.{TIMEOUT, Actor}, Actor._
import java.util.concurrent.TimeoutException


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

  def receive(): Any = receiveImpl(consumeOne)
  def receiveWithin(msec: Int) = receiveImpl(consumeOneWithin(msec))

  def receiveImpl(_consOne: (Channel, String) => Any) = withChan(fact) { chan =>
    ensureQueueExists(chan)
    _consOne(chan, queueName)
  }

  def derived(name: String) = copy(queueName = name)

  def bindWithin[T](msec: Int)(act: (SessionChannel) => T) = 
    bind(consumeOneWithin(msec), act)
  def bind[T](act: SessionChannel => T): T =
    bind(consumeOne, act)
  def bind[T](_consumeOne: (Channel, String) => Any, act: SessionChannel => T): T = {
    //println("bind: " + role + ", waiting for invite on: " + queueName)
    withChan(fact) { chan =>
      ensureQueueExists(chan)
      //println("bind "+role+" consuming invite on: "+queueName)
      val Invite(sessIDPort, invProtocol, invRole) = _consumeOne(chan, queueName)
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
        val roleAddresses = _consumeOne(chan, replyQueue).asInstanceOf[Map[Symbol,AMQPPrivateAddress]]
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
          //println("Sent Quit and got replies from: "+receiverActor+", "+participantsMap.values)
        }
      }
    }
  }

  def publish(chan: Channel, queue: String, msg: Any) {
    //println("publishing: "+msg+" to queue: "+queue)
    chan.basicPublish("", queue, MessageProperties.BASIC, serialize(msg))
    //println("done: "+msg+" to "+queue)
  }

  // Only use when there's a single message in the queue max guaranteed, otherwise reorderings can happen
  def consumeOne(chan: Channel, queue: String) = 
    consumeOneImpl(chan, queue, self.receive)
    
  def consumeOneWithin(msec: Int)(chan: Channel, queue: String) = {
    //println("calling consumeOneWithin: "+msec)
    consumeOneImpl(chan, queue, self.receiveWithin(msec))
  }
  
  def consumeOneImpl(chan: Channel, queue: String, receive: (PartialFunction[Any,Any]) => Any): Any = {
    // auto-ack: true
    //println("consumeOneImpl")
    val consumerTag = chan.basicConsume(queue, true, new SendMsgConsumer(chan, Actor.self))
    try {
      receive {
        case bytes: Array[Byte] => 
          deserialize(bytes)
        case TIMEOUT => throw new TimeoutException
      }
    } finally {
      //println("consumeOneImpl cancelling consume")
      chan.basicCancel(consumerTag)
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
