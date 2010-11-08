package uk.ac.ic.doc.sessionscala

import AMQPUtils._
import com.rabbitmq.client._
import SharedChannel._
import scala.actors.{Channel => _, _}
import Actor._

class AMQPSharedChannel(awaiting: Set[Symbol], brokerHost: String, port: Int, user: String, password: String) extends SharedChannel(awaiting) {
  val factory = createFactory(brokerHost, port, user, password)

  val scribbleType = "protocol Foo {}"

  def join(role: Symbol)(act: ActorFun): Unit = { throw new IllegalStateException("TODO") }

  def invite(mapping: (Symbol,String)*): Unit = {
    checkMapping(mapping)

    val initChan = connectAndInitExchange()
    val (chan,sessName) = initSessionExchange(initChan)
    mapping foreach { case (role, host) =>
      declareInvitationQueueForHost(chan, host)
      declareSessionRoleQueue(chan, sessName, role)
      publishInvite(chan, role, host)
    }
    close(chan)
  }

  def publishInvite(chan: Channel, role: Symbol, host: String) {
    val msgBytes = ("s1," + role.name + "," + scribbleType).getBytes(CHARSET)
    chan.basicPublish(INIT_EXCHANGE, host, null, msgBytes)
  }

  def declareInvitationQueueForHost(chan: Channel, host: String) {
    //Parameters to queueDeclare: (queue, durable, exclusive, autoDelete, arguments)
    chan.queueDeclare(host, false, false, false, null)
    //Parameters to queueBind: (queue = host, exchange = INIT_EXCHANGE, routingKey = host)
    chan.queueBind(host, INIT_EXCHANGE, host)
  }
  
  def declareSessionRoleQueue(chan: Channel, sessName: String, role: Symbol) {
    val roleQueue = sessName + role.name
    chan.queueDeclare(roleQueue, false, false, false, null)
    chan.queueBind(roleQueue, sessName, role.name)
  }

  def initSessionExchange(initChan: Channel): (Channel,String) = {
    var i = 1; var notDeclared = true; var chan = initChan
    def sessName = "s" + i
    while (notDeclared) {
      try {
        println("initSessionExchange trying exchange name: " + sessName + ", i: " + i)
        chan.exchangeDeclarePassive(sessName) // throws ioe if exchange does not exist. This closes the channel, why oh why?
        // exchange already exists, try another name
        i += 1
      } catch {
        case ioe: java.io.IOException => 
          chan = chan.getConnection.createChannel // rabbitmq client api stupidness
          chan.exchangeDeclare(sessName, "direct") // this only runs if the exchange didn't already exists
          notDeclared = false
      }
    }
    (chan, sessName)
  }

  def close(chan: Channel) {
    chan.getConnection.close()
  }

  def contains[K,V](seq: Seq[(K,V)], k: K): Boolean = {
    for ((key, _) <- seq) {
      if (k == key) return true
    }
    false
  }

  def checkMapping(mapping: Seq[(Symbol,String)]) {
    val declaredRoles = Set() ++ mapping map (_._1)
    if (declaredRoles != awaiting)
      throw new IllegalArgumentException("Missing or extra roles in invite. Awaiting: " + awaiting + ", invited: " + declaredRoles)
  }

  override def close() {
    invitationReceiverActor ! Exit
    matchMakerActor ! Exit
    proxyRegistryActor ! Exit
  }

  case object Exit

  class SendMsgConsumer(chan: Channel, dest: Actor) extends DefaultConsumer(chan) {
    override def handleDelivery(consumerTag: String, env: Envelope, prop: AMQP.BasicProperties, body: Array[Byte]) {
      println("Received for dst: "+dest+", body: " + body)
      dest ! body
    }
  }

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
          val (invitedRole,sessExchange) = openInvite(body)
          matchMakerActor ! Invite(invitedRole, sessExchange)
          println("sent invitation for " + invitedRole + " to matchmaker")
        case Exit => 
          println("Invitation receiver exiting") 
          closeAndExit(chan, consumerTag)
      }
    }
  }

  def openInvite(body: Array[Byte]): (Symbol,String) = {
    val msg = new String(body, CHARSET)
    val Array(exchange, role, protocol) = msg.split(",")
    // todo: check protocol is compatible with the local protocol
    println("received for session: exchange: " + exchange + ", role: " + role + ", protocol: " + protocol)
    (Symbol(role), exchange)
  }

  def closeAndExit(chan: Channel, consumerTag: String) {
    chan.basicCancel(consumerTag) 
    close(chan)
    exit()
  }

  case class Invite(role: Symbol, sessExchange: String) 
  case class Accept(role: Symbol)

  val matchMakerActor = actor { 
    println("started matchmaker")
    
    var invites = Map.empty[Symbol, List[String]]
    var accepts = Map.empty[Symbol, List[OC]]

    def matchAccept(acceptRole: Symbol, acceptSender: OC) {
      invites get acceptRole match {
        case Some(x :: xs) =>
          acceptSender ! x
          if (xs == Nil) invites -= acceptRole 
          else invites += (acceptRole -> xs)
        case _ =>
          invites -= acceptRole
          accepts += acceptRole -> (acceptSender :: accepts.getOrElse(acceptRole, Nil))
      }
    }

    def matchInvite(invite: Invite) {
     accepts get invite.role match {
       case Some(x :: xs) =>
         x ! invite.sessExchange
         if (xs == Nil) accepts -= invite.role 
         else accepts += (invite.role -> xs)
       case _ =>
         accepts -= invite.role
         invites += invite.role -> (invite.sessExchange :: invites.getOrElse(invite.role, Nil))
     }
    }

    loop {
      react {
        case i: Invite => 
          matchInvite(i)
        case Accept(acceptRole: Symbol) => 
          matchAccept(acceptRole, sender)
        case Exit => println("matchmaker exiting...")
      }
    }
  }

  val proxyRegistryActor = actor {
    def loop(listProxies: List[Actor]) {
      react {
        case proxy: Actor => loop(proxy :: listProxies)
        case Exit => {
          println("Proxy registry terminating proxies: " + listProxies)
          listProxies foreach (_ ! Exit)
        }
      }
    }
    loop(Nil)
  }

  // todo: proper serialization
  val INT_CODE: Byte = 0
  val STRING_CODE: Byte = 1
  val BIG_ENOUGH = 8192
  import java.nio.ByteBuffer
  def serialize(srcRole: Symbol, msg: Any): Array[Byte] = {
    val buf = ByteBuffer.allocate(BIG_ENOUGH)
    val srcBytes = srcRole.name.getBytes(CHARSET)
    assert(srcBytes.length < 256)
    buf.put(srcBytes.length.asInstanceOf[Byte])
    buf.put(srcBytes)
    msg match {
      case s: String => 
        buf.put(STRING_CODE)
        buf.putInt(s.length)
        buf.put(s.getBytes(CHARSET))
      case i: Int => 
        buf.put(INT_CODE)
        buf.putInt(i)
      case x => throw new IllegalArgumentException("Serialization not supported for: " + x)
    }
    val result = Array.ofDim[Byte](buf.position)
    buf.flip()
    buf.get(result)
    println("serialize (" + srcRole + "," + msg + "): " + java.util.Arrays.toString(result))
    result
  }
  def deserialize(msg: Array[Byte]): (Symbol, Any) = {
    val buf = ByteBuffer.wrap(msg)
    val length = buf.get()
    val roleBytes = Array.ofDim[Byte](length)
    buf.get(roleBytes)
    val role = Symbol(new String(roleBytes, CHARSET))
    val typeCode = buf.get()
    val result = typeCode match {
      case INT_CODE => (role, buf.getInt()) // big-endian
      case STRING_CODE =>
        val length = buf.getInt()
        val stringBytes = Array.ofDim[Byte](length)
        buf.get(stringBytes)
        (role, new String(stringBytes, CHARSET))
      case t => throw new IllegalArgumentException("Unsupported type code in deserialize: " + t)
    }
    println("deserialize: " + result)
    result
  }

  case class NewDestinationRole(role: Symbol, chan: actors.Channel[Any])
  case class NewSourceRole(role: Symbol, chan: actors.Channel[Any])
  case class DeserializedMsgReceive(fromRole: Symbol, body: Any)

  import collection.mutable
  class AMQPActorProxy(exchange: String, role: Symbol) extends Actor { 
      val chan = connect(factory)
      val roleQueue = exchange + role.name
      // self instead of this gives the actor for the thread creating this object
      // self is only valid in the act method
      val consumerTag = chan.basicConsume(roleQueue, true, new SendMsgConsumer(chan, this))
      // noAck = true, automatically sends acks todo: probably should be false here
      println("Proxy for role "+role+" is consuming messages on queue: "+roleQueue+"...")

      val srcRoleChans = mutable.Map[Symbol, actors.Channel[Any]]()

      var reactBody: PartialFunction[Any, Unit] = {
        case NewDestinationRole(dstRole, actorChan) =>
          reactBody = reactBody orElse {
            case actorChan ! msg => 
              println("Proxy for "+role+" is sending message " + msg + " to exchange " + exchange + " with routingKey: " + dstRole.name)
              chan.basicPublish(exchange, dstRole.name, null, serialize(role, msg))
          }
        case rawMsg: Array[Byte] =>
          println("Proxy for " +role+ " received a message")
          val (srcRole, msg) = deserialize(rawMsg)
          self ! DeserializedMsgReceive(srcRole, msg)
        case NewSourceRole(role, actorChan) =>
          srcRoleChans += (role -> actorChan)
          println("In proxy for "+role+", expanded srcRoleChans: " + srcRoleChans)
        case DeserializedMsgReceive(role, msg) if srcRoleChans.isDefinedAt(role) =>
          println("sending " + msg + " to channel " + srcRoleChans(role))
          srcRoleChans(role) ! msg
        case Exit => 
          println("Proxy for role "+role+" exiting")
          closeAndExit(chan, consumerTag) 
      }

    def act = {
      proxyRegistryActor ! self
      loop {
        react(reactBody)
      }
    } 
  }

  def accept(role: Symbol)(act: ActorFun): Unit = {
    println("accept: " + role + ", awaiting: " + awaiting)
    checkRoleAwaiting(role)
    println("sending blocking message")
    val sessExchange = (matchMakerActor !? Accept(role)).asInstanceOf[String]
    println("got reply from matchmaker")

    val proxy = new AMQPActorProxy(sessExchange, role)
    proxy.start
    val sessChan = (awaiting foldLeft Map[Symbol, ParticipantChannel]()) { case (result, awaitedRole) =>
      if (role == awaitedRole) result // we don't support messages to self through session channel
      else {
        // binds the channel to the current actor (Actor.self). 
        // self is the only actor that can receive on the channel
        val chanFrom = new actors.Channel[Any]() 
        val chanTo = new actors.Channel[Any](proxy)
        proxy ! NewSourceRole(awaitedRole, chanFrom)
        proxy ! NewDestinationRole(awaitedRole, chanTo)
        result + (awaitedRole -> new ParticipantChannel(chanFrom, chanTo))
      }
    }
    act(sessChan)
  }

  def connectAndInitExchange(): Channel = {
    val chan = connect(factory)
    chan.exchangeDeclare(INIT_EXCHANGE, "direct")
    chan
  }
}
