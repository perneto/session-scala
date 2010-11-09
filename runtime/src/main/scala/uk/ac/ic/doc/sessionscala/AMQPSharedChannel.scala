package uk.ac.ic.doc.sessionscala

import AMQPUtils._
import com.rabbitmq.client._
import SharedChannel._
import scala.actors.{Channel => _, _}
import Actor._
import collection.mutable

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
      publishInvite(chan, sessName, role, host)
    }
    close(chan)
  }

  def publishInvite(chan: Channel, sessExchange: String, role: Symbol, host: String) {
    val msgBytes = (sessExchange + "," + role.name + "," + scribbleType).getBytes(CHARSET)
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
  case object Terminate
  case object ExitSignal

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
    
    val invites = mutable.Map.empty[Symbol, List[String]]
    val accepts = mutable.Map.empty[Symbol, List[OC]]

    def matchMsg[T1,T2](map: mutable.Map[Symbol, List[T1]], key: Symbol, value: T2, otherMap: mutable.Map[Symbol, List[T2]])(action: T1 => Unit) {
      map get key match {
        case Some(x :: xs) =>
          action(x)
          if (xs == Nil) map -= key 
          else map += (key -> xs) // key already there so this is actually an update, not an add
        case _ =>
          map -= key
          otherMap += (key -> (value :: otherMap.getOrElse(key, Nil)))
      }
    }

    loop {
      react {
        case i: Invite => 
          matchMsg(accepts, i.role, i.sessExchange, invites) { acceptSender =>
            acceptSender ! i.sessExchange
          }
        case Accept(acceptRole: Symbol) => 
          matchMsg(invites, acceptRole, sender, accepts) { sessExchange =>
            sender ! sessExchange
          }
        case Exit =>
          println("Matchmaker exiting")
          exit()
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
  val JAVA_OBJECT_CODE: Byte = -127
  val EXIT_SIGNAL_CODE: Byte = 127
  val BIG_ENOUGH = 8192
  import java.nio.ByteBuffer
  import java.io._
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
      case ExitSignal =>
        buf.put(EXIT_SIGNAL_CODE)
      case x => 
        println("Warning - using non-interoperable Java serialization for " + x)
        buf.put(JAVA_OBJECT_CODE)
        val arrayOs = new ByteArrayOutputStream
        val oos = new ObjectOutputStream(arrayOs)
        oos.writeObject(x)
        oos.close()
        buf.put(arrayOs.toByteArray())
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
    val value = typeCode match {
      case INT_CODE => buf.getInt() // big-endian
      case STRING_CODE =>
        val length = buf.getInt()
        val stringBytes = Array.ofDim[Byte](length)
        buf.get(stringBytes)
        new String(stringBytes, CHARSET)
      case EXIT_SIGNAL_CODE =>
        ExitSignal
      case JAVA_OBJECT_CODE =>
        println("Warning - decoding non-interoperable Java object")
        val bytes = Array.ofDim[Byte](buf.limit - buf.position)
        buf.get(bytes)
        val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
        ois.close()
        ois.readObject()
      case t => throw new IllegalArgumentException("Unsupported type code in deserialize: " + t)
    }
    val result = (role, value)
    println("deserialize: " + result)
    result
  }

  case class NewDestinationRole(role: Symbol, chan: actors.Channel[Any])
  case class NewSourceRole(role: Symbol, chan: actors.Channel[Any])
  case class DeserializedMsgReceive(fromRole: Symbol, body: Any)

  class AMQPActorProxy(exchange: String, role: Symbol) extends Actor {
    def publish(dstRole: Symbol, msg: Array[Byte]) {
      println("Proxy for "+role+" is sending message " + msg + " to exchange " + exchange + " with routingKey: " + dstRole.name)
      chan.basicPublish(exchange, dstRole.name, null, msg)
    }

    val chan = connect(factory)
    val roleQueue = exchange + role.name
    // self instead of this gives the actor for the thread creating this object
    // self is only valid in the act method
    val consumerTag = chan.basicConsume(roleQueue, true, new SendMsgConsumer(chan, this))
    // noAck = true, automatically sends acks todo: probably should be false here
    println("Proxy for role "+role+" is consuming messages on queue: "+roleQueue+"...")

    val srcRoleChans = mutable.Map[Symbol, actors.Channel[Any]]()
    val exitSignals = mutable.Set[Symbol]()

    var reactBody: PartialFunction[Any, Unit] = {
      case NewDestinationRole(dstRole, actorChan) =>
        reactBody = reactBody orElse {
          case actorChan ! msg => publish(dstRole, serialize(role, msg))
        }
      case rawMsg: Array[Byte] =>
        println("Proxy for " +role+ " received a message")
        val (srcRole, msg) = deserialize(rawMsg)
        msg match {
          case ExitSignal =>
            exitSignals += srcRole
            if (exitSignals == srcRoleChans.keySet) self ! Terminate
          case other => self ! DeserializedMsgReceive(srcRole, other)
        }
      case NewSourceRole(role, actorChan) =>
        srcRoleChans += (role -> actorChan)
        println("In proxy for "+role+", expanded srcRoleChans: " + srcRoleChans)
      case DeserializedMsgReceive(role, msg) if srcRoleChans.isDefinedAt(role) =>
        println("sending " + msg + " to channel " + srcRoleChans(role))
        srcRoleChans(role) ! msg
      case Exit =>
        srcRoleChans.keys foreach { remoteRole =>
          publish(remoteRole, serialize(role, ExitSignal))
        }
      case Terminate =>
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
