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

    val chan = initChanAndExchange()
    mapping foreach { case (role, host) =>
      //Parameters to queueDeclare: (queue, durable, exclusive, autoDelete, arguments)
      chan.queueDeclare(host, false, false, false, null)
      //Parameters to queueBind: (queue = host, exchange = INIT_EXCHANGE, routingKey = host)
      chan.queueBind(host, INIT_EXCHANGE, host)

      val msgBytes = ("s1," + role.name + "," + scribbleType).getBytes(CHARSET)
      chan.basicPublish(INIT_EXCHANGE, host, null, msgBytes)
    }
    close(chan)
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
  }

  case object Exit

  class SendMsgConsumer(chan: Channel) extends DefaultConsumer(chan) {
    override def handleDelivery(consumerTag: String, env: Envelope, prop: AMQP.BasicProperties, body: Array[Byte]) {
      invitationReceiverActor ! body
    }
  }

  val invitationReceiverActor = actor { 
    println("Starting invitation receiver actor...")
    val chan = initChanAndExchange()
    chan.queueDeclare(localhost, false, false, false, null)
    // noAck = true, automatically sends acks
    val consumerTag = chan.basicConsume(localhost, true, new SendMsgConsumer(chan))
    println("consuming...")

    loop {
      react {
        case body: Array[Byte] =>
          val (invitedRole,sessExchange) = declareSessionQueues(chan, body)
          matchMakerActor ! Invite(invitedRole, sessExchange)
          println("sent invitation for " + invitedRole + " to matchmaker")
        case Exit => 
          chan.basicCancel(consumerTag); close(chan); 
          println("invitation receiver exiting..."); exit()
      }
    }
  }

  case class Invite(role: Symbol, sessExchange: String) {
    def replyIfRoleMatches(acceptRole: Symbol, replyTo: OC): Boolean = {
      val matches = role == acceptRole
      if (matches) replyTo ! sessExchange
      matches
    }
  }

  case class Accept(role: Symbol)

  val matchMakerActor = actor { 
    println("started matchmaker")
    def matchAndLoop(invites: List[Invite], accepts: List[(Symbol,OC)]) {
      println("trying to match invites: " + invites + " with accepts: " + accepts)
      import java.util.LinkedList
      import scalaj.collection.Imports._
      val mAccepts = new LinkedList(accepts asJava)
      val mInvites = new LinkedList(invites asJava)
      accepts foreach { case pair@(role, replyTo) =>
        val it = mInvites.iterator
        var looping = true
        while (it.hasNext && looping) {
          val i = it.next
          if (i.replyIfRoleMatches(role, replyTo)) {
            mAccepts.remove(pair)
            it.remove()
            looping = false
          }
        }
      }
      loop(List(mInvites.asScala: _*), List(mAccepts.asScala: _*))
    }
    def loop(invites: List[Invite], accepts: List[(Symbol,OC)]) {
      react {
        case i: Invite => 
          matchAndLoop(i :: invites, accepts)
        case Accept(acceptRole: Symbol) => 
          matchAndLoop(invites, (acceptRole, sender) :: accepts)
        case Exit => println("matchmaker exiting...")
      }
    }
    loop(List(), List())
  }

  def declareSessionQueues(chan: Channel, body: Array[Byte]): (Symbol,String) = {
    val msg = new String(body, CHARSET)
    val Array(exchange, role, protocol) = msg.split(",")
    println("creating: exchange: " + exchange + ", role: " + role + ", protocol: " + protocol)

    chan.exchangeDeclare(exchange, "direct")
    val roleQueue = exchange + role
    chan.queueDeclare(roleQueue, false, false, false, null)
    chan.queueBind(roleQueue, exchange, role)

    (Symbol(role), exchange)
  }

  class AMQPActorProxy(exchange: String, role: Symbol) extends Actor { def act = {} }

  def addMap(map: Map[Symbol, ParticipantChannel], role: Symbol, actor: Actor) = 
    map + (role -> new ParticipantChannel(actor, actor))

  def accept(role: Symbol)(act: ActorFun): Unit = {
    println("amqp, accept: " + role + ", awaiting: " + awaiting)
    checkRoleAwaiting(role)
    println("sending blocking message")
    val sessExchange = (matchMakerActor !? Accept(role)).asInstanceOf[String]
    println("got reply from matchmaker")
    val sessChan = (awaiting foldLeft Map[Symbol, ParticipantChannel]()) { case (result, awaitedRole) =>
      println("awaitedRole: " + awaitedRole)
      if (role == awaitedRole) addMap(result, awaitedRole, self)
      else {
        val proxy = new AMQPActorProxy(sessExchange, role)
        addMap(result, awaitedRole, proxy)
      }
    }
    println(sessChan)
    act(sessChan)
  }

  def initChanAndExchange(): Channel = {
    val chan = connect(factory)
    chan.exchangeDeclare(INIT_EXCHANGE, "direct")
    chan
  }
}
