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
          val invitedRole = declareSessionQueues(chan, body)
          matchMakerActor ! Left(invitedRole)
          println("sent invitation for " + invitedRole + " to matchmaker")
        case Exit => 
          chan.basicCancel(consumerTag); close(chan); 
          println("invitation receiver exiting..."); exit()
      }
    }
  }

  val matchMakerActor = actor { 
    println("started matchmaker")
    def tryToMatch(invites: Set[Symbol], accepts: Set[(Symbol,OC)]) {
      println("trying to match invites: " + invites + " with accepts: " + accepts)
      accepts foreach { case (role, replyTo) =>
        if (invites.contains(role)) replyTo ! ()
      }
    }
    def loop(invites: Set[Symbol], accepts: Set[(Symbol,OC)]) {
      tryToMatch(invites, accepts)
      react {
        case Left(invitedRole: Symbol) => 
          loop(invites + invitedRole, accepts)
        case Right(acceptRole: Symbol) => 
          loop(invites, accepts + ((acceptRole, sender)))
        case Exit => println("matchmaker exiting...")
      }
    }
    loop(Set(), Set())
  }

  def declareSessionQueues(chan: Channel, body: Array[Byte]): Symbol = {
    val msg = new String(body, CHARSET)
    val parts = msg.split(",")
    val exchange = parts(0)
    val role = parts(1)
    val protocol = parts(2)
    println("creating: exchange: " + exchange + ", role: " + role + ", protocol: " + protocol)

    chan.exchangeDeclare(exchange, "direct")
    val roleQueue = exchange + role
    chan.queueDeclare(roleQueue, false, false, false, null)
    chan.queueBind(roleQueue, exchange, role)

    Symbol(role)
  }

  def accept(role: Symbol)(act: ActorFun): Unit = {
    println("amqp, accept: " + role + ", awaiting: " + awaiting)
    checkRoleAwaiting(role)
    println("sending blocking message")
    matchMakerActor !? Right(role)
    println("got reply from matchmaker")
    val sessChan = Map[Symbol, ParticipantChannel]()
    act(sessChan)
  }

  def initChanAndExchange(): Channel = {
    val chan = connect(factory)
    chan.exchangeDeclare(INIT_EXCHANGE, "direct")
    chan
  }
}
