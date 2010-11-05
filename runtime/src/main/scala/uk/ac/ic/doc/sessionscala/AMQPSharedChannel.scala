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

  // lazy so it's only started when accept is called for the first time
  // todo: this avoids race conditions with invite, but still brittle, should be improved
  lazy val relayActor = actor {
    val chan = initChanAndExchange()
    chan.queueDeclare(localhost, false, false, false, null)
    val consumer = new QueueingConsumer(chan)
    val consumerTag = chan.basicConsume(localhost, true, consumer)
    loop {
      receive {
      case (role: Symbol, actor: Actor) =>
        val delivery = consumer.nextDelivery() // blocks waiting for message
        declareSessionQueues(chan, delivery)

        reply(Map())
      }
    }
  }

  def declareSessionQueues(chan: Channel, delivery: QueueingConsumer.Delivery) {
    val msg = new String(delivery.getBody, CHARSET)
    val parts = msg.split(",")
    val exchange = parts(0)
    val role = parts(1)
    val protocol = parts(2)
    println("exchange: " + exchange + ", role: " + role + ", protocol: " + protocol)

    chan.exchangeDeclare(exchange, "direct")
    val roleQueue = exchange + role
    chan.queueDeclare(roleQueue, false, false, false, null)
    chan.queueBind(roleQueue, exchange, role)

    println("created session queue/exchange")
  }

  def accept(role: Symbol)(act: ActorFun): Unit = {
    println("amqp, accept: " + role + ", awaiting: " + awaiting)
    checkRoleAwaiting(role)
    val sessChan = (relayActor !? ((role, Actor.self))).asInstanceOf[Symbol => ParticipantChannel]
    act(sessChan)
  }

  def initChanAndExchange(): Channel = {
    val chan = connect(factory)
    chan.exchangeDeclare(INIT_EXCHANGE, "direct")
    chan
  }
}
