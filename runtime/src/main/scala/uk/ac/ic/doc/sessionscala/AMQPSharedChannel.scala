package uk.ac.ic.doc.sessionscala

import AMQPUtils._
import com.rabbitmq.client.Channel

class AMQPSharedChannel(awaiting: Set[Symbol], brokerHost: String, port: Int, user: String, password: String) extends SharedChannel {
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
  }

  def contains[K,V](seq: Seq[(K,V)], k: K): Boolean = {
    for ((key, value) <- seq) {
      if (k == key) return true
    }
    false
  }

  def checkMapping(mapping: Seq[(Symbol,String)]) {
    val missing = awaiting flatMap { role =>
      if (!contains(mapping, role)) List(role)
      else Nil
    }
    if (!missing.isEmpty)
      throw new IllegalArgumentException("Missing roles in invite: " + missing)
  }

  def accept(role: Symbol)(act: ActorFun): Unit = {
    val chan = initChanAndExchange()
    // parameters: queue name, noAck
    val response = chan.basicGet(SharedChannel.localhost, true)
    val msg = new String(response.getBody, CHARSET)
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

    act(Map())
  }

  def initChanAndExchange(): Channel = {
    val chan = connect(factory)
    chan.exchangeDeclare(INIT_EXCHANGE, "direct")
    chan
  }
}
