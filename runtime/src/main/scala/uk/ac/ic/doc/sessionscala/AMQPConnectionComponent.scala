package uk.ac.ic.doc.sessionscala

import AMQPUtils._
import actors.Actor._
import com.rabbitmq.client.AMQP.BasicProperties
import com.rabbitmq.client.{Envelope, DefaultConsumer, Consumer, Channel}
import java.lang.String
import actors.{TIMEOUT, Actor}

/**
 * Created by: omp08
 */

trait AMQPConnectionComponent {
  val brokerHost: String
  val port: Int
  val user: String
  val password: String
  private val factory = createFactory(brokerHost, port, user, password)

  // The only instance (except for the initSessionExchange method). It is managed
  // by the actor below.
  private val chan = connect()


  def close(chan: Channel) {
    //println("Closing: " + chan)
    chan.getConnection.close()
  }

  def connect() = AMQPUtils.connect(factory)

  val connectionManagerActor = actor {
    var proxies = Set[Actor]()

    loop {
      react {
        case ('publish, exchange: String, routingKey: String, msg: Array[Byte]) =>
          chan.basicPublish(exchange, routingKey, null, msg)
        case ('exchangeDeclare, name: String) =>
          chan.exchangeDeclare(name, "direct")
        case ('queueDeclare, name: String) =>
          //Parameters to queueDeclare: (queue, durable, exclusive, autoDelete, arguments)
          chan.queueDeclare(name, false, false, false, null)
        case ('queueBind, queue: String, exchange: String, routingKey: String) =>
          chan.queueBind(queue, exchange, routingKey)
        case ('consume, queue: String, dest: Actor) =>
          chan.basicConsume(queue, true,
            new SendMsgConsumer(chan, dest)
          //  new DefaultConsumer(chan) {
          //  override def handleDelivery(consumerTag: String, envelope: Envelope,
          //                              properties: BasicProperties, body: Array[Byte]) = dest ! body
          //}
          )

        case ('start, proxy: Actor) => proxies += proxy
        case ('stop, proxy: Actor) => proxies -= proxy
        case Quit if proxies.isEmpty =>
          close(chan)
          //println("@@@@@@@@@@@@@@@ Connection manager actor exiting, connection has been closed")
          exit()
      }
    }
  }
}