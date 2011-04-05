package uk.ac.ic.doc.sessionscala

import actors.Actor
import com.rabbitmq.client.{AMQP, Envelope, DefaultConsumer, Channel}

/**
 * Created by: omp08
 */
class SendMsgConsumer(chan: Channel, dest: Actor) extends DefaultConsumer(chan) {
  override def handleDelivery(consumerTag: String, env: Envelope, prop: AMQP.BasicProperties, body: Array[Byte]) {
    println("Received for dst: "+dest+", body: " + body)
    dest ! body
  }
}

  