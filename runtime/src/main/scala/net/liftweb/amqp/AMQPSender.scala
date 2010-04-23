/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package amqp {

import _root_.com.rabbitmq.client._
import _root_.java.io.ByteArrayOutputStream
import _root_.java.io.ObjectOutputStream
import actors.Actor, Actor.loop

/**
 * An actor with a long-lived connection to an AMQP exchange/queue.
 *
 * @see ExampleStringAMQPSender for an example use.
 * @author Steve Jenson (stevej@pobox.com)
 */
abstract class AMQPSender[T](cf: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String) extends Actor {
  val conn = cf.newConnection(host, port)
  val channel = conn.createChannel()

  /**
   * Override this to use your own AMQP queue/exchange with the given channel.
   */
  def configure(channel: Channel): AnyRef

  def send(msg: T) {
    // Now write an object to a byte array and shove it across the wire.
    val bytes = new ByteArrayOutputStream
    val store = new ObjectOutputStream(bytes)
    store.writeObject(msg)
    store.close
    channel.basicPublish(exchange, routingKey, null, bytes.toByteArray)
  }

  def act = loop { react {
    case AMQPMessage(msg: T) => send(msg)
  }}
}

/**
 * An example subclass of AMQPSender[T]
 *
 * An example of how to send messages to an AMQP queue/exchange. Notice that this
 * is setup with the same params as StringAQMPExample. After making a new instance of
 * StringAMQPExample, just send ExampleAMQPSender ! "hi" to see the message "hi"
 * appear in the output log. Fun and Easy!
 *
 * If you are planning to send lots of messages to lots of different exchange/queues,
 * consider creating Actor-based Senders, that will help your application to scale.
 */
class StringAMQPSender(cf: ConnectionFactory, host: String, port: Int, exchange: String, routingKey: String)
    extends AMQPSender[String](cf, host, port, exchange, routingKey){
  override def configure(channel: Channel) = {
    val conn = cf.newConnection(host, port)
    val channel = conn.createChannel()
    channel
  }
}

}
}