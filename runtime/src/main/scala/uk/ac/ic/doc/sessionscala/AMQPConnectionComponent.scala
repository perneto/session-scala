package uk.ac.ic.doc.sessionscala

import com.rabbitmq.client.Channel
import AMQPUtils._


/**
 * Created by: omp08
 */

trait AMQPConnectionComponent {
  val brokerHost: String
  val port: Int
  val user: String
  val password: String
  private val factory = createFactory(brokerHost, port, user, password)

  def close(chan: Channel) {
    chan.getConnection.close()
  }

  def connect() = AMQPUtils.connect(factory)

  def close(chan: Channel, consumerTag: String) {
    chan.basicCancel(consumerTag)
    close(chan)
  }
    
}