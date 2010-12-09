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
  val factory = createFactory(brokerHost, port, user, password)

  def close(chan: Channel) {
    chan.getConnection.close()
  }
  
  def close(chan: Channel, consumerTag: String) {
    chan.basicCancel(consumerTag)
    close(chan)
  }

  def connectAndInitExchange(): Channel = {
    val chan = connect(factory)
    //chan.exchangeDeclare(INIT_EXCHANGE, "direct")  no need to declare amq.direct, spec says it's always there by default
    chan
  }    
}