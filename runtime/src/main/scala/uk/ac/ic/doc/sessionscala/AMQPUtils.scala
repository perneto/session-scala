package uk.ac.ic.doc.sessionscala

import com.rabbitmq.client.{Channel, ConnectionFactory}

/**
 * Created by: omp08
 */

object AMQPUtils {
  
  def createFactory(brokerHost: String, port: Int, user: String, password: String): ConnectionFactory = {
    val factory = new ConnectionFactory
    factory.setHost(brokerHost)
    factory.setPort(port)
    factory.setUsername(user)
    factory.setPassword(password)
    factory
  }

  def connect(factory: ConnectionFactory) = {
    //println("Connecting to AMQP broker at: " + factory.getHost + ", port: " + factory.getPort + ", user: " + factory.getUsername)
    val connection = factory.newConnection
    connection.createChannel
  }

  def connectDefaults() = connect(new ConnectionFactory)
  
  def close(chan: Channel) {
    //println("closing: "+chan)
    chan.getConnection.close()
  }
  
  def withChan[T](fact: ConnectionFactory)(f: Channel => T) = {
    val chan = connect(fact)
    try {
      f(chan)
    } finally {
      close(chan)
    }
  }
    
}
