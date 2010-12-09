package uk.ac.ic.doc.sessionscala

import com.rabbitmq.client.ConnectionFactory

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
    val connection = factory.newConnection
    connection.createChannel
  }

  def connectDefaults() = {
    val factory = new ConnectionFactory
    factory.newConnection.createChannel
  }  
}
