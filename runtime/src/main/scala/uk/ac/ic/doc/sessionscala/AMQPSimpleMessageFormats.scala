package uk.ac.ic.doc.sessionscala

import java.nio.ByteBuffer
import java.io.{ByteArrayInputStream, ObjectInputStream, ObjectOutputStream, ByteArrayOutputStream}

/**
 * Created by: omp08
 */

trait AMQPSimpleMessageFormats {
  def serialize(msg: Any): Array[Byte] = {
    //println("serialize: "+msg)
    msg match {
      case Invite(AMQPPublicPort(_, _, queueName, brokerHost, port, user, pwd), proto, role) =>
        ("INVITE%"+proto+"%"+role.name+"%"+queueName+"%"+brokerHost+"%"+port+"%"+user+"%"+pwd).getBytes
      case _ => javaSerializeObject(msg)
    }
  }
  
  def deserialize(bytes: Array[Byte]): Any = {
    val str = new String(bytes)
    if (str.startsWith("INVITE%")) {
      //println("Deserializing INVITE, len: "+str.length)
      var start=6 ; var i=6
      def next() = { 
        i += 1 ; start=i
        while (i < str.length && str(i) != '%') i+=1
        str.substring(start, i) 
      }
      val proto = next()
      val role = Symbol(next())
      val queueName = next()
      val brokerHost = next()
      val port = next().toInt
      val user = next()
      val pwd = next()
      Invite(AMQPPublicPort(proto, role, queueName, brokerHost, port, user, pwd), proto, role)
      
    } else javaDeserializeObject(bytes)
  }
  
  def javaSerializeObject(x: Any): Array[Byte] = {
    //println("Warning - using non-interoperable Java serialization for " + x)
    val arrayOs = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(arrayOs)
    oos.writeObject(x)
    oos.close()
    arrayOs.toByteArray
  }

  def javaDeserializeObject(bytes: Array[Byte]): AnyRef = {
    //println("Warning - decoding non-interoperable Java object")
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val o = ois.readObject()
    ois.close()
    //println("Java deserialize result:" + o)
    o
  }
}