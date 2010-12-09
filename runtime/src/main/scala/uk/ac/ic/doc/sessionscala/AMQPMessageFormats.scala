package uk.ac.ic.doc.sessionscala

import java.util.Arrays
import java.nio.ByteBuffer
import java.io._

/**
 * Created by: omp08
 */

trait AMQPMessageFormats {  
  private val CHARSET = "ISO-8859-1"

  private val INVITE_SEPARATOR = "$"

  def serializeInvite(sessExchange: String, role: Symbol, protocol: String): Array[Byte] = {
    (sessExchange + INVITE_SEPARATOR
            + role.name + INVITE_SEPARATOR + protocol).getBytes(CHARSET)
  }

  def openInvite(body: Array[Byte]): (Symbol, String, String) = {
    val msg = new String(body, CHARSET)
    println(msg)
    val Array(exchange, role, protocol) = msg.split("\\" + INVITE_SEPARATOR)
    // todo: check protocol is compatible with the local protocol
    println("received for session: exchange: " + exchange + ", role: " + role + ", protocol: " + protocol)
    (Symbol(role), exchange, protocol)
  }



// todo: proper serialization
  val INT_CODE: Byte = 0
  val STRING_CODE: Byte = 1
  val TRUE_CODE: Byte = 2
  val FALSE_CODE: Byte = 3
  val LABELLED_CODE: Byte = 4
  val JAVA_OBJECT_CODE: Byte = -127
  val BIG_ENOUGH = 8192
  def serialize(msg: Any, buf: ByteBuffer): Unit = msg match {
    case s: String =>
      buf.put(STRING_CODE)
      buf.putInt(s.length)
      buf.put(s.getBytes(CHARSET))
    case i: Int =>
      buf.put(INT_CODE)
      buf.putInt(i)
    case true =>
      buf.put(TRUE_CODE)
    case false =>
      buf.put(FALSE_CODE)
    case x if hasUnapply(x) =>
      buf.put(LABELLED_CODE)
      serialize(typeName(x), buf)
      // todo
    case x if hasUnapplySeq(x) =>
      // todo
    case x =>
      println("Warning - using non-interoperable Java serialization for " + x)
      buf.put(JAVA_OBJECT_CODE)
      val arrayOs = new ByteArrayOutputStream
      val oos = new ObjectOutputStream(arrayOs)
      oos.writeObject(x)
      oos.close()
      buf.put(arrayOs.toByteArray())
  }

  def serialize(srcRole: Symbol, msg: Any): Array[Byte] = {
    println("serialize, msg: " + msg)
    val buf = ByteBuffer.allocate(BIG_ENOUGH)
    val srcBytes = srcRole.name.getBytes(CHARSET)
    assert(srcBytes.length < 256)
    buf.put(srcBytes.length.asInstanceOf[Byte])
    buf.put(srcBytes)

    serialize(msg, buf)
    val result = Array.ofDim[Byte](buf.position)
    buf.flip()
    buf.get(result)
    println("serialize (" + srcRole + "," + msg + "): " + Arrays.toString(result))
    result
  }

  def typeName(x: Any): String = "TODO"
  def hasUnapply(x: Any): Boolean = false
  def hasUnapplySeq(x: Any): Boolean = false

  def deserialize(msg: Array[Byte]): (Symbol, Any) = {
    val buf = ByteBuffer.wrap(msg)
    val length = buf.get()
    val roleBytes = Array.ofDim[Byte](length)
    buf.get(roleBytes)
    val role = Symbol(new String(roleBytes, CHARSET))
    val typeCode = buf.get()
    val value = typeCode match {
      case INT_CODE => buf.getInt() // big-endian
      case STRING_CODE =>
        val length = buf.getInt()
        val stringBytes = Array.ofDim[Byte](length)
        buf.get(stringBytes)
        new String(stringBytes, CHARSET)
      case TRUE_CODE => true
      case FALSE_CODE => false
      case JAVA_OBJECT_CODE =>
        println("Warning - decoding non-interoperable Java object")
        val bytes = Array.ofDim[Byte](buf.limit - buf.position)
        buf.get(bytes)
        val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
        ois.close()
        ois.readObject()
      case t => throw new IllegalArgumentException("Unsupported type code in deserialize: " + t)
    }
    val result = (role, value)
    println("deserialize: " + result)
    result
  }

}