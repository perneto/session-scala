package uk.ac.ic.doc.sessionscala.messageformats

import java.util.Arrays
import java.nio.ByteBuffer
import java.io._

/**
 * Created by: omp08
 */

trait AMQPMessageFormats extends ShortMessageFormat with LongMessageFormat {

  val FORMAT = LongMessageFormat()

  def serializeInvite(sessExchange: String, role: Symbol, protocol: String): Array[Byte] = {
    FORMAT.serializeInvite(sessExchange, role, protocol)     
  }

  def openInvite(body: Array[Byte]): (Symbol, String, String) = {
    val invite = FORMAT.openInvite(body)
    println("received invite: " + invite)
    invite
  }

  def putLength(len: Int, buf: ByteBuffer) {
    if (len < 0 || len > 9999) throw new IllegalArgumentException
      ("Length was negative or over 9999: " + len)
    buf.put(len.toString.getBytes(CHARSET))
  }

  trait SessionMessageFormat {
    type Msg <: SessionMessage
    def serializeInvite(sessName: String, invitedRole: Symbol, proto: String): Array[Byte]
    def openInvite(bytes: Array[Byte]): (Symbol, String, String)
    val parser: SessionMessageParser[Msg]
    val extractor: SessionMessageExtractor[Msg]

  }
  trait SessionMessage {
    def serialize(buf: ByteBuffer): Unit
  }
  trait SessionMessageParser[S <: SessionMessage] {
    def unapply(bytes: Array[Byte]): Option[S]
  }
  trait SessionMessageExtractor[S <: SessionMessage] {
    def unapply(sessMsg: S): Option[(Symbol, Symbol, Any)]
    def apply(sessName: String, src: Symbol, dst: Symbol, label: Symbol, contents: Option[Any]): S
  }
  
  
  def serialize(sessName: String, srcRole: Symbol, dstRole: Symbol, label: Symbol, msg: Option[Any]): Array[Byte] = {
    println("serialize, msg: " + msg)
    val result = withBuf { buf =>
      val sessMsg = FORMAT.extractor.apply(sessName, srcRole, dstRole, label, msg)
      sessMsg.serialize(buf)
    }
    //println("serialize (" + srcRole + "," + msg + "): " + Arrays.toString(result))
    result
  }

  def deserialize(msg: Array[Byte]): (Symbol, Symbol, Option[Any]) = {
    val sessMsg = FORMAT.parser.unapply(msg).get
    val result = FORMAT.extractor.unapply(sessMsg).get
    //println("deserialize: " + result)
    result
  }

  val CHARSET = "ISO-8859-1"

  def withBuf(block: ByteBuffer => Unit): Array[Byte] = {
    val BIG_ENOUGH = 16384
    val buf: ByteBuffer = ByteBuffer.allocate(BIG_ENOUGH)
    block(buf)
    toByteArray(buf)
  }

  def wrap[T](array: Array[Byte])(block: ByteBuffer => T): T = {
    val buf = ByteBuffer.wrap(array)
    block(buf)    
  }

  def toByteArray(buf: ByteBuffer) = {
    val result = Array.ofDim[Byte](buf.position)
    buf.flip()
    buf.get(result)
    result
  }

  def javaSerializeObject(x: Any): Array[Byte] = {
    println("Warning - using non-interoperable Java serialization for " + x)
    val arrayOs = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(arrayOs)
    oos.writeObject(x)
    oos.close()
    arrayOs.toByteArray
  }

  def javaDeserializeObject(buf: ByteBuffer): AnyRef = {
    println("Warning - decoding non-interoperable Java object")
    val bytes = Array.ofDim[Byte](buf.limit - buf.position)
    buf.get(bytes)
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    ois.close()
    ois.readObject()
  }

}