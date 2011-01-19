package uk.ac.ic.doc.sessionscala.messageformats

import java.nio.ByteBuffer

/**
 * Created by: omp08
 */

trait ShortMessageFormat {
  self: AMQPMessageFormats =>

  object ShortMessageParser {
    def apply() = new SessionMessageFormat {
      type Msg = SessionMsgShort
      val parser = ShortFromBytes
      val extractor = SessionMsgShortGeneric

      def serializeInvite(sessName: String, invitedRole: Symbol, proto: String) = {
        (sessName + INVITE_SEPARATOR
                        + invitedRole.name + INVITE_SEPARATOR + proto).getBytes(CHARSET)        
      }

      private val INVITE_SEPARATOR = "$"
      
      def openInvite(bytes: Array[Byte]): (Symbol, String, String) = {
        val msg = new String(bytes, CHARSET)
        val Array(exchange, role, protocol) = msg.split("\\" + INVITE_SEPARATOR)
        (Symbol(role), exchange, protocol)
      }
    }
  }

  object ShortFromBytes extends SessionMessageParser[SessionMsgShort] {
    def unapply(bytes: Array[Byte]) = wrap(bytes) { buf =>
      val srcLen = buf.get
      val src = readSymbol(srcLen, buf)
      val labelLen = buf.getShort
      val label = readSymbol(labelLen, buf)
      val contents = deserializeShort(buf)
      Some(SessionMsgShort(src, label, contents))
    }

    def readSymbol(len: Int, buf: ByteBuffer): Symbol = {
      val bytes = Array.ofDim[Byte](len)
      buf.get(bytes)
      Symbol(new String(bytes, CHARSET))
    }
  }

  object SessionMsgShortGeneric extends SessionMessageExtractor[SessionMsgShort] {
    def unapply(msg: SessionMsgShort) = {
      val SessionMsgShort(srcRole, label, contents) = msg
      Some((srcRole, label, contents))
    }

    def apply(sessName: String, src: Symbol, dst: Symbol, label: Symbol, contents: Option[Any]) = {
      SessionMsgShort(src, label, contents)
    }
  }

  case class SessionMsgShort(srcRole: Symbol, label: Symbol, contents: Option[Any]) extends SessionMessage {
    def serialize(buf: ByteBuffer) {
      val srcBytes = srcRole.name.getBytes(CHARSET)
      assert(srcBytes.length < 256)
      buf.put(srcBytes.length.asInstanceOf[Byte])
      buf.put(srcBytes)
      val name = label.name
      buf.putShort(name.length.asInstanceOf[Short])
      buf.put(name.getBytes(CHARSET))

      serializeShort(contents, buf)
    }
  }

  // todo: proper serialization
  val INT_CODE: Byte = 0
  val STRING_CODE: Byte = 1
  val TRUE_CODE: Byte = 2
  val FALSE_CODE: Byte = 3
  val LABEL_CODE: Byte = 4
  val TUPLE_CODE: Byte = 5
  val JAVA_OBJECT_CODE: Byte = -127
  val NO_CONTENTS_CODE: Byte = 127

  def serializeShort(opt: Option[Any], buf: ByteBuffer): Unit = opt match {
    case Some(msg) => msg match {
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
      case x: Product if x.productArity > 0 =>
        buf.put(TUPLE_CODE)
        val len = x.productArity.asInstanceOf[Byte]
        buf.put(len)
        x.productIterator foreach (x => serializeShort(Some(x), buf))
      case x =>
        buf.put(JAVA_OBJECT_CODE)
        val bytes = javaSerializeObject(x)
        buf.put(bytes)
      }
    case None => buf.put(NO_CONTENTS_CODE)
  }

  def deserializeShort(buf: ByteBuffer): Option[Any] = {
    val typeCode = buf.get()
    val value = if (typeCode == NO_CONTENTS_CODE) None
    else Some( typeCode match {
      case INT_CODE => buf.getInt() // big-endian
      case STRING_CODE =>
        val length = buf.getInt()
        val stringBytes = Array.ofDim[Byte](length)
        buf.get(stringBytes)
        new String(stringBytes, CHARSET)
      case TRUE_CODE => true
      case FALSE_CODE => false
      case JAVA_OBJECT_CODE =>
        javaDeserializeObject(buf)
      case t => throw new IllegalArgumentException("Unsupported type code in deserialize: " + t)
    } )
    println("deserialize: " + value)
    value
  }

}