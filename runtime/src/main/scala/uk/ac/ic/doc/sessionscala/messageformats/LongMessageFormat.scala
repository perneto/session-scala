package uk.ac.ic.doc.sessionscala.messageformats

import java.nio.ByteBuffer

/**
 * Created by: omp08
 */

trait LongMessageFormat {
  self: AMQPMessageFormats =>

  object LongMessageFormat {
    def apply() = new SessionMessageFormat {
      type Msg = SessionMsgLong
      val parser = FromBytes
      val extractor = SessionMsgLongGeneric

      def serializeInvite(sessName: String, invitedRole: Symbol, proto: String) = {
        withBuf { buf =>
          SessionMsgLong(sessName, Symbol("Adm"), invitedRole, Symbol("inv"), Some(proto)).serialize(buf)
        }
      }

      def openInvite(bytes: Array[Byte]): (Symbol, String, String) = {
        val FromBytes(SessionMsgLong(sessName, _, dstRole, label, contents)) = bytes
        (dstRole, sessName, contents.get.asInstanceOf[String])
      }
    }
  }

  object SessionMsgLongGeneric extends SessionMessageExtractor[SessionMsgLong] {
    def unapply(sessMsg: SessionMsgLong) = {
      val SessionMsgLong(_, srcRole, _, label, contents) = sessMsg
      Some((srcRole, label, contents))
    }
    def apply(sessName: String, src: Symbol, dst: Symbol, label: Symbol, contents: Option[Any]): SessionMsgLong =
      SessionMsgLong(sessName,src,dst,label, contents)
  }
  case class SessionMsgLong(sessName: String, srcRole: Symbol,
                            dstRole: Symbol, label: Symbol,
                            contents: Option[Any]) extends SessionMessage {
    def serialize(buf: ByteBuffer) {
      var str = ""
      str += serializeLong(sessName) // sessionid/session name/session exchange name
      str += serializeLong(srcRole)
      str += serializeLong(dstRole)
      str += serializeLong(label)
      str += serializeLong(contents)
      str += serializeLong("0000") // option field, this is the shortest it can be
      buf.put(str.getBytes(CHARSET))
    }
  }

  def serializeLong(msg: Option[Any]): String = serializeLong(msg.getOrElse(""))

  def serializeLong(msg: Any): String = msg match {
    case s: Symbol => serializeLong(s.name)
    case x =>
      val s = x.toString
      if (s.length > 9999) throw new IllegalArgumentException("string too long for serialize (limit 9999): " + s)
      var len = s.length.toString
      while (len.length < 4) len = "0" + len
      val res = len + s
      //println("serializeLong: " + res)
      res
  }

  def deserializeLong(buf: ByteBuffer): Any = {
    val str = readStr(buf)
    try {
      Integer.parseInt(str)
    } catch {
      case _ => str
    }
  }

  def deserializeOption(buf: ByteBuffer): Option[Any] = deserializeLong(buf) match {
    case "" => None
    case x => Some(x)
  }

  def readLength(buf: ByteBuffer): Int = {
    val bytes = Array.ofDim[Byte](4)
    buf.get(bytes)
    Integer.parseInt(new String(bytes, CHARSET))
  }

  def readStr(buf: ByteBuffer): String = {
    val len = readLength(buf)
    val bytes = Array.ofDim[Byte](len)
    buf.get(bytes)
    new String(bytes, CHARSET)
  }

  object FromBytes extends SessionMessageParser[SessionMsgLong] {
    def unapply(x: Array[Byte]): Option[SessionMsgLong] = wrap(x) { buf =>
      val res = Some(SessionMsgLong(
        readStr(buf),
        Symbol(readStr(buf)),
        Symbol(readStr(buf)),
        Symbol(readStr(buf)),
        deserializeOption(buf)))
      println("FromBytes, result: " + res)
      res
    }
  }  
}