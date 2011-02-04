package uk.ac.ic.doc.sessionscala

import java.io.ByteArrayInputStream
import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import scalaj.collection.Imports._

object SharedChannel {
  def createLocalChannel(awaiting: Set[Symbol]): SharedChannel = {
    if (awaiting.isEmpty) throw new IllegalArgumentException("At least one role is required")
    new SharedChannelSameVM(awaiting)
  }
  private def createAMQPChannel(awaiting: Set[Symbol], brokerHost: String = "localhost",
                        port: Int = 5672, user: String = "guest", password: String = "guest"): SharedChannel =
    new AMQPSharedChannel(awaiting, brokerHost, port, user, password)

  def localhost: String = {
    val name = java.net.InetAddress.getLocalHost.getCanonicalHostName
    println("localhost: " + name)
    name
  }
  def withLocalChannel[T](protocol: String)(block: SharedChannel => T): T = {
    val awaiting = retrieveRolesSet(protocol)
    val shared = createLocalChannel(awaiting)
    try { block(shared) } finally { shared.close() }
  }
  def withAMQPChannel[T](protocol: String, brokerHost: String = "localhost",
                         port: Int = 5672, user: String = "guest", 
                         password: String = "guest")(block: SharedChannel => T): T = {
    val awaiting = retrieveRolesSet(protocol)
    val shared = createAMQPChannel(awaiting, brokerHost, port, user, password)
    try { block(shared) } finally { shared.close() }
  }

  private def retrieveRolesSet(protocol: String): Set[Symbol] = {
    val scribbleParser = new ANTLRProtocolParser
    val errorsJournal = new ExceptionsJournal
    val model = scribbleParser.parse( // todo: find out what charset Scribble uses
      new ByteArrayInputStream(protocol.getBytes), errorsJournal, null)
    if (errorsJournal.hasError) throw new IllegalArgumentException(
      "Could not parse Scribble protocol: " + protocol)
    Set(model.getRoles.asScala.map(r => Symbol(r.getName)): _*)
  }
}

abstract class SharedChannel(val awaiting: Set[Symbol]) {
  type ActorFun = (Symbol => ParticipantChannel) => Unit
  /** Blocks until all awaited roles have joined. */
  def join(role: Symbol)(act: ActorFun): Unit

  /** Sends out invites. Needs to give a mapping for each awaited role.
      @param mapping Maps role names to host names or IP addresses. */
  def invite(protocolFile: String, mapping: (Symbol,String)*): Unit

  def forwardInvite(mapping: (Symbol,String)*): Unit
  /** Accept to play a given role. Waits for an invite before proceeding. */
  def accept(role: Symbol)(act: ActorFun): Unit

  def close() {}

  def checkRoleAwaiting(role: Symbol) {
    if (!awaiting.contains(role)) throw new IllegalArgumentException
            ("Role:" + role + " not defined on channel, awaiting: " + awaiting)
  }
}


/*
object SessionAcceptorRegistry {
  def register(role: String, act,  channel: SharedChannel) {
    actor {
      loop {
        channel.join(role, act) // blocks until all participants have called openSession
      }
    }
  }
}
*/
