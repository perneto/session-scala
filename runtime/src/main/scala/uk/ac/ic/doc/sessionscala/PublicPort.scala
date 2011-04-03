package uk.ac.ic.doc.sessionscala

import actors.Actor.actor
import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import java.io.{File, ByteArrayInputStream}
import java.util.UUID

case class Invite(replyPort: PublicPort, protocol: String, role: Symbol)
case class AcceptedInvite(role: Symbol, replyPort: PrivatePort, port: PrivatePort)

trait PrivatePort {
  def send(msg: Any)
}

//case class -> [L,R](left: L, right: R)

object PublicPort {
  def newLocalPort(protocol: String, role: Symbol): PublicPort =
    new PublicPortSameVM(protocol, role)

  def AMQPPort(protocol: String, role: Symbol, queueAddr: String = "",
               user: String = "guest", password: String = "guest"): PublicPort = {

    def splitAddr(queueAddr: String) = {
      def checkLength2(array: Array[String]) {
        if (array.length > 2) throw new IllegalArgumentException("Bad queue address: " + queueAddr)
      }

      val array = 
        if (queueAddr == "") Array(role.name)
        else queueAddr.split(Array('@'))
      checkLength2(array)
      val queue = array(0)
      val (hostname, port) =
        if (array.length == 1) ("localhost", 5672)
        else {
          val array2 = array(1).split(':')
          checkLength2(array2)
          (array2(0), if (array2.length == 1) 5672 else array2(1).toInt)
        }
      (queue, hostname, port)
    }

    val (name, brokerHost, brokerPort) = splitAddr(queueAddr)
    AMQPPublicPort(protocol, role, name, brokerHost, brokerPort, user, password)
  }

  /**
   * Sends out invites for a new session to all session ports,
   * for their respective assigned roles.
   * Blocks until all session ports have replied with the definitive
   * location of the process that accepted the invitation
   * (can be different because of forwarding).
   * Finally, sends out the final locations of all roles
   * to each private port obtained in the replies.
   */
  def startSession(ports: PublicPort*) {
    checkPorts(ports)
    val sessID = UUID.randomUUID().toString
    val sessIDPort = ports(0).derived(sessID)
    //println("sending invites")
    ports foreach { p =>
      p.send(Invite(sessIDPort, p.protocol, p.role))
    }
    //println("Finished sending invites")
    val replies = for (i <- 1 to ports.length)
      yield sessIDPort.receive().asInstanceOf[AcceptedInvite]
    val mapping = finalLocationsMapping(replies)
    val privPorts = mapping.values
    for (rp <- replies map (_.replyPort)) {
      //println("send mapping to: "+rp)
      rp.send(mapping)
    }
    sessIDPort.close()
  }

  def finalLocationsMapping(replies: Seq[AcceptedInvite]): Map[Symbol, PrivatePort] = {
    (replies foldLeft Map[Symbol, PrivatePort]()) { case (result, AcceptedInvite(role, _, pp)) =>
      result + (role -> pp)
    }
  }

  def checkPorts(ports: Seq[PublicPort]) {
    assert(ports.length > 0)
    var first = ports(0)
    for (p <- ports) assert(first.getClass == p.getClass)
  }

  def forwardInvite(map: (PublicPort, PublicPort)*) {
    map foreach { case (from, to) =>
      actor { to.send(from.receive()) }
    }
  }
}

trait PublicPort {
  val protocol: String
  val role: Symbol
  def derived(name: String): PublicPort

  def send(msg: Any)

  def receive(): Any

  def close() {}
  
  /** Accept to play a given role. Waits for an invite, creates
   *  a local session channel, and sends back a confirmation message
   *  with the name of the session channel before proceeding.
   */
  def bind[T](act: SessionChannel => T): T

  def protocolsCompatible(proto1: String, proto2: String): Boolean = {
    // TODO later: check two Scribble protocols are compatible (for invite receive)
    true
  }

  def retrieveRolesSet: Set[Symbol] = {
    val scribbleParser = new ANTLRProtocolParser
    val errorsJournal = new ExceptionsJournal
    val model = scribbleParser.parse( // Scribble doesn't use any chars outside ascii, so any charset is fine
      new ByteArrayInputStream(protocol.getBytes), errorsJournal, null)
    if (errorsJournal.hasError) throw new IllegalArgumentException(
      "Could not parse Scribble protocol: " + protocol)
    ScribbleUtils.roleSymbols(model)
  }

  def loadProtocolFile: String = {
    val source =
      if (new File(protocol).canRead) io.Source.fromFile(protocol)
      else if (protocol == "") null
      else try {
        io.Source.fromURL(protocol)
      } catch {
        case _ => null
      }
    if (source != null) source.foldLeft("")(_ + _)
    else if (protocol == "") "<no protocol given>"
    else protocol
  }

  val protocolRoles: Set[Symbol] = retrieveRolesSet
  if (protocolRoles.size == 0) throw new IllegalArgumentException
    ("The protocol should have at least one role")
  if (!protocolRoles.contains(role)) throw new IllegalArgumentException(
     "Role:" + role + " not defined on channel, awaiting: " + protocolRoles)

  val scribbleType: String = loadProtocolFile
}
