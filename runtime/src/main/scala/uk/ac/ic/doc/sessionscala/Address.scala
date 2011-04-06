package uk.ac.ic.doc.sessionscala

import actors.Actor.actor
import org.scribble.protocol.parser.antlr.ANTLRProtocolParser
import java.io.{File, ByteArrayInputStream}
import java.util.UUID

object Address {
  def newLocalAddress(protocol: String, role: Symbol): Address =
    new LocalAddressImpl(protocol, role)

  def AMQPAddress(protocol: String, role: Symbol, queueAddr: String = "",
               user: String = "guest", password: String = "guest"): Address = {

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
    AMQPAddressImpl(protocol, role, name, brokerHost, brokerPort, user, password)
  }
  
  case class ->(role: Symbol, contents: Any)

  /**
   * Sends out invites for a new session to all addresses,
   * for their respective assigned roles.
   * Blocks until all invitees have replied with the definitive
   * location of the process that accepted the invitation
   * (can be different because of forwarding).
   * Finally, sends out the final locations of all roles
   * to each private port obtained in the replies.
   */
  def startSession(addresses: Address*) {
    startSessionImpl(_.receive(), addresses)
  }
  def startSessionWithin(msec: Int, addresses: Address*) {
    startSessionImpl(_.receiveWithin(msec), addresses)  
  }
  
  def startSessionImpl(receive: Address => Any, addresses: Seq[Address]) {
    checkAddr(addresses)
    val sessID = UUID.randomUUID().toString
    val sessIDPort = addresses(0).derived(sessID)
    //println("sending invites")
    addresses foreach { p =>
      p.send(Invite(sessIDPort, p.protocol, p.role))
    }
    //println("Finished sending invites")
    val replies = for (i <- 1 to addresses.length)
      yield receive(sessIDPort).asInstanceOf[AcceptedInvite]
    val mapping = finalLocationsMapping(replies)
    val privPorts = mapping.values
    for (rp <- replies map (_.replyPort)) {
      //println("send mapping to: "+rp)
      rp.send(mapping)
    }
    sessIDPort.close()
  }

  def finalLocationsMapping(replies: Seq[AcceptedInvite]): Map[Symbol, PrivateAddress] = {
    (replies foldLeft Map[Symbol, PrivateAddress]()) { case (result, AcceptedInvite(role, _, pp)) =>
      result + (role -> pp)
    }
  }

  def checkAddr(ports: Seq[Address]) {
    assert(ports.length > 0)
    var first = ports(0)
    for (p <- ports) assert(first.getClass == p.getClass)
  }

  def forwardInvite(map: (Address, Address)*) {
    for ((from,to) <- map) {
      actor { to.send(from.receive()) }
    }
  }
}

trait Address {
  val protocol: String
  val role: Symbol
  def derived(name: String): Address

  def send(msg: Any)

  def receive(): Any
  def receiveWithin(msec: Int): Any
  
  def close() {}
  
  /** Accept to play a given role. Waits for an invite, creates
   *  a local session channel, and sends back a confirmation message
   *  with the name of the session channel before proceeding.
   */
  def bind[T](act: SessionChannel => T): T
  def bindWithin[T](timeout: Int)(act: SessionChannel => T): T
  
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
