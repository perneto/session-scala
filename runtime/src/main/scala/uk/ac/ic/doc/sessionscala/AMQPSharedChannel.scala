package uk.ac.ic.doc.sessionscala

import messageformats.AMQPMessageFormats
import scala.actors.{Channel => _, _}
import java.io.File

class AMQPSharedChannel(awaiting: Set[Symbol], protocol: String, val brokerHost: String, val port: Int, val user: String, val password: String)
        extends SharedChannel(awaiting)
        with AMQPConnectionComponent
        with AMQPMessageFormats
        with MatchmakerActorComponent
        with AMQPActorProxyComponent
        with CoordinationActorsComponent {

  def join(role: Symbol)(act: ActorFun): Unit = { throw new IllegalStateException("TODO") }

  val INIT_EXCHANGE = "amq.direct"

  def loadProtocolFile(protocol: String): String = {
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

  def invite(mapping: (Symbol,String)*): Unit = {
    def checkMapping() {
      val declaredRoles = Set() ++ mapping map (_._1)
      if (declaredRoles != awaiting)
        throw new IllegalArgumentException("Missing or extra roles in invite. Awaiting: " + awaiting + ", invited: " + declaredRoles)
    }

    def randomInitSessionExchange(): String = {
      val rand = (new java.util.Random).nextInt(10000)
      var sessName = rand.toString
      while (sessName.length < 4) sessName = "0" + sessName
      connectionManagerActor ! ('exchangeDeclare, sessName)
      sessName
    }

    def initSessionExchange(): String = {
      var i = 1; var notDeclared = true; var chan = connect()
      def sessName = "s" + i
      while (notDeclared) {
        try {
          println("initSessionExchange trying exchange name: " + sessName + ", i: " + i)
          chan.exchangeDeclarePassive(sessName) // throws ioe if exchange does not exist. This closes the channel, why oh why?
          // exchange already exists, try another name
          i += 1
        } catch {
          case ioe: java.io.IOException =>
            chan = chan.getConnection.createChannel // amqp stupidness, cf above
            chan.exchangeDeclare(sessName, "direct") // this only runs if the exchange didn't already exists
            notDeclared = false
        }
      }
      close(chan)
      sessName
    }


    checkMapping()

    val sessName = randomInitSessionExchange()
    val scribbleType = loadProtocolFile(protocol)
    inviteImpl(sessName, scribbleType, mapping: _*)
  }

  def inviteImpl(sessName: String, protocol: String, mapping: (Symbol,String)*): Unit = {
    def declareInvitationQueueForHost(host: String) {
      connectionManagerActor ! ('queueDeclare, host)
      connectionManagerActor ! ('queueBind, host, INIT_EXCHANGE, host)
    }

    def declareSessionRoleQueue(sessName: String, role: Symbol) {
      val roleQueue = sessName + role.name
      connectionManagerActor ! ('queueDeclare, roleQueue)
      connectionManagerActor ! ('queueBind, roleQueue, sessName, role.name)
    }

    mapping foreach { case (role, host) =>
      declareInvitationQueueForHost(host)
      declareSessionRoleQueue(sessName, role)
      connectionManagerActor ! ('publish, INIT_EXCHANGE, host, serializeInvite(sessName, role, protocol))
    }
  }

  def forwardInvite(mapping: (Symbol,String)*): Unit = {
    mapping foreach { case (role, host) =>
      println("forwardInvite: " + role + ", awaiting: " + awaiting + ", host: " + host)
      checkRoleAwaiting(role)
      val (sessExchange, protocol) = (matchMakerActor !? Accept(role)).asInstanceOf[(String,String)]
      println("forwarding invite for role: " + role + " on session exchange: " + sessExchange)
      inviteImpl(sessExchange, protocol, role -> host)
    }
  }

  override def close() {
    invitationReceiverActor ! Quit
    matchMakerActor ! Quit
    proxyRegistryActor ! Quit
    connectionManagerActor ! Quit
  }

  def accept(role: Symbol)(act: ActorFun): Unit = {
    println("accept: " + role + ", awaiting: " + awaiting)
    checkRoleAwaiting(role)
    println("sending blocking message")
    val (sessExchange, protocol) = (matchMakerActor !? Accept(role)).asInstanceOf[(String,String)]
    println("got reply from matchmaker")
    // to implement optimized local communication, should receive list of local proxies from matchmaker along with
    // sessExchange. Probably requires changing the API to have all local accepts at once - then we can wait for
    // all invites, and be sure of which roles are local. With current approach, I am sending some messages over
    // amqp at first, then switching to local messages as other local proxies come up 

    val proxy = new AMQPActorProxy(sessExchange, role)
    proxy.start
    val sessChan = (awaiting foldLeft Map[Symbol, ParticipantChannel]()) { case (result, awaitedRole) =>
      if (role == awaitedRole) result // we don't support messages to self through session channel
      else {
        // binds the channel to the current actor (Actor.self). 
        // self is the only actor that can receive on the channel
        val chanFrom = new actors.Channel[Any]() 
        val chanTo = new actors.Channel[Any](proxy)
        proxy ! NewSourceRole(awaitedRole, chanFrom)
        proxy ! NewDestinationRole(awaitedRole, chanTo)
        result + (awaitedRole -> new ParticipantChannel(chanFrom, chanTo))
      }
    }
    act(sessChan)
    println("!!!!!!!!!!!!!!!!accept: call to act finished")
    proxy ! Quit
  }

}
